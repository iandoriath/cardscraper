# cagematch_helpers.R

library(tidyverse)
library(rvest)
library(lubridate)
library(TrueSkillThroughTime)
library(hash)
library(plotly)

# Convert integers in a (possibly nested) list to characters
convert_integers_to_characters = function(x) {
  if (is.list(x)) {
    lapply(x, convert_integers_to_characters)
  } else if (is.integer(x)) {
    as.character(x)
  } else {
    x
  }
}

# Fetch a single page of events for a given year and show type.
fetch_cagematch_page <- function(year, showtype, page = 1, verbose = FALSE) {
  offset <- (page - 1) * 100
  url <- paste0(
    "https://www.cagematch.net/?id=8&nr=1&page=4&name=&vDay=&vMonth=&vYear=", year,
    "&showtype=", showtype,
    "&location=&arena=&region=&minRating=&maxRating=&minVotes=&maxVotes=&s=", offset
  )
  
  if (verbose) message("Fetching: ", url)
  Sys.sleep(0.03)
  doc <- tryCatch(read_html(url), 
                  error = function(e) {
                    warning("Error reading URL: ", url, "\n", e)
                    return(NULL)
                  })
  if (is.null(doc)) return(tibble())
  
  rows <- doc %>% html_elements("tr[class*='TRow']")
  if (length(rows) == 0) return(tibble())
  
  results <- lapply(rows, function(r) {
    cols <- r %>% html_elements("td")
    if (length(cols) < 5) return(NULL)
    
    td_text <- cols %>% html_text2()
    event_date     <- td_text[2] %>% str_trim()
    event_location <- ifelse(length(td_text) >= 5, td_text[5], NA_character_)
    event_rating   <- ifelse(length(td_text) >= 8, td_text[8], NA_character_)
    event_votes    <- ifelse(length(td_text) >= 9, td_text[9], NA_character_)
    event_name     <- ifelse(length(td_text) >= 4, td_text[4], NA_character_)
    
    link_node   <- cols[4] %>% html_element("a")
    event_url   <- link_node %>% html_attr("href")
    
    event_rating <- suppressWarnings(as.numeric(event_rating))
    event_votes  <- suppressWarnings(as.integer(event_votes))
    
    event = str_extract(event_url, "(?<=nr=)[^&]+")
    
    tibble(
      Date         = dmy(event_date),
      `Event Name` = event_name,
      event        = event,
      event_url    = event_url
    )
  })
  
  bind_rows(results)
}

# Iterate through pages to fetch all events for a given year and show type.
fetch_cagematch_events <- function(year, showtype, max_pages = 20, verbose = FALSE) {
  all_results <- tibble()
  
  for (p in seq_len(max_pages)) {
    df_page <- fetch_cagematch_page(year, showtype, page = p, verbose = verbose)
    if (nrow(df_page) == 0) break
    all_results <- bind_rows(all_results, df_page)
  }
  
  all_results
}

# New function: Fetch events within a specific date range.
# This function loops over each year from the start to the end date,
# uses fetch_cagematch_events() for that year, and then filters the results.
fetch_cagematch_events_range <- function(start_date, end_date, showtype, max_pages = 20, verbose = FALSE) {
  start_year <- year(start_date)
  end_year   <- year(end_date)
  
  all_events <- tibble()
  
  for (y in seq(start_year, end_year)) {
    events_year <- fetch_cagematch_events(y, showtype, max_pages = max_pages, verbose = verbose)
    if (nrow(events_year) > 0) {
      events_year <- events_year %>%
        filter(Date >= start_date & Date <= end_date) %>%
        mutate(year = y)
      all_events <- bind_rows(all_events, events_year)
    }
  }
  
  all_events
}

# Fetch detailed card data for a given event.
fetch_card <- function(event, scrape_delay = 0.03) {
  library(tidyverse)
  library(rvest)
  library(lubridate)
  
  # --- Part 1: Fetch and parse match results ---
  Sys.sleep(scrape_delay)
  base_url1 <- paste0("https://www.cagematch.net/?id=1&nr=", event)
  message("Reading results from: ", base_url1)
  doc1 <- read_html(base_url1)
  
  match_nodes_doc1 <- doc1 %>% html_elements(".Match")
  all_wrestlers <- tibble()
  results_list  <- list()
  taped_dates   <- vector("list", length(match_nodes_doc1))
  
  for (i in seq_along(match_nodes_doc1)) {
    this_match_node <- match_nodes_doc1[[i]]
    results_node <- this_match_node %>% html_element(".MatchResults")
    if (is.null(results_node)) next
    
    raw_text <- results_node %>% html_text2()
    
    note_node <- this_match_node %>% html_element(".MatchNotes")
    taped_date <- NA_Date_
    if (!is.null(note_node)) {
      note_text <- note_node %>% html_text2()
      taped_date_str <- str_extract(note_text, "(?i)(?<=taped on )\\d{2}\\.\\d{2}\\.\\d{4}")
      if (length(taped_date_str) == 0) taped_date_str <- NA_character_
      if (!is.na(taped_date_str)) {
        maybe_date <- dmy(taped_date_str)
        if (!is.na(maybe_date)) taped_date <- maybe_date
      }
    }
    taped_dates[[i]] <- taped_date
    
    links_for_this_match  <- results_node %>% html_elements("a") %>% html_attr("href")
    names_for_this_match  <- results_node %>% html_elements("a") %>% html_text2()
    ids_for_this_match <- str_extract(links_for_this_match, "(?<=&nr=)\\d+")
    
    tmp_wrestlers <- tibble(
      wrestler_names = names_for_this_match,
      wrestler_ids   = ids_for_this_match,
      wrestler_links = links_for_this_match
    )
    all_wrestlers <- bind_rows(all_wrestlers, tmp_wrestlers)
    
    defeat_occurred <- grepl("defeats", raw_text, ignore.case = TRUE)
    outcome_str_pos <- if (defeat_occurred) {
      str_locate(raw_text, "(?i)defeats")[1]
    } else NA_integer_
    
    wrestler_pos <- purrr::map_int(names_for_this_match, function(nm) {
      loc <- str_locate(raw_text, fixed(nm))
      if (all(is.na(loc))) NA_integer_ else loc[1]
    })
    
    result_values <- if (defeat_occurred) {
      ifelse(wrestler_pos < outcome_str_pos, 1, -1)
    } else rep(0, length(wrestler_pos))
    
    df_wide <- tibble(
      wrestler_ids = ids_for_this_match,
      result       = result_values
    ) %>%
      pivot_wider(
        names_from  = wrestler_ids,
        values_from = result,
        names_prefix = "w"
      )
    
    results_list[[i]] <- df_wide
  }
  
  results_wide <- bind_rows(results_list)
  num_actual_matches <- nrow(results_wide)
  taped_vec <- rep(NA_Date_, num_actual_matches)
  fill_i <- 1
  for (i in seq_along(results_list)) {
    if (!is.null(results_list[[i]])) {
      taped_vec[fill_i] <- taped_dates[[i]]
      fill_i <- fill_i + 1
    }
  }
  results_wide$taped_date <- taped_vec
  
  # --- Part 2: Fetch and parse the match card ---
  Sys.sleep(scrape_delay)
  base_url2 <- paste0("https://www.cagematch.net/?id=1&nr=", event, "&page=2")
  message("Reading card from: ", base_url2)
  doc2 <- read_html(base_url2)
  
  match_cards_nodes <- doc2 %>% html_elements(".Match")
  all_teams <- tibble()
  
  for (i in seq_along(match_cards_nodes)) {
    card_text_for_this_match <- match_cards_nodes[[i]] %>% html_text2()
    card_text_for_this_match <- gsub(
      "(?i)^.*?match\\s*",
      "",
      card_text_for_this_match,
      perl = TRUE
    )
    
    links_for_this_match <- match_cards_nodes[[i]] %>%
      html_elements("a") %>%
      html_attr("href")
    names_for_this_match <- match_cards_nodes[[i]] %>%
      html_elements("a") %>%
      html_text2()
    ids_for_this_match <- str_extract(links_for_this_match, "(?<=&nr=)\\d+")
    
    teams_split <- strsplit(
      card_text_for_this_match,
      "(?i)vs\\.",
      perl = TRUE
    )[[1]] %>% trimws()
    
    for (t_i in seq_along(teams_split)) {
      chunk_text <- teams_split[t_i]
      in_this_team <- vapply(
        names_for_this_match,
        function(nm) grepl(nm, chunk_text, fixed = TRUE),
        logical(1)
      )
      these_ids <- ids_for_this_match[in_this_team]
      
      if (length(these_ids) > 0) {
        all_teams <- bind_rows(
          all_teams,
          tibble(
            match_index   = i,
            team_number   = t_i,
            raw_team_text = chunk_text,
            wrestler_ids_team = paste(these_ids, collapse = ",")
          )
        )
      }
    }
  }
  
  results_long <- results_wide %>%
    mutate(match_index = row_number()) %>%
    select(match_index, taped_date, starts_with("w")) %>%
    pivot_longer(
      cols     = -c(match_index, taped_date),
      names_to = "wrestler_col",
      values_to = "result"
    ) %>%
    mutate(
      wrestler_id = as.integer(str_remove(wrestler_col, "^w")),
      outcome = case_when(
        result ==  1 ~ "winner",
        result == -1 ~ "loser",
        result ==  0 ~ "tie",
        TRUE         ~ NA_character_
      )
    ) %>%
    filter(!is.na(outcome))
  
  all_teams_long <- all_teams %>%
    separate_rows(wrestler_ids_team, sep = ",", convert = TRUE) %>%
    rename(wrestler_id = wrestler_ids_team) %>%
    left_join(results_long, by = c("match_index", "wrestler_id"))
  
  all_teams_long <- all_teams_long %>%
    group_by(match_index, team_number) %>%
    mutate(
      team_outcome = case_when(
        any(outcome == "winner") ~ "winner",
        any(outcome == "loser")  ~ "loser",
        any(outcome == "tie")    ~ "tie",
        TRUE                     ~ NA_character_
      )
    ) %>%
    ungroup()
  
  all_teams_summary <- all_teams_long %>%
    group_by(match_index, team_number, raw_team_text, team_outcome, taped_date) %>%
    summarise(
      wrestlers_in_team = list(wrestler_id),
      .groups = "drop"
    )
  
  df <- results_wide %>%
    mutate(match_index = row_number()) %>%
    pivot_longer(
      cols = -c(match_index, taped_date),
      names_to = "wrestler_col",
      values_to = "result"
    ) %>%
    mutate(
      wrestler_id = as.integer(str_remove(wrestler_col, "^w")),
      outcome = case_when(
        result ==  1 ~ "winner",
        result == -1 ~ "loser",
        result ==  0 ~ "tie",
        TRUE         ~ NA_character_
      )
    ) %>%
    filter(!is.na(outcome)) %>%
    group_by(match_index, outcome) %>%
    mutate(outcome_index = row_number()) %>%
    ungroup() %>%
    pivot_wider(
      id_cols     = c(match_index, taped_date),
      names_from  = c(outcome, outcome_index),
      values_from = wrestler_id,
      values_fill = NA
    ) %>%
    rename_with(~ str_replace(.x, "^winner_(\\d+)$", "w\\1_id")) %>%
    rename_with(~ str_replace(.x, "^loser_(\\d+)$",  "l\\1_id")) %>%
    rename_with(~ str_replace(.x, "^tie_(\\d+)$",    "t\\1_id")) %>%
    select(
      match_index, taped_date,
      matches("^w\\d+_id$"),
      matches("^l\\d+_id$"),
      matches("^t\\d+_id$")
    )
  
  list(
    df            = df,
    wrestlers     = all_wrestlers,
    teams_long    = all_teams_long,
    teams_summary = all_teams_summary
  )
}

# Create a safe version of fetch_card
fetch_card_safe = safely(fetch_card)