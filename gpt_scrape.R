###############################################################################
# PACKAGES
###############################################################################
library(tidyverse)
library(rvest)      # modern HTML scraping
library(lubridate)  # date parsing
library(purrr)      # map(), safely(), possibly() 
library(caret)
library(lme4)

###############################################################################
# HELPER FUNCTIONS
###############################################################################

#-------------------------------------------------------------------------------
# fetch_page()
#   - Fetch the main listing page for a given promotion + year + offset
#   - Extract event info: date, event name, event ID, partial URL for the event
#-------------------------------------------------------------------------------
fetch_page = function(year=2023, page=0, promotion='WWE') {
  
  # Replace spaces in promotion with '+', to fit Cagematch search param
  promotion = promotion %>% str_replace_all(' ', '+')
  
  # Construct the listing URL
  base_url = 'https://www.cagematch.net/?id=1&view=tvppv'
  query_url = paste0(base_url,
                     '&search=', promotion,
                     '&Year=', year,
                     '&s=', page)
  
  # Polite sleep to avoid overwhelming the server 
  Sys.sleep(0.05)
  
  # Read HTML with rvest
  doc = tryCatch(
    read_html(query_url),
    error = function(e) NULL
  )
  
  # If we failed to read_html, return a harmless empty result
  if (is.null(doc)) {
    return(list(n=0, df=tibble()))
  }
  
  # Attempt to extract the main table
  tbl_node = doc %>% html_node("table")
  
  if (is.na(tbl_node) || length(tbl_node) == 0) {
    # No table found => return empty
    return(list(n=0, df=tibble()))
  }
  
  # Convert the HTML table to a data frame
  df = tbl_node %>% html_table(fill = TRUE)
  
  # Clean up the columns if possible; typically the table columns might be:
  # "Date" | "Event Name" | "Location" | ...
  # Adjust if you see differences.
  
  # Make sure we have a "Date" or similar
  if (!("Date" %in% names(df))) {
    # Possibly the site changed column order. Try to guess or skip
    return(list(n=0, df=tibble()))
  }
  
  # Convert Date to lubridate
  df = df %>%
    mutate(
      Date = dmy(Date) %>% suppressWarnings()
    ) 
  
  # Extract all event links from the table 
  link_nodes = doc %>% html_nodes("table a")
  hrefs      = link_nodes %>% html_attr("href")
  
  # Typically the event links are of the form '?id=1&nr=XXXX&page=2'
  # We find those that have 'page=2', which usually means an event details page
  event_hrefs = hrefs[grepl("page=2", hrefs)]
  
  # Extract numeric event ID
  event_ids = str_extract(event_hrefs, "(?<=&nr=)\\d+")
  
  # Build a smaller tibble so we can merge event IDs
  hrefs_tibble = tibble(
    event      = event_ids,
    event_url  = event_hrefs
  )
  
  # Attempt to attach the event IDs to the original data frame
  # We do this by row order; if the table and links are consistent, this should match up
  df = df %>%
    bind_cols(hrefs_tibble) %>%
    select(Date, `Event Name`, event, event_url, everything())
  
  # The site uses a "NavigationPart" or some variant to indicate total pages
  # We'll find maximum numeric link in that navigation to gauge how many pages exist
  nav_nodes_text = doc %>% 
    html_nodes(".NavigationPart, .TRowTVShow") %>% # old or new format
    html_nodes("a") %>% 
    html_text() %>% 
    as.numeric() %>% 
    na.omit()
  
  max_num = ifelse(length(nav_nodes_text) == 0, 0, max(nav_nodes_text, na.rm=TRUE))
  
  return(
    list(
      n  = max_num,
      df = df
    )
  )
}

#-------------------------------------------------------------------------------
# get_matches()
#   - Iterates over all pages for a given year + promotion
#   - Returns a combined data frame of events for that year.
#-------------------------------------------------------------------------------
get_matches = function(year=2023, scrape_delay = 0.03, promotion = 'WWE') {
  
  # Fetch the first page
  output = fetch_page(year, page=0, promotion=promotion)
  df = output$df
  
  # Get the number of navigation pages
  navpages = output$n
  
  # If there's only one page or none, we can just return
  if (navpages < 1) {
    return(list(df=df))
  }
  
  # Build a sequence of page offsets (Cagematch uses multiples of 100 for pagination)
  page_offsets = 100*(2:navpages - 1)
  
  for (p in page_offsets) {
    Sys.sleep(scrape_delay)
    out_next = fetch_page(year, page = p, promotion=promotion)
    if (nrow(out_next$df) > 0) {
      df = bind_rows(df, out_next$df)
    }
  }
  
  # Remove duplicates if needed
  df = distinct(df)
  
  return(list(df = df))
}

#-------------------------------------------------------------------------------
# fetch_card()
#   - Given an event ID (numeric string), parse the event's page
#   - Extract match-level data, including:
#       * List of wrestlers
#       * Who won, who lost, or if no decision
#       * Match time, title changes, etc.
#   - Return a list with "df" (aggregated results) and "wrestlers"
#-------------------------------------------------------------------------------
fetch_card = function(event, scrape_delay=0.03) {
  
  # Polite sleep
  Sys.sleep(scrape_delay)
  
  # Construct the event URL
  url = paste0('https://www.cagematch.net/?id=1&nr=', event)
  
  doc = tryCatch(
    read_html(url),
    error = function(e) NULL
  )
  
  if (is.null(doc)) {
    # Return empty if we can't parse
    return(list(df = tibble(), wrestlers = tibble()))
  }
  
  # The individual matches are contained in nodes with classes "Match",
  # and the participant results in "MatchResults". 
  # Let's gather them:
  
  match_nodes = doc %>% html_nodes(".Match")
  result_nodes = doc %>% html_nodes(".MatchResults")
  raw_text = result_nodes %>% html_text(trim = TRUE)
  
  # Match Type is found in the ".MatchType" nodes
  match_type_nodes = doc %>% html_nodes(".MatchType")
  match_types = match_type_nodes %>% html_text(trim = TRUE)
  
  # Initialize storage
  all_wrestlers = tibble()
  df = tibble()
  
  # Iterate over each match result
  for (i in seq_along(result_nodes)) {
    
    # Extract all <a> inside .MatchResults => Wrestler links
    w_links = result_nodes[[i]] %>% html_nodes("a") 
    wrestler_links = w_links %>% html_attr("href") 
    wrestler_names = w_links %>% html_text()
    
    # Attempt to get numeric ID from the href
    wrestler_ids = str_extract(wrestler_links, "(?<=&nr=)\\d+")
    
    # Update all_wrestlers tibble
    w_df = tibble(
      wrestler_names = wrestler_names,
      wrestler_ids   = wrestler_ids,
      wrestler_links = wrestler_links
    )
    all_wrestlers = bind_rows(all_wrestlers, w_df)
    
    # Now parse the raw text to identify outcome
    text_values = raw_text[[i]]
    
    # Remove each wrestler’s name from the text so we can isolate the rest
    for (w in wrestler_names) {
      text_values = str_replace_all(text_values, w, '')
    }
    
    # Attempt to parse match time in MM:SS from the text
    match_time_seconds = text_values %>%
      str_extract_all("\\(\\d{1,2}:\\d{2}\\)") %>%
      unlist() %>%
      str_remove_all("\\(") %>%
      str_remove_all("\\)") %>%
      strsplit(":") %>%
      map_dbl(~ as.numeric(.x[1]) * 60 + as.numeric(.x[2]))
    
    if (length(match_time_seconds) == 0) {
      match_time_seconds = NA_real_
    }
    
    # Check for title holder (c)
    title_holder_pos = str_locate(text_values, "\\(c\\)")[1]
    has_title_holder = !is.na(title_holder_pos)
    # Remove (c) from text
    text_values = str_remove_all(text_values, "\\(c\\)")
    
    # Also remove match times from text 
    text_values = str_remove_all(text_values, "\\(\\d{1,2}:\\d{2}\\)")
    # Remove 'w/' patterns 
    text_values = str_remove_all(text_values, "w/")
    
    # The typical outcome phrase is e.g. "def. or "defeat" 
    # Identify if it's a defeat scenario or something else (no contest / draw?)
    defeat_occurred = grepl("def\\.?|defeat", text_values, ignore.case=TRUE)
    
    # We'll locate the approximate position of the outcome phrase
    # (or fallback to an extreme so winner < outcome_pos)
    outcome_matches = str_locate_all(text_values, "\\bdef\\.?\\b|\\bdefeat\\b|\\bbeat\\b")[[1]]
    outcome_pos = ifelse(nrow(outcome_matches)>0, outcome_matches[1,1], 999999)
    
    # Next, we want to see which wrestlers are before or after this phrase
    # For each name, see if it was *before* the outcome in the original raw text
    raw_text_i = raw_text[[i]]
    w_pos = map_int(wrestler_names, ~ {
      loc = str_locate(raw_text_i, .x)
      if (is.na(loc[1])) 999999 else loc[1]
    })
    
    # If defeat_occurred = TRUE, those who appear before the phrase are winners (1), 
    # those after are losers (-1). If not a defeat, set them as 0 => draws, no contest, etc.
    result_vals = if (defeat_occurred) {
      ifelse(w_pos < outcome_pos, 1, -1)
    } else {
      rep(0, length(w_pos))
    }
    
    # Title change logic: If there was a champion (c) in the text, and that champion
    # was after the outcome, a defeat might have changed the title.
    # This logic might need refinement for multi-person matches.
    title_change = (has_title_holder && (title_holder_pos > outcome_pos) && defeat_occurred)
    
    # Build a row with pivot_wider approach:
    result_df = tibble(
      wrestler_ids = wrestler_ids,
      winner       = result_vals
    ) %>%
      mutate(title_change = title_change) %>%
      pivot_wider(
        names_from  = wrestler_ids,
        values_from = winner,
        names_prefix = 'w'
      )
    
    # Store match_time_seconds if desired
    result_df = result_df %>%
      mutate(match_time_s = ifelse(length(match_time_seconds)>0, match_time_seconds[1], NA_real_))
    
    df = bind_rows(df, result_df)
  }
  
  # Attach match type if # of rows matches # of match types
  # If there's a mismatch in rows vs match types, you might have to handle carefully
  # (some pages combine multiple falls in one row, etc.)
  if (nrow(df) == length(match_types)) {
    df = df %>% mutate(match_type = match_types)
  } else {
    df = df %>% mutate(match_type = NA_character_)
  }
  
  return(list(df = df, wrestlers = all_wrestlers))
}

###############################################################################
# MAIN SCRAPING ROUTINE
###############################################################################

# We'll demonstrate how you'd build up a big "events" table,
# then fetch each event's match card. Adjust the year ranges
# and promotions as suits your project.

# Example: Collect events from various RAW eras
events = tibble()

# Monday Night Raw eras
for (y in c(1995:1996, 2006:2023)) {
  cat("Scraping: ", y, " (Monday Night RAW)\n")
  Sys.sleep(0.03)
  out = get_matches(y, promotion = 'Monday Night RAW')
  events = events %>% bind_rows(out$df)
}

for (y in 1997:2001) {
  cat("Scraping: ", y, " (WWF RAW is WAR)\n")
  Sys.sleep(0.03)
  out = get_matches(y, promotion = 'WWF RAW is WAR')
  events = events %>% bind_rows(out$df)
}

for (y in 2002:2005) {
  cat("Scraping: ", y, " (WWE RAW)\n")
  Sys.sleep(0.03)
  out = get_matches(y, promotion = 'WWE RAW')
  events = events %>% bind_rows(out$df)
}

# SmackDown
for (y in 1999:2023) {
  cat("Scraping: ", y, " (SmackDown)\n")
  Sys.sleep(0.03)
  out = get_matches(y, promotion = 'SmackDown')
  events = events %>% bind_rows(out$df)
}

# Clean up duplicates in events 
events = events %>% distinct()

# Now, fetch each event’s card safely
fetch_card_safe = safely(fetch_card)

df = events %>%
  mutate(
    event_data = map(event, fetch_card_safe)
  ) %>%
  mutate(
    result   = map(event_data, "result"),
    error    = map(event_data, "error")
  ) %>%
  mutate(
    df        = map(result, "df"),
    wrestlers = map(result, "wrestlers")
  ) %>%
  select(-event_data, -result)

# Optionally, save your data locally so you don’t have to scrape again
# readr::write_rds(df, "fullwwe_raw_smackdown.Rds")

###############################################################################
# NEXT STEPS
#   - Combine or unnest the `df` column for each event if you want a single
#     tidy table of match outcomes.
#   - Build your kayfabe ELO calculations on the wide or long format.
###############################################################################
