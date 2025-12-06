#!/usr/bin/env Rscript
# daily_update.R
# Daily update script to update fullwwe data and the Bayesian skill model.
# This version loads and saves the processed event data in the same format as cagematch_scrape.R’s df.Rds

# ---- Libraries ----
library(tidyverse)
library(lubridate)
library(hash)
source("trueskill2.R")

# ---- Source the Scraper & Processing Code ----
# This loads functions such as fetch_cagematch_events_range(), fetch_card_safe(), and convert_integers_to_characters()
source("cagematch_helpers.R")

# ---- Helper Functions Specific to the Updater ----
process_new_events <- function(raw_events) {
  # Process raw event listings into the same structure as the original df
  events <- raw_events %>%
    mutate(
      category = if_else(
        str_detect(`Event Name`, regex("RAW|SmackDown|NXT", ignore_case = TRUE)),
        "Broadcast", "PPV")
    ) %>%
    mutate(event_data = map(event, fetch_card_safe)) %>%
    mutate(
      result        = map(event_data, ~ .x$result),
      error         = map(event_data, ~ .x$error),
      df            = map(event_data, ~ .x$result$df),
      wrestlers     = map(event_data, ~ .x$result$wrestlers),
      teams_summary = map(event_data, ~ .x$result$teams_summary)
    ) %>%
    select(Date, `Event Name`, event, event_url, year, category,
           event_data, result, error, df, wrestlers, teams_summary)
  return(events)
}

# Prepare model data for TrueSkill2.
# new_model_df is expected to be the nested tibble produced in Step 5.
prepare_model_data <- function(model_df, date_origin = NULL) {
  if (!"Date" %in% colnames(model_df)) {
    stop("Column 'Date' is missing in model_df")
  }
  if (is.null(date_origin)) {
    date_origin <- min(model_df$Date, na.rm = TRUE)
  }
  
  model_df <- model_df %>%
    mutate(week_index = as.numeric(interval(date_origin, Date) / weeks(1)))
  
  list(
    composition = map(model_df$teams, ~ unlist(.x)),
    results     = map(model_df$result, ~ as.numeric(.x)),
    times       = model_df$week_index
  )
}

# Convert old TrueSkillThroughTime format to TrueSkill2 format
convert_old_to_new <- function(old_file = "history.Rds", new_file = "players.Rds") {
  if (file.exists(old_file) && !file.exists(new_file)) {
    old_history <- readRDS(old_file)
    new_players <- list()
    
    for (player in names(old_history$players)) {
      last_rating <- tail(old_history$players[[player]]$ratings, 1)[[1]]
      new_players[[player]] <- TS2Player(mu = last_rating@mu, sigma = last_rating@sigma)
    }
    
    save_player_states(new_players, new_file)
    message("Converted old TrueSkillThroughTime data to TrueSkill2 format.")
  }
}

# Update the TrueSkill2 model
update_trueskill2_model <- function(new_model_data, players_file = "players.Rds") {
  # Load existing player states or initialize new ones
  players <- load_player_states(players_file)
  
  # Process each event
  for (i in seq_along(new_model_data$composition)) {
    event_players <- new_model_data$composition[[i]]
    event_results <- new_model_data$results[[i]]
    
    # Ensure all players exist in the players list
    for (player in event_players) {
      if (!(player %in% names(players))) {
        players[[player]] <- TS2Player()
      }
    }
    
    # Update skills for players in this event
    event_player_list <- players[event_players]
    updated_players <- update_skills(event_player_list, event_results)
    
    # Update the main players list
    for (j in seq_along(event_players)) {
      players[[event_players[j]]] <- updated_players[[j]]
    }
  }
  
  # Save updated player states
  save_player_states(players, players_file)
  
  return(players)
}

# ---- Main Daily Update Script ----

# Step 1: Load Existing Processed Data (df in the same format as produced by cagematch_scrape.R)
if (file.exists("df.Rds")) {
  df_old <- read_rds("df.Rds")
  message("Loaded existing df data: ", nrow(df_old), " events.")
} else {
  df_old <- tibble()
  message("No existing df data found; starting fresh.")
}

# Step 2: Determine Date Range for New Raw Events
if (nrow(df_old) > 0) {
  last_event_date <- max(df_old$Date, na.rm = TRUE)
} else {
  last_event_date <- as_date("1984-01-01")
}
new_start_date <- last_event_date + days(1)
new_end_date   <- Sys.Date()

if (new_start_date > new_end_date) {
  message("No new events to scrape. Data is up-to-date.")
  new_raw_events <- tibble()
} else {
  message("Scraping new raw events from ", new_start_date, " to ", new_end_date)
  
  # Define show types (same as in cagematch_helpers.R)
  SHOWTYPE_BROADCAST <- "TV-Show%7CPay+Per+View%7CPremium+Live+Event%7COnline+Stream"
  SHOWTYPE_PPVS      <- "Pay+Per+View%7CPremium+Live+Event"
  
  raw_broadcast <- fetch_cagematch_events_range(new_start_date, new_end_date, SHOWTYPE_BROADCAST, verbose = TRUE)
  raw_ppv       <- fetch_cagematch_events_range(new_start_date, new_end_date, SHOWTYPE_PPVS, verbose = TRUE)
  
  new_raw_events <- bind_rows(raw_broadcast, raw_ppv) %>% 
    distinct() %>% 
    arrange(Date)
  
  message("New raw events found: ", nrow(new_raw_events))
}

# Step 3: Process New Raw Events into df Format
if (nrow(new_raw_events) > 0) {
  new_df <- process_new_events(new_raw_events)
  message("Processed new events into df format: ", nrow(new_df), " events.")
} else {
  new_df <- tibble()
  message("No new events processed.")
}

# Step 3b: Update wrestler_names.Rds with any new wrestlers
if (nrow(new_df) > 0) {
  # Read existing wrestler names
  existing_wrestlers <- readRDS("wrestler_names.Rds")
  
  # Extract all wrestlers from new events
  new_wrestlers <- new_df %>%
    select(wrestlers) %>%
    unnest(wrestlers) %>%
    distinct(wrestler_ids, wrestler_names, wrestler_links)
    
  # Combine with existing, keeping only new unique wrestlers
  updated_wrestlers <- bind_rows(existing_wrestlers, new_wrestlers) %>%
    distinct(wrestler_ids, .keep_all = TRUE)
    
  # Save updated wrestler names
  saveRDS(updated_wrestlers, "wrestler_names.Rds")
  message("Updated wrestler_names.Rds with ", 
          nrow(updated_wrestlers) - nrow(existing_wrestlers), 
          " new wrestlers.")
}

# Step 4: Append New Processed Events to Existing Data and Save Updated df
df_updated <- bind_rows(df_old, new_df) %>%
  distinct(event, .keep_all = TRUE) %>%
  arrange(Date)
write_rds(df_updated, "df.Rds")
message("Updated df saved to df.Rds (", nrow(df_updated), " total events).")

# Step 5: Prepare Model Data from Updated df
# Preserve Date and event-related columns during unnesting.
model_data <- df_updated %>%
  select(Date, `Event Name`, event, event_url, teams_summary, result) %>% 
  # Unnest teams_summary while preserving Date, event, and event_url.
  unnest(teams_summary) %>% 
  # Filter out rows missing a team outcome.
  filter(!is.na(team_outcome)) %>%
  filter(is.na(taped_date) | as.numeric(Date - taped_date) <= 90) %>% 
  mutate(result = if_else(team_outcome == 'loser', 0, 1)) %>% 
  mutate(week_index = as.numeric(interval(min(Date, na.rm = TRUE), Date) / weeks(1))) %>% 
  group_by(week_index, event, `Event Name`, match_index, Date, event_url) %>% 
  nest() %>% 
  mutate(result_length = map_int(data, ~ length(.x$result))) %>% 
  filter(result_length > 1) %>% 
  mutate(teams = map(data, "wrestlers_in_team"),
         result = map(data, "result"))

# Step 6: Convert old format to new format (if necessary)
convert_old_to_new("history.Rds", "players.Rds")

# Step 7: Update the TrueSkill2 Model
model_input <- prepare_model_data(model_data, date_origin = min(df_updated$Date, na.rm = TRUE))
updated_players <- update_trueskill2_model(model_input, players_file = "players.Rds")
message("TrueSkill2 model updated and saved to players.Rds.")

# Step 8: Extract and Save Updated Skill Estimates
df_lc <- tibble(
  wrestler_id = names(updated_players),
  mu = map_dbl(updated_players, ~ .x$mu),
  sigma = map_dbl(updated_players, ~ .x$sigma),
  date = Sys.Date()
)

# Add historical data if available
if (file.exists("df_lc.Rds")) {
  old_df_lc <- read_rds("df_lc.Rds")
  df_lc <- bind_rows(old_df_lc, df_lc) %>%
    arrange(wrestler_id, date) %>%
    distinct(wrestler_id, date, .keep_all = TRUE)
}

write_rds(df_lc, "df_lc.Rds")
message("Updated skill estimates saved to df_lc.Rds.")

# Step 9: Generate summary statistics
summary_stats <- df_lc %>%
  group_by(wrestler_id) %>%
  summarize(
    current_mu = last(mu),
    current_sigma = last(sigma),
    max_mu = max(mu),
    min_mu = min(mu),
    avg_mu = mean(mu),
    n_updates = n()
  ) %>%
  arrange(desc(current_mu))

write_csv(summary_stats, "wrestler_summary_stats.csv")
message("Summary statistics saved to wrestler_summary_stats.csv.")

# Step 10: Generate predictions for upcoming matches (if available)
if (file.exists("upcoming_matches.csv")) {
  upcoming_matches <- read_csv("upcoming_matches.csv")
  predictions <- upcoming_matches %>%
    mutate(
      wrestler1_stats = map(wrestler1, ~ updated_players[[.x]]),
      wrestler2_stats = map(wrestler2, ~ updated_players[[.x]]),
      prediction = map2(wrestler1_stats, wrestler2_stats, predict_match)
    ) %>%
    mutate(
      wrestler1_win_prob = map_dbl(prediction, ~ .x$p_win),
      expected_score_diff = map_dbl(prediction, ~ .x$expected_score_diff)
    ) %>%
    select(match_id, wrestler1, wrestler2, wrestler1_win_prob, expected_score_diff)
  
  write_csv(predictions, "match_predictions.csv")
  message("Match predictions saved to match_predictions.csv.")
}

message("Daily update completed successfully.")
