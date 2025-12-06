## analysis.R

# ---- Libraries ----
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(TrueSkillThroughTime)
library(hash)

# ---- Data Loading and Preprocessing ----

# Load and clean the full WWE data
load_fullwwe <- function(file_path = "fullwwe.Rds") {
  read_rds(file_path) %>%
    mutate(
      df = map(
        df, 
        possibly(~ .x %>% mutate(across(starts_with("w"), as.double)), otherwise = NULL)
      )
    ) %>%
    filter(!map_lgl(df, is.null))
}

fullwwe <- load_fullwwe("fullwwe.Rds")

# Assuming fullwwe_fixed is equivalent to fullwwe
df <- fullwwe %>% 
  select(-wrestlers) %>% 
  unnest(df) %>% 
  mutate(
    match_id = row_number(),
    day_index = as.integer(Date - min(Date))
  )

# Diagnostic: Summarize counts (winners, losers, missing)
df_summary <- df %>%
  mutate(
    count_1    = rowSums(across(starts_with("w")) == 1, na.rm = TRUE),
    count_neg1 = rowSums(across(starts_with("w")) == -1, na.rm = TRUE),
    count_na   = rowSums(is.na(across(starts_with("w"))))
  ) %>%
  select(-starts_with("w")) %>% 
  summarize(max_winners = max(count_1),
            max_losers  = max(count_neg1),
            max_na      = max(count_na))
# print(df_summary)

# ---- Data Transformation Functions ----

# Transform match-level data into a wide format suitable for modeling.
transform_match_data <- function(df) {
  df %>%
    # 1) Create a unique row identifier.
    mutate(row_id = row_number()) %>%
    # 2) Retain only the match ID and columns starting with "w"
    select(row_id, starts_with("w")) %>%
    # 3) Pivot to long format so that each cell becomes a row.
    pivot_longer(
      cols = -row_id,
      names_to = "wrestler_col",
      values_to = "result"
    ) %>%
    # 4) Extract numeric wrestler IDs and classify outcomes.
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
    # 5) Within each match and outcome group, assign an index.
    group_by(row_id, outcome) %>%
    mutate(outcome_index = row_number()) %>%
    ungroup() %>%
    # 6) Pivot back to wide format so each match gets columns like w1_id, l1_id, etc.
    pivot_wider(
      id_cols     = row_id,
      names_from  = c(outcome, outcome_index),
      values_from = wrestler_id,
      values_fill = NA
    ) %>%
    # 7) Rename columns for clarity.
    rename_with(~ str_replace(.x, "^winner_(\\d+)$", "w\\1_id")) %>%
    rename_with(~ str_replace(.x, "^loser_(\\d+)$",  "l\\1_id")) %>%
    rename_with(~ str_replace(.x, "^tie_(\\d+)$",    "t\\1_id")) %>%
    # 8) Optional: reorder columns.
    select(row_id, matches("^w\\d+_id$"), matches("^l\\d+_id$"), matches("^t\\d+_id$"))
}

df_transformed <- transform_match_data(df)
# print(df_transformed)

# Build a wrestler database to map wrestler IDs to names.
build_wrestler_db <- function(fullwwe) {
  fullwwe %>% 
    select(wrestlers) %>% 
    unnest(wrestlers) %>%
    group_by(wrestler_ids) %>%
    nest() %>% 
    mutate(
      wrestler_names = map_chr(data, ~ {
        .x %>%
          mutate(idx = row_number()) %>%
          group_by(wrestler_names) %>%
          summarize(freq = n(), last_idx = max(idx), .groups = "drop") %>%
          arrange(desc(freq), desc(last_idx)) %>%
          slice(1) %>%
          pull(wrestler_names)
      })
    ) %>% 
    select(-data) %>% 
    mutate(wrestler_ids = as.integer(wrestler_ids))
}

wrestler_db <- build_wrestler_db(fullwwe)
# print(wrestler_db)

# ---- Model Data Preparation ----

# The function below is a placeholder. In practice, you would transform your
# match/event tibble into three lists:
# - composition: a list of teams (each match is a list; each team is a character vector of wrestler IDs)
# - results: a list of match outcomes (e.g. 1 for win, 0 for loss)
# - times: a numeric time index (e.g., weeks since the first event)
prepare_model_data <- function(model_df, date_origin = NULL) {
  if (is.null(date_origin)) {
    date_origin <- min(model_df$Date)
  }
  model_df <- model_df %>%
    mutate(week_index = as.numeric(interval(date_origin, Date) / weeks(1)))
  
  list(
    composition = convert_integers_to_characters(model_df$teams),
    results     = model_df$result,
    times       = model_df$week_index
  )
}

# ---- Model Updating Function ----

# This function will be called to update the existing Bayesian model with new data.
# It loads an existing History object (if available), appends the new data, and runs convergence.
update_bayesian_model <- function(new_model_data, history_file = "history.Rds",
                                  gamma = 0.15, p_draw = 0.2,
                                  eps = 1e-6, iterations = 30) {
  # new_model_data should be a list with elements: composition, results, times
  if (file.exists(history_file)) {
    old_history <- readRDS(history_file)
    # Combine the existing and new data.
    combined_composition <- c(old_history$composition, new_model_data$composition)
    combined_results     <- c(old_history$results, new_model_data$results)
    combined_times       <- c(old_history$times, new_model_data$times)
    
    h <- History(composition = combined_composition,
                 results     = combined_results,
                 times       = combined_times,
                 gamma       = gamma,
                 p_draw      = p_draw)
  } else {
    h <- History(composition = new_model_data$composition,
                 results     = new_model_data$results,
                 times       = new_model_data$times,
                 gamma       = gamma,
                 p_draw      = p_draw)
  }
  
  message("Running model convergence...")
  h$convergence(epsilon = eps, iterations = iterations, verbose = TRUE)
  saveRDS(h, history_file)
  return(h)
}

# ---- Example Usage ----

# Suppose you have a tibble "new_events" with new match data (including Date, teams, and result columns).
# You would first prepare the model input:
# new_model_input <- prepare_model_data(new_events)
#
# Then update the model:
# updated_history <- update_bayesian_model(new_model_input, history_file = "history.Rds")
#
# You can then extract learning curves, predictions, etc. from updated_history:
# lc <- updated_history$learning_curves()
#
# (Your plotting and further analysis code would follow here.)

# ---- End of Script ----
