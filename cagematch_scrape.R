library(tidyverse)
library(lubridate)
library(hash)

# Source helper functions
source("cagematch_helpers.R")
source("trueskill2.R")

#### Load Data ####

SHOWTYPE_BROADCAST <- "TV-Show%7CPay+Per+View%7CPremium+Live+Event%7COnline+Stream"
SHOWTYPE_PPVS      <- "Pay+Per+View%7CPremium+Live+Event"

start_year <- 1984
end_year   <- year(Sys.Date())

all_broadcast <- tibble()
all_ppvs      <- tibble()

for (y in start_year:end_year) {
  message("Year: ", y, " — Broadcasted shows")
  df_broadcast <- fetch_cagematch_events(y, showtype = SHOWTYPE_BROADCAST)
  if (nrow(df_broadcast) > 0) {
    df_broadcast <- df_broadcast %>%
      mutate(year = y, category = "Broadcast")
    all_broadcast <- bind_rows(all_broadcast, df_broadcast)
  }
  
  message("Year: ", y, " — PPVs/PLEs")
  df_ppv <- fetch_cagematch_events(y, showtype = SHOWTYPE_PPVS)
  if (nrow(df_ppv) > 0) {
    df_ppv <- df_ppv %>%
      mutate(year = y, category = "PPV")
    all_ppvs <- bind_rows(all_ppvs, df_ppv)
  }
}

# Combine and filter events
events <- bind_rows(all_broadcast, all_ppvs) %>% 
  distinct() %>%
  filter(!grepl('WWE Speed', `Event Name`)) %>%
  group_by(event) %>%
  slice_max(category %in% c("PPV", "PLE"), with_ties = FALSE) %>%
  ungroup() %>%
  arrange(Date)

# Process events
df <- events %>%
  mutate(event_data = map(event, fetch_card_safe)) %>%
  mutate(
    result = map(event_data, "result"),
    error = map(event_data, "error")
  ) %>%
  mutate(
    df = map(result, "df"),
    wrestlers = map(result, "wrestlers"),
    teams_summary = map(result, 'teams_summary')
  )

# Prepare model data
model_data <- df %>%
  select(Date, `Event Name`, event, event_url, teams_summary) %>%
  unnest(teams_summary) %>%
  filter(!is.na(team_outcome)) %>%
  filter(is.na(taped_date) | as.numeric(Date - taped_date) <= 90) %>%
  mutate(result = if_else(team_outcome == 'loser', 0, 1)) %>%
  mutate(week_index = as.numeric(interval(min(Date), Date) / weeks(1))) %>%
  group_by(week_index, event, `Event Name`, match_index) %>%
  nest() %>%
  mutate(result_length = map_int(data, ~.x$result %>% length())) %>%
  filter(result_length > 1) %>%
  mutate(teams = map(data, 'wrestlers_in_team'),
         result = map(data, 'result'))

#### Fit TrueSkill2 Model ####

# Initialize players
players <- hash()

# Process matches
for (i in 1:nrow(model_data)) {
  match <- model_data[i, ]
  match_teams <- match$teams[[1]]
  match_results <- match$result[[1]]
  
  # Ensure all players exist
  for (team in match_teams) {
    for (player in team) {
      if (is.null(players[[player]])) {
        players[[player]] <- TS2Player()
      }
    }
  }
  
  # Update skills
  match_players <- map(match_teams, ~map(.x, ~players[[.x]]))
  updated_players <- update_skills(match_players, match_results)
  
  # Update player states
  for (t in 1:length(match_teams)) {
    for (p in 1:length(match_teams[[t]])) {
      player_id <- match_teams[[t]][p]
      players[[player_id]] <- updated_players[[t]][[p]]
    }
  }
}

# Create df_lc
df_lc <- tibble(
  wrestler_id = names(players),
  mu = map_dbl(players, ~.x$mu),
  sigma = map_dbl(players, ~.x$sigma),
  date = max(model_data$Date)
)

# Create wrestler_names
wrestler_names <- df %>%
  select(wrestlers) %>%
  unnest(wrestlers) %>%
  select(-wrestler_links) %>%
  rename(wrestler_id = wrestler_ids) %>% 
  group_by(wrestler_names, wrestler_id) %>% 
  summarize(n=n()) %>% 
  group_by(wrestler_id) %>% 
  slice_max(n) %>% 
  slice_head(n=1) %>% 
  select(-n)

#### Save output ####

df %>% write_rds('df.Rds')
df_lc %>% write_rds('df_lc.Rds')
wrestler_names %>% write_rds('wrestler_names.Rds')

# Save TrueSkill2 model state
save_player_states(players, "players.Rds")

message("Initial TrueSkill2 model fit completed and saved.")

#### End ####
