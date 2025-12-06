library(tidyverse)

df_lc = read_rds('df_lc.Rds')

df_lc %>%
  mutate(time = date_origin + weeks(floor(time))) %>% 
  left_join(wrestler_names, by=c('wrestler_id')) %>%
  # filter(wrestler_id %in% best_wrestlers$wrestler_id) %>%
  # 2) Plot each wrestler’s skill (mu) over time with transparency
  ggplot(aes(x = time, y = mu, group = wrestler_id, color = wrestler_names)) +
  # A line for each wrestler
  geom_line(alpha = 0.8, show.legend = FALSE) +
  # Optional: if you want to show a band for mu ± sigma, uncomment:
  # geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma, fill = wrestler_id),
  #             alpha = 0.1, color = NA, show.legend = FALSE) +
  labs(
    title = "Weekly Wrestler Skill Estimates Over Time",
    x = "Date",
    y = "Estimated Skill (mu)"
  ) +
  theme_bw()

top_n_wrestlers = df_lc %>% 
  # rename(time_index = time) %>%  
  # filter(time >= max(time)-(52*5)) %>% 
  group_by(wrestler_id) %>%
  summarize(n = n()) %>%
  # slice_max(n, n=64) %>%
  filter(n>=25) %>%
  pull(wrestler_id)

chosen_wrestlers = wrestler_names %>% 
  filter(wrestler_names %in% c('John Cena', 'The Rock', 'Rhea Ripley', 'The Bawk')) %>% 
  pull(wrestler_id)

wrestler_names %>% filter(grepl("Ultimate", wrestler_names))

chosen_wrestlers = wrestler_names %>%
  filter(wrestler_names %in% c('The Ultimate Warrior')) %>%
  pull(wrestler_id)

# Create the ggplot
p = df_lc %>%
  # filter((wrestler_id != 84) | (wrestler_id == 84 & time < 600)) %>% 
  mutate(time = date_origin + weeks(floor(time))) %>% 
  filter(wrestler_id %in% top_n_wrestlers) %>%
  # filter(wrestler_id %in% chosen_wrestlers) %>%
  left_join(wrestler_names, by = c('wrestler_id')) %>% 
  ggplot(aes(x = time, y = mu, group = wrestler_id, color = wrestler_names, text = wrestler_names)) +
  # A line for each wrestler
  geom_line(alpha = 0.4, show.legend = FALSE) +
  geom_hline(yintercept = 0, linewidth=0.2, color='black') + 
  # geom_point(data=mom, aes(x=time, y=mu)) + 
  # Optional: if you want to show a band for mu ± sigma, uncomment:
  # geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma, fill = wrestler_id),
  #             alpha = 0.1, color = NA, show.legend = FALSE) +
  labs(
    title = "Weekly Wrestler Skill Estimates Over Time",
    x = "Date",
    y = "Estimated Skill (mu)"
  ) +
  theme_bw() #+
# scale_x_date(date_breaks='years' , date_labels = '%y', limits=c(date('1996-01-01'), date('2024-12-31')))

# Add interactivity with plotly
interactive_plot = ggplotly(p, tooltip = "text") %>%
  layout(
    title = "Bayesian Kayfabe Estimation of Wrestler Skill",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Bayesian Kayfabe (BK) Estimated Skill"),
    hovermode = "closest"
  ) %>%
  config(
    scrollZoom = TRUE  # Enable zooming with mouse wheel
  )

# Display the interactive plot
interactive_plot

#### Bad Event / Replay Detection ####

df %>% 
  mutate(ultimate_warrior_present = map_lgl(wrestlers, ~(84 %in% .x$wrestler_ids))) %>% 
  filter(ultimate_warrior_present == TRUE) %>% 
  select(Date, `Event Name`, event, year, category, ultimate_warrior_present) %>% 
  View()


#### Current Rankings ####
df_lc %>% 
  filter(time >= max(time) - 52) %>% 
  group_by(wrestler_id) %>% 
  mutate(n = n()) %>% 
  filter(n >= 10) %>% 
  slice_max(time) %>% 
  ungroup() %>% 
  left_join(wrestler_names) %>% 
  drop_na(wrestler_names) %>% 
  arrange(-mu) %>%
  mutate(
    lower = mu - qt(0.975, df = n - 1) * (sigma / sqrt(n)), # 95% CI lower bound
    upper = mu + qt(0.975, df = n - 1) * (sigma / sqrt(n))  # 95% CI upper bound
  ) %>%
  mutate(status = case_when(mu >= 1.5 ~ 'Superstars',
                            mu >= 0 ~ 'Midcarders',
                            mu <= 0 ~ 'Jobbers')) %>% 
  ggplot() + 
  theme_bw() + theme(axis.text.y = element_text(size=6)) + 
  geom_segment(aes(color = mu, x = lower, xend = upper, y = fct_reorder(wrestler_names, mu, .desc = FALSE))) +
  geom_point(aes(color = mu, x = mu, y = fct_reorder(wrestler_names, mu, .desc = FALSE))) +
  labs(
    x = "Estimated Value (with CI)",
    y = "Wrestler Names",
    title = "Wrestler Rankings with Confidence Intervals",
    subtitle = "among wrestlers with 10+ appearances in 2024"
  ) +
  facet_wrap(~status, scales='free_y')


#### Project Notes ####
# Need to identify replays and eliminate them. e.g. event 262367 replays
# UW vs. Hulk, which can't be in the training data.
bad_events = c(262367)

# Need to maintain event id throughout the data loading process. df_lc loses it and it sucks.

# Tag Teams are treated as completely separate from their comprising wrestlers
# Make some distribution of scores plots. Would be cool to load championship data in there.
# 