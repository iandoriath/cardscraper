library(tidyverse)
library(lubridate)
library(nloptr)
library(nflfastR)

#### Data Loaders ####
load_italian_basketball = function(){
    read_csv('serie_a_22_23_apr15.csv') %>%
      mutate(Season = '22-23') %>%
      rename(home_pts = PTS...3, away_pts = PTS...6, home_team = Team, away_team = Opp) %>%
      filter(is.na(Notes) | Notes != "Postponed") %>%
      select(-...4, -...7, -OT, -Notes) %>%
      mutate(home_result = ifelse(home_pts > away_pts, 1, 0)) %>%
      mutate(home_margin = home_pts - away_pts,
             game_id = row_number()) %>% 
      filter(!is.na(home_result)) %>% 
      mutate(Date = mdy(Date), # Convert date to date format
             t = as.integer(difftime(Date, min(Date), units = "days")) + 1) %>%  # Compute day number
      return()
}

load_nfl_year = function(year=2022){
  # Load data for the 2022 season
  pbp_2022 <- nflfastR::load_pbp(2022)
  
  # Filter for regular season games
  pbp_2022_regular <- pbp_2022 %>% 
    filter(week <= 18)
  
  # Get the last play of each game
  final_plays <- pbp_2022_regular %>%
    group_by(game_id) %>%
    filter(row_number() == n())
  
  # Get the final scores and winner
  df <- final_plays %>%
    mutate(winner = ifelse(total_home_score > total_away_score, home_team, away_team)) %>%
    select(game_id, home_team, away_team, total_home_score, total_away_score, week, game_date)
  
  df = df %>% 
    rename(home_pts = total_home_score,
           away_pts = total_away_score,
           t = week) %>% 
    mutate(home_margin = home_pts - away_pts,
           home_result = as.integer(home_margin > 0))
  
  return(df)
}

#### Model 1 ####
model_1_neg_log_lik = function(params, data, team_indices, error=1e-12){
  beta = params[1]
  b = params[2:length(params)]
  
  # b = round(b, 2)
  
  # b = pmax(b, 0.01)
  # b = sign(b)*pmax(abs(b), 0.01)
  # baddies = which(abs(b) < 0.01)
  # b[baddies] = 0

  btb = as.numeric(t(b) %*% b)
  #btb = 250^2
  
  M = length(team_indices)
  
  # tau = params[2]
  tau = btb / M
  tau = max(tau, error)
  #tau = 200^2
  #tau = max(tau, 1)
   
  p = plogis(beta + b[data$home_team_index] - b[data$away_team_index])
  
  ll = sum(data$home_result * log(p) + (1 - data$home_result) * log(1-p))
  ll = ll - M/2 * log(2*pi*tau)
  ll = ll - btb / (2*tau)
  
  # if(is.na(ll)){
    # print('NA loglik')
    # print(paste('beta: ', beta))
    # print(paste('tau: ', tau))
    # print(paste('b: ', b))
    # print(paste('btb: ', as.numeric(t(b) %*% b)))
    # print(-ll)
  # }
  
  return(-ll)
}

model_1_gradient = function(params, data, team_indices, error=1e-6){
  beta = params[1]
  b = params[2:length(params)]

  # b = pmax(b, 0.01)
  # b = sign(b)*pmax(abs(b), 0.01)
  # b = round(b, 2)
  
  # baddies = which(abs(b) < 0.01)
  # b[baddies] = 0
  
  btb = as.numeric(t(b) %*% b)
  #btb = log(250)
  
  M = length(team_indices)
  p = plogis(beta + b[data$home_team_index] - b[data$away_team_index])
  
  # b = pmax(b, error)
  # tau = params[2]
  # tau = max(as.numeric(t(b) %*% b / M), error)
  tau = btb / M
  tau = max(tau, error)
  #tau = max(tau, 1)
  
  data$p = p
  
  grad = numeric(length(params))
  
  # Gradient with respect to beta
  grad[1] = sum(data$home_result - p)
  
  # Gradient with respect to b_j
  home_games = data %>% group_by(home_team) %>% 
    nest() %>% 
    mutate(home_sum = map_dbl(data, ~sum(.x$home_result - .x$p))) %>% 
    select(-data) %>% 
    rename(team = home_team) #%>% arrange(-home_sum)
    
  away_games = data %>% group_by(away_team) %>% 
    nest() %>% 
    mutate(away_sum = map_dbl(data, ~sum(.x$home_result - .x$p))) %>% 
    select(-data) %>% 
    rename(team = away_team) #%>% arrange(away_sum)
  
  games = full_join(home_games, away_games, by='team')
  
  games$b = b
  
  games = games %>% 
    mutate(factor = b/tau) %>% 
    mutate(grad = home_sum - away_sum - (b/tau)) 
  
  # grad wrt tau
  # grad = c(grad, -M/(2*tau) + as.numeric(t(b) %*% b) / M)

  # glue everything together
  grad[2:length(grad)] = games$grad
  
  # print(-grad)
  
  # print(paste('gradient function: ', -grad))
  
  # Return negative grad since this we are using with a negative log likelihood objective function
  # but the calculations were from the regular log likelihood
  return(-grad)

}

#### Model 2 ####
model_2_log_lik = function(params, data, team_indices){
  beta = params[1]
  tau = params[2]
  rho = params[3]
  b = params[4:length(params)]

  #b = pmax(b, error)
    
  M = length(team_indices)
  p = plogis(beta + b[data$home_team_index] - b[data$away_team_index])
  
  ll = sum(data$home_result * log(p) + (1 - data$home_result) * log(1-p))
  ll = ll - M/2 * log(2*pi*tau)
  ll = ll - (t(b) %*% b)/(2*tau)
  
  return(-ll)
}

# Gradient function
model_2_gradient = function(params, data, team_indices) {
  
  beta = params[1]
  tau = max(1e-12, params[2])
  rho = params[3]
  
  if(rho < 0){
    rho = 0
  } else if (rho > 1) {
    rho = 1
  }
  
  # Extract b_j,t values from params vector
  b = matrix(params[4:length(params)], nrow = length(team_indices), byrow = TRUE)
  
  # Gradient with respect to tau & rho
  home = data %>% group_by(home_team) %>% 
    nest() %>% 
    rename(home_games = data, team = home_team)
  
  away = data %>% group_by(away_team) %>% 
    nest() %>% 
    rename(away_games = data, team = away_team)
  
  games = left_join(home, away, by='team') %>% 
    mutate(combined = map2(home_games, away_games, ~bind_rows(.x, .y))) %>% 
    select(team, combined) %>%
    unnest(combined) %>% 
    mutate(opponent = if_else(is.na(away_team), home_team, away_team)) %>% 
    mutate(team_index = team_indices[team]) %>% 
    arrange(team_index, t) %>% 
    mutate(delta_t = t-lag(t, default=0))
  
  unique_times = games$t %>% unique() %>% sort()
  names(unique_times) = 1:length(unique_times)
  
  games = games %>% left_join(tibble(time_index=names(unique_times), t=unique_times))
  data = data %>% left_join(tibble(time_index=names(unique_times), t=unique_times))
  
  # Compute p_i value
  p = plogis(beta + b[data$team_index] - b[data$away_team_index])
  
  p = games %>% 
    arrange(home_team_index, time_index) %>% 
    mutate(bhome = list(b[home_team_index, as.numeric(time_index)]),
           baway = list(b[away_team_index, as.numeric(time_index)]))

  # Gradient vector
  grad = numeric(length(params))
  
  # Gradient with respect to beta
  grad[1] = sum(data$home_result - p)
  
  delta_t = outer(unique_times, unique_times, FUN=function(x,y) abs(x-y))
  d_sigma_j_d_rho = delta_t * rho^(delta_t-1)
  d_sigma_j_d_tau = rho^(delta_t)
  
  sigma_j = tau * d_sigma_j_d_tau

  sigma_inverse = solve(sigma_j)
  
  tausum = 0
  rhosum = 0
  for(j in 1:ydim){
    bj = b[j,]
    tausum = tausum + sum(diag(sigma_inverse %*% d_sigma_j_d_tau)) - (t(bj) %*% sigma_inverse %*% d_sigma_j_d_tau %*% sigma_inverse %*% bj)
    rhosum = rhosum + sum(diag(sigma_inverse %*% d_sigma_j_d_rho)) - (t(bj) %*% sigma_inverse %*% d_sigma_j_d_rho %*% sigma_inverse %*% bj)
  }
  grad[2] = -tausum/2 # tau
  grad[3] = -rhosum/2 # tau
  
  # Gradient with respect to b_j,t
  for (i in 1:length(data)) {
    h = data$home_team_index[i]
    a = data$away_team_index[i]
    t = data$t[i]
    
    grad[h + 3] = grad[h + 3] + (data$home_result[i] - p[i])
    grad[a + 3] = grad[a + 3] - (data$home_result[i] - p[i])
  }
  
  grad[4:length(params)] = grad[4:length(params)] - (sigma_inverse %*% b[i,])[i]
  
  return(-grad) # Return the negative gradient, since we want to minimize the likelihood
}

