library(tidyverse)
library(mvtnorm)  # For multivariate normal distributions

# TrueSkill2 Player class
TS2Player <- function(mu = 25, sigma = 25/3, sigma_t = 25/3, beta = 25/6) {
  list(
    mu = mu,           # Skill mean
    sigma = sigma,     # Skill uncertainty
    sigma_t = sigma_t, # Team performance uncertainty
    beta = beta        # Score weight
  )
}

# Helper function to compute Gaussian product
gaussian_product <- function(mu1, sigma1, mu2, sigma2) {
  precision1 <- 1 / sigma1^2
  precision2 <- 1 / sigma2^2
  
  precision <- precision1 + precision2
  mu <- (precision1 * mu1 + precision2 * mu2) / precision
  sigma <- sqrt(1 / precision)
  
  list(mu = mu, sigma = sigma)
}

# Helper function to compute Gaussian quotient
gaussian_quotient <- function(mu1, sigma1, mu2, sigma2) {
  precision1 <- 1 / sigma1^2
  precision2 <- 1 / sigma2^2
  
  precision <- precision1 - precision2
  mu <- (precision1 * mu1 - precision2 * mu2) / precision
  sigma <- sqrt(1 / precision)
  
  list(mu = mu, sigma = sigma)
}

# TrueSkill2 update function
update_skills <- function(players, scores) {
  n_players <- length(players)
  
  # Compute team performances
  t <- vector("list", n_players)
  for (i in 1:n_players) {
    player <- players[[i]]
    t[[i]] <- gaussian_product(player$mu, player$sigma, scores[i], player$beta)
  }
  
  # Compute skill posteriors
  for (i in 1:n_players) {
    player <- players[[i]]
    t_exc_i <- gaussian_product(0, 1e-4, 0, 1e-4)  # Initialize with a very certain zero
    
    for (j in 1:n_players) {
      if (i != j) {
        t_exc_i <- gaussian_product(t_exc_i$mu, t_exc_i$sigma, t[[j]]$mu, t[[j]]$sigma)
      }
    }
    
    y <- gaussian_quotient(t[[i]]$mu, t[[i]]$sigma, t_exc_i$mu, t_exc_i$sigma)
    
    # Update player's skill estimate
    players[[i]]$mu <- y$mu
    players[[i]]$sigma <- y$sigma
  }
  
  players
}

# Predict match outcome
predict_match <- function(player1, player2) {
  mu_diff <- player1$mu - player2$mu
  sigma_diff <- sqrt(player1$sigma^2 + player2$sigma^2 + 
                     player1$sigma_t^2 + player2$sigma_t^2 + 
                     player1$beta^2 + player2$beta^2)
  
  p_win <- pnorm(mu_diff / sigma_diff)
  
  list(
    p_win = p_win,
    p_draw = 0,  # TrueSkill2 doesn't explicitly model draws
    expected_score_diff = mu_diff
  )
}

# Save player states
save_player_states <- function(players, file_path) {
  saveRDS(players, file_path)
}

# Load player states
load_player_states <- function(file_path) {
  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    list()
  }
}
