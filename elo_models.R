# Load the packages
library(nflfastR)
library(dplyr)
library(optimx)

source('elo_inference.R')

# Load chosen dataset
df = load_nfl_year(2022)
# df = load_italian_basketball()

# Create a list of all unique team names
teams = unique(c(df$home_team, df$away_team))
# Assign a unique index to each team
team_indices = setNames(1:length(teams), teams)

# Add team indices to the data frame
df = df %>%
  mutate(home_team_index = team_indices[home_team],
         away_team_index = team_indices[away_team])

#### Model 1 ####

# Initial parameter values
# beta, tau, b vector (tau has closed form so no parameter)
# init_params = res$solution
init_params = c(0.23, rnorm(length(team_indices), mean=1, sd=10))
# init_params = c(0.23, rep(0, length(team_indices)))

# Optimization options

# options = list("algorithm" = "NLOPT_LD_TNEWTON_RESTART", 
# options = list("algorithm" = "NLOPT_LD_LBFGS", 
# options = list("algorithm" = "NLOPT_LN_NELDERMEAD",               
               # "xtol_rel" = 1.0e-3, 
               # "maxeval"=1e5, 
               # "print_level"=1) #, "learning_rate"=1.0e-6)
# lower_bounds = c(-Inf, 0.001, rep(-Inf, length(team_indices)))

#e = 0 # when not using loop

#error_test = tibble()
#for(e in c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)){
# for(e in seq(0.1,10,0.1)){
  #Perform the optimization
  # res = nloptr(x0 = init_params,
  #              eval_f = model_1_neg_log_lik,
  #              # lb = lower_bounds,
  #              eval_grad_f = model_1_gradient,
  #              opts = options,
  #              data = df,
  #              team_indices = team_indices,
  #              error = e)
  # 
  # error_test = error_test %>%
  #   bind_rows(tibble(param=c('beta', names(team_indices)), elo=res$solution) %>%
  #                                         arrange(-elo) %>% mutate(error=e))

# }

# tibble(param=c('beta', names(team_indices)), elo=res$solution) %>%
#   arrange(-elo) %>% 
#   mutate(standard_elo = 1500 + 173*elo) %>%   # 538's ELO formula
#   View()
# 
# error_test %>% 
#   mutate(error = as_factor(error)) %>% 
#   ggplot(aes(x=error, y=elo)) +
#   facet_wrap(~param, scales='free_y') +
#   geom_point()


# Initial parameter values
# beta, tau, b vector (tau has closed form so no parameter)
init_params = c(0.0, rnorm(length(team_indices), mean=0, sd=0.2))

#### Handmade Function ####
descend = function(par, 
                   data,
                   ti,
                   learning_rate = 1.0e-3,
                   tol = 1.0e-3,
                   max_iter = 50,
                   e = 1.0e-3){
  
  output = list()
  
  nll_before = 1
  nll_after <- nll_before - tol - 10
  
  iter = 0
  
  # par = round(par, 2)
  
  training = tibble()
  new_params = par
  best_params = par
  
  best_nll = Inf
  best_iter = 0
  
  while(iter < max_iter){
  #while(nll_after < nll_before - tol){
    
    iter = iter + 1
    print(paste('iteration ', iter))
    
    nll_before = model_1_neg_log_lik(params = par, 
                                     data = data, 
                                     team_indices = ti, 
                                     error = tol)  
    
    grad = model_1_gradient(par, data, ti, tol)
    
    # Approximate second derivatives
    #grad_plus = model_1_gradient(par + e, data, ti, tol)
    #grad_minus = model_1_gradient(par - e, data, ti, tol)
    #second_derivatives = (grad_plus - grad_minus) / e 
    
    # Update parameters
    new_params = par - learning_rate * grad # / second_derivatives
    
    nll_after = model_1_neg_log_lik(params = new_params, 
                                    data = data, 
                                    team_indices = ti, 
                                    error = tol)
    
    print('new params')
    print(new_params)
    # print('grad')
    # print(grad)
    print('nll')
    print(nll_after)
    
    training = training %>% bind_rows(tibble('iter' = iter, 
                                             'params' = list(new_params),
                                      'grad' = list(grad),
                                      'nll' = nll_after))
    
    if(nll_after < best_nll){
      best_nll = nll_after
      best_iter = iter
      best_params = new_params
    }
    
    # if(nll_after < nll_before){
      par = new_params
    # } else {
      # learning_rate = learning_rate * 0.5
    # }
    
    if(iter > max_iter){
      output$message = 'reached max iterations'
      break
    }
  
  }
  
  output$training = training
  output$estimates = new_params
  output$best_nll = best_nll
  output$best_iter = best_iter
  output$best_params = best_params
  
  return(output)
  
}

init_params = c(0.0, rnorm(length(team_indices), mean=0, sd=0.25))
# init_params = params$best_params
params = descend(init_params, 
                 df, 
                 team_indices, 
                 learning_rate = 1.0e-4, 
                 tol=1.0e-3, 
                 max_iter = 1000)

# params$training %>% ggplot(aes(x=iter, y=nll)) + geom_line()


tibble(team = names(team_indices), index=team_indices, estimate=params$best_params[2:length(params$best_params)]) %>% 
  arrange(-estimate)

nll(params$best_params)

# current_params = params
current_params = res$solution

current_params$training %>% unnest_wider(params, names_sep = '_') %>% 
  select(-grad) %>% 
  pivot_longer(-iter) %>% 
  ggplot(aes(x=iter, y=value)) + geom_line() +
  facet_wrap(~name, scales='free_y')

tibble(team=names(team_indices), elo=current_params$best_params[2:length(current_params$best_params)]) %>% 
  arrange(-elo)

tibble(std_dev = seq(0.01, 10, 0.01)) %>% 
  mutate(nll = map_dbl(std_dev, ~model_1_neg_log_lik(c(0.23, rnorm(length(team_indices), 0, .x)), df, team_indices = team_indices))) %>% 
  arrange(nll) %>% 
  ggplot(aes(x=std_dev, y=nll)) +
  geom_point()

options = list("method" = "BFGS", "maxit" = 10000, 'trace' = 1)

# Perform the optimization
res = optimx(par = init_params, 
             fn = model_1_neg_log_lik, 
             gr = model_1_gradient, 
             method = options$method, 
             control = list(maxit = options$maxit, trace = options$trace),
             data = df, 
             team_indices = team_indices,
             error = 1.0e-4,
             hessian = FALSE)

# Extract the best solution
bestsol = res[rownames(res) == "best",]

beta = res[1]
b = res[2:(length(team_indices)+1)] %>% as.numeric()

# Get beta, b, and tau
# beta = bestsol[1]
# b = bestsol[2:length(bestsol)] %>% as.numeric()
tau = as.numeric(t(b) %*% b) / length(team_indices)
games = tibble(team=names(team_indices), b) %>% arrange(-b)

games %>% select(team, b)

beta = res$solution[1]
b = res$solution[2:length(res$solution)]
tau_sim = res$solution[2]
tau = as.numeric(t(b) %*% b) / length(team_indices)
games = tibble(team=names(team_indices), b) %>% arrange(-b)

res
tau
games %>% select(team, b)

#### Model 2 ####
# unique timepoints
num_timepoints = length(unique(df$t))

# Initial parameter values
#             beta,  tau,  rho, b vector
init_params = c(0, 1.0e-12, 0.5, rep(0, num_timepoints * length(team_indices)))

# Optimization options
options = list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1.0e-12)

# Perform the optimization
res = nloptr(x0 = init_params, 
             eval_f = model_2_log_lik, 
             eval_grad_f = model_2_gradient,
             opts = options, 
             data = df, 
             team_indices = team_indices)

beta = res$solution[1]
b = res$solution[2:length(res$solution)]
tau = as.numeric(t(b) %*% b / length(team_indices))

games = tibble(team=names(team_indices), b) %>% arrange(-b)

res$message
beta
tau


games %>% select(team, b) %>% 
  mutate(scaled_b = as.numeric(scale(b))) %>% 
  mutate(ELO = round(1500 + 200 * scaled_b, 0)) %>%
  group_by(ELO) %>% 
  mutate(elo_count = row_number()) %>% 
  ggplot(aes(y=ELO, x='Team', team_abbr=team)) + geom_nfl_logos(height=0.1, width=0.1, )
