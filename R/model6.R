### Script for MODEL 6

#############################
# Import processed data
#############################
  dat_E <- read.table("./output/dat_models_E.csv",header=T, sep=",")

#############################
# Model 6: body condition and parasite load
#############################

# Load.packages
  pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 6:

  boldness_6 <- bf(z_log_boldness ~ 1 + z_dens + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_6 <- bf(z_log_activity ~ 1 + z_dens + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
  explore_6 <- bf(z_exploration  ~ 1 + z_dens + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

  
  model_6_1 <- brms::brm(boldness_6 + activity_6 + explore_6 + set_rescor(TRUE), 
                       data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./output/models/model_6_1", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_6)

# Look at the model
summary(model_6)
