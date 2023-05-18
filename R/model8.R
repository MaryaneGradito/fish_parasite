### Script for MODEL 8

#############################
# Import processed data
#############################
dat_E <- read.table("./output/dat_models_E.csv",header=T, sep=",")

#############################
# Model 8: Parasite load and body condition interaction
#############################

# Load.packages
pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 8:

boldness_8 <- bf(z_log_boldness ~ 1 + z_dens + z_bc + z_dens*z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_8 <- bf(z_log_activity ~ 1 + z_dens + z_bc + z_dens*z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
 explore_8 <- bf(z_exploration  ~ 1 + z_dens + z_bc + z_dens*z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()


model_8 <- brms::brm(boldness_8 + activity_8 + explore_8 + set_rescor(TRUE), 
                     data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(all = TRUE), file = "./output/models/model_8", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

model_8 <- add_criterion(model_8, c("loo", "waic"), moment_match = TRUE)

# Look at the MCMC chains.
plot(model_8)

# Look at the model
summary(model_8)
