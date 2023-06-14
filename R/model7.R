### Script for MODEL 7

#############################
# Import processed data
#############################
dat_parasite <- read.table("./output/dat_models_parasite.csv",header=T, sep=",")

#############################
# Model 7: Looking at the effects of two parasites
#############################

# Load.packages
pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 7

boldness_7 <- bf(z_log_boldness ~ 1 + z_ces + z_bs_2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_7 <- bf(z_log_activity ~ 1 + z_ces + z_bs_2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_7 <- bf(z_exploration  ~ 1 + z_ces + z_bs_2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()


model_7_2 <- brms::brm(boldness_7 + activity_7 + explore_7 + set_rescor(TRUE), 
                     data = dat_parasite, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(), file = "./output/models/model_7_2", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_7)

# Look at the model
summary(model_7_2)
