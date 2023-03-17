### Script for MODEL 9

#############################
# Import processed data
#############################
dat_E <- read.table("./output/dat_models_E.csv",header=T, sep=",")

#############################
# Model 9: interaction between parasite species
#############################

# Load.packages
pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 9: 

boldness_9 <- bf(log_boldness ~ 1 + z_ces + z_bs + z_ces*z_bs + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_9 <- bf(log_activity ~ 1 + z_ces + z_bs  + z_ces*z_bs + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_9 <- bf(exploration  ~ 1 + z_ces + z_bs  + z_ces*z_bs + (1 | ID_fish) + (1 | cage)) + gaussian()



model_9 <- brms::brm(boldness_9 + activity_9 + explore_9 + set_rescor(TRUE), 
                     data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(), file = "./output/models/model_9", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_9)

# Look at the model
summary(model_9)
