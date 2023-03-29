### Script for MODEL 10

#############################
# Import processed data
#############################
dat_parasite <-newdta
#############################
# Model 9: interaction between parasite species
#############################

# Load.packages
pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 9: 

boldness_10 <- bf(slope_bold | se(se_bold) ~ delta_pl + delat_bc) + gaussian()
activity_10 <- bf(slope_activity | se(se_activity) ~ delta_pl + delat_bc) + gaussian()
 explore_10 <- bf(slope_exploration  ~ | se(se_exploration) ~ delta_pl + delat_bc) + gaussian()



model_10 <- brms::brm(boldness_10 + activity_10 + explore_10 + set_rescor(TRUE), 
                     data = data_new, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(all = TRUE), file = "./output/models/model_10", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

model_10 <- add_criterion(model_9, c("loo", "waic"), moment_match = TRUE)

# Look at the MCMC chains.
plot(model_9)

# Look at the model
summary(model_9)
