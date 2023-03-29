### Script for MODEL 10

#############################
# Import processed data
#############################
new_dat <- read.table("./output/new_data1.csv",header=T, sep=",")

#############################
# Model 10: slopes and change of bs and bc
#############################

# Load.packages
pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 10: 

boldness_10 <- bf(bold_E | se(se_b_E) ~ z_change_bs*z_change_bc) + gaussian()
activity_10 <- bf(act_E | se(se_act_E) ~ z_change_bs*z_change_bc) + gaussian()
 explore_10 <- bf(exp_E | se(se_exp_E) ~ z_change_bs*z_change_bc) + gaussian()



model_10 <- brms::brm(boldness_10 + activity_10 + explore_10 + set_rescor(TRUE), 
                     data = new_dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(all = TRUE), file = "./output/models/model_10", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

model_10 <- add_criterion(model_9, c("loo", "waic"), moment_match = TRUE)

# Look at the MCMC chains.
plot(model_10)

# Look at the model
summary(model_10)

#change becomes negative
#when you are below average change in parasite load, activity going up
#high change in BS, decrease in the animals activity
