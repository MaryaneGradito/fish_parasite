### Script for MODEL 6

#############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 6: body condition as a response variable
#############################

# Load.packages
  pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, cmdstanr)

### Model 6: body condition as a response variable

body_condition <- bf(z_body_condition ~ 1  + (1 | ID_fish) + (1 | cage)) + gaussian()


model_BC <- brms::brm(boldness_C + activity_C + explore_C, data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_BC", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_BC)

# Look at the model
summary(model_BC)