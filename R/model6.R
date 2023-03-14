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

#pivot data

   dat2<- all_data  %>% 
    pivot_longer(cols=c('z_bc_1','z_bc_2',"z_bc_3","z_bc_4"),
                      values_to='body_condition') 
  
  
### Model 6: body condition as a response variable

  boldness_6 <- bf(log_boldness ~ 1 + z_parasite_load + (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_6 <- bf(log_activity ~ 1 + z_parasite_load + (1 | ID_fish) + (1 | cage)) + gaussian()
  explore_6 <- bf(exploration ~ 1 + z_parasite_load + (1 | ID_fish) + (1 | cage)) + gaussian()
  body_condition <- bf(z_body_condition ~ 1  + (1 | ID_fish) + (1 | cage)) + gaussian()
  
  model_6 <- brms::brm(boldness_6 + activity_6 + explore_6 + set_rescor(TRUE), 
                       data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./output/models/model_6", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_6)

# Look at the model
summary(model_6)