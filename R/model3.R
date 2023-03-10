### Script for MODEL 3

###This model is part of step 2 of the analysis plan
###Subset the experimental and control fish into two datasets (60 fish and 4 measurements for each C and E group)
### Will allow us to answer:
###Question 3: How does behaviour change with parasite infection?


#############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 3: effect of parasite on experimental group
#############################

# Load.packages
  pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, cmdstanr)

### Subset of dataset into two groups
# Experimental group
  dat_E <-subset(dat, treatment == "E")

# Control group
  dat_C <-subset(dat, treatment == "C")

### Model 3: experimental group

  boldness_3 <- bf(log_boldness ~ 1 + z_parasite_load + tank1 + (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_3 <- bf(log_activity ~ 1 + z_parasite_load + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()
  explore_3 <- bf(exploration ~ 1 + z_parasite_load + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()

  model_3 <- brms::brm(boldness_3 + activity_3 + explore_3 + set_rescor(TRUE), 
                      data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_3", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
  plot(model_3)

# Look at the model
  summary(model_3)

