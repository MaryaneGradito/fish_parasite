### Script for MODEL 5

###This model is part of step 2 of the analysis plan
###Subset the experimental and control fish into two datasets (60 fish and 4 measurements for each C and E group)
### Will allow us to answer:
###Question 3: How does behaviour change with parasite infection?


#############################
# Import processed data
#############################
all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 5: control group
#############################

# Load.packages
  pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, cmdstanr)

### Subset of dataset into two groups
# Experimental group
  dat_E <-subset(all_data, treatment == "E")

# Control group
  dat_C <-subset(all_data, treatment == "C")

### Model 5: control group 

  boldness_5 <- bf(log_boldness ~ 1 + (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_5 <- bf(log_activity ~ 1 + (1 | ID_fish) + (1 | cage)) + gaussian()
  explore_5 <- bf(exploration ~ 1 + (1 | ID_fish) + (1 | cage)) + gaussian()

  model_5 <- brms::brm(boldness_5 + activity_5 + explore_5 + set_rescor(TRUE), data = dat_C, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(), file = "./output/models/model_5", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
  plot(model_5)

# Look at the model
  summary(model_5)