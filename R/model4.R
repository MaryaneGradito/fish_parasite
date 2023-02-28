### Script for MODEL 4

###This model is part of step 2 of the analysis plan
###Subset the experimental and control fish into two datasets (60 fish and 4 measurements for each C and E group)
### Will allow us to answer:
###Question 3: How does behaviour change with parasite infection?


#############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 4: effect of parasite on experimental group with parasite load not linear
#############################

# Load.packages
  pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, cmdstanr)

### Subset of dataset into two groups
# Experimental group
  dat_E <-subset(dat, treatment == "E")


# Control group
  dat_C <-subset(dat, treatment == "C")


### Model 4: experimental group

  boldness_4 <- bf(log_boldness ~ 1 + z_parasite_load + z_parasite_load^2 + tank1 + (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_4 <- bf(log_activity ~ 1 + z_parasite_load + z_parasite_load^2 + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()
  explore_4 <- bf(exploration ~ 1 + z_parasite_load + z_parasite_load^2 + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()

  model_4 <- brms::brm(boldness_4 + activity_4 + explore_4 + set_rescor(TRUE), data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_4", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_4)

# Look at the model
summary(model_4)