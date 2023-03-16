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
# Model 5: body condition as a response variable
#############################

# Load.packages
  pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

#pivot data for each trial
#we need parasite load z-transform and body condition z-transform for the next model
  
  dat_trial1<-all_data  %>% 
    select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,z_parasite_load, z_bc_1) %>% 
    filter(trial == 1) %>% 
    pivot_longer("z_bc_1",
  values_to='body_condition') %>% arrange(ID_fish)
  
  dat_trial2<-all_data  %>% 
    select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,z_parasite_load, z_bc_2) %>%
    filter(trial == 2) %>% 
    pivot_longer("z_bc_2",
                 values_to='body_condition') %>% arrange(ID_fish)
  
  dat_trial3<-all_data  %>% 
    select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,z_parasite_load, z_bc_3) %>%
    filter(trial == 3) %>% 
    pivot_longer("z_bc_3",
                 values_to='body_condition') %>% arrange(ID_fish)
  
  dat_trial4<-all_data  %>% 
    select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,z_parasite_load, z_bc_4) %>%
    filter(trial == 4) %>% 
    pivot_longer("z_bc_4",
                 values_to='body_condition') %>% arrange(ID_fish)

#rbind together to get dataset for the model

dat_5 <- rbind(dat_trial1, dat_trial2, dat_trial3, dat_trial4) %>% arrange(ID_fish)
dat_5 <- dat_5 %>% filter(treatment == "C") %>% mutate(z_bc = scale(body_condition))

#############################
# Model 5: control group
#############################

### Model 5: control group 

  boldness_5 <- bf(log_boldness ~ 1 + body_condition + (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_5 <- bf(log_activity ~ 1 + body_condition + (1 | ID_fish) + (1 | cage)) + gaussian()
   explore_5 <- bf(exploration  ~ 1 + body_condition + (1 | ID_fish) + (1 | cage)) + gaussian()

  model_5 <- brms::brm(boldness_5 + activity_5 + explore_5 + set_rescor(TRUE), data = dat_5, iter = 6000, warmup = 2000, chains = 4, cores = 4, save_pars = save_pars(), file = "./output/models/model_5", file_refit = "on_change", control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
  plot(model_5)

# Look at the model
  summary(model_5)