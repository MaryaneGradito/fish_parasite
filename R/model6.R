### Script for MODEL 6

#############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 6: body condition as a response variable
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

dat_6 <- rbind(dat_trial1, dat_trial2, dat_trial3, dat_trial4) %>% arrange(ID_fish)
dat_6 <- dat6 %>% filter(treatment == "E") %>%  mutate(z_bc = scale(body_condition),
                                                       z_pl = scale(parasite_load))

### Model 6: body condition as a response variable

  boldness_6 <- bf(log_boldness ~ 1 + z_parasite_load + body_condition+ (1 | ID_fish) + (1 | cage)) + gaussian()
  activity_6 <- bf(log_activity ~ 1 + z_parasite_load + body_condition + (1 | ID_fish) + (1 | cage)) + gaussian()
   explore_6 <- bf(exploration  ~ 1 + z_parasite_load + body_condition + (1 | ID_fish) + (1 | cage)) + gaussian()
  body_condition_6 <- bf(body_condition ~ 1  + (1 | ID_fish) + (1 | cage)) + gaussian()
  
  model_6 <- brms::brm(boldness_6 + activity_6 + explore_6 + body_condition_6 + set_rescor(TRUE), 
                       data = dat_6, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./output/models/model_6", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_6)

# Look at the model
summary(model_6)
