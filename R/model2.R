### Script for MODEL 2

###This model is part of step 1 of the analysis plan
###Using all data (60 fish and 4 measurements / fish)
###Will allow us to answers these questions:
###Question 1: Do we find evidence of personality (i.e., consistent differences in behaviour among individuals)? Does boldness, exploration and activity form a beavioural syndrome?
###Question 2: Does parasitic infection impact the repeatability of each trait and the strength of behavioural syndromes?

#############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 2: without tank effect
#############################

# Load.packages
  pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, tidyr, cmdstanr)

### Model 2: without tank effect

  boldness_2 <- bf(log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                  sigma ~ -1 + treatment) + gaussian()
  activity_2 <- bf(log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                  sigma ~ -1 + treatment) + gaussian()
  explore_2 <- bf(exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                  sigma ~ -1 + treatment) + gaussian()

  model2 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                      data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model2", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))
  
# Compare models  
  model2 <- add_criterion(model2 , c("loo", "waic"), moment_match = TRUE)

  saveRDS(model2, file = "./output/models/model2.rds")

# Compare model 1 and 2: should we keep tank effect ? 

  model1 <- readRDS(file = "./output/models/model1.rds")
  model2 <- readRDS(file = "./output/models/model2.rds")

  loo(model1, model2)

# Look at the MCMC chains.
  plot(model2)

# Look at the model
summary(model2)

###############
#Model 2.2 - looking at the distribution of E and C

#Slipt treatment into two colums so we can use both in the model
dat<-  all_data  %>%  
        select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration)   %>% 
        mutate(trial = if_else(trial == 3, 1, if_else(trial == 4, 2, trial))) %>% 
        pivot_wider(values_from = c(log_activity, log_boldness, exploration), names_from = treatment)  %>% 
        arrange(ID_fish) %>%  data.frame()
summary(dat)
head(dat)


boldness_C_2.2 <- bf(log_boldness_C ~  1 + (1 |q| ID_fish) + (1 | cage)) + gaussian()
boldness_E_2.2 <- bf(log_boldness_E ~ 1 + (1 |q| ID_fish) + (1 | cage) )  + gaussian()

activity_C_2.2 <- bf(log_activity_C ~ 1 + (1 |q| ID_fish) + (1 | cage) )  + gaussian()
activity_E_2.2 <- bf(log_activity_E ~ 1 + (1 |q| ID_fish) + (1 | cage) )  + gaussian()

exploration_C_2.2 <- bf(exploration_C ~ 1 + (1 |q| ID_fish) + (1 | cage))  + gaussian()
exploration_E_2.2 <- bf(exploration_E ~ 1 + (1 |q| ID_fish) + (1 | cage) )  + gaussian()

model2.2 <- brms::brm(boldness_C_2.2 + boldness_E_2.2 + activity_C_2.2 + activity_E_2.2 + exploration_C_2.2 + exploration_E_2.2, set_rescor(TRUE), 
                    data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./output/models/model2.2", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

