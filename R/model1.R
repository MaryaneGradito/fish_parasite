# Script for MODEL 1

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
# Model 1: with tank effect
#############################

  # Load.packages
    pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

  ### Model 1: with tank effect
    #prior <- prior(lkj_corr_cholesky(3), class = "L")

    boldness_1 <- bf(z_log_boldness ~ 1 + treatment + tank1 + (-1 + treatment|q| ID_fish) + (1 | cage),  sigma ~ -1 + treatment) + gaussian()
    activity_1 <- bf(z_log_activity ~ 1 + treatment + tank2 + (-1 + treatment|q| ID_fish) + (1 | cage),  sigma ~ -1 + treatment) + gaussian()
    explore_1 <- bf(z_exploration ~ 1 + treatment + tank2 + (-1 + treatment|q| ID_fish) + (1 | cage), sigma ~ -1 + treatment) + gaussian()

    model1 <- brms::brm(boldness_1 + activity_1 + explore_1 + set_rescor(TRUE), 
                      data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model1", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))
    prior_summary(model1)

    # Compare models  
    model1 <- add_criterion(model1, c("loo", "waic"),  moment_match = TRUE)

    saveRDS(model1, file = "./output/models/model1.rds")
    
  ### Model 1.2: looking at the correlation between traits

    boldness_1.2 <- bf(z_log_boldness ~ 1 + treatment + (1|q| ID_fish) + (1 | cage)) + gaussian()
    activity_1.2 <- bf(z_log_activity ~ 1 + treatment + (1|q| ID_fish) + (1 | cage)) + gaussian()
    explore_1.2 <- bf(z_exploration ~ 1 + treatment + (1|q| ID_fish) + (1 | cage)) + gaussian()

    model1.2 <- brms::brm(boldness_1.2 + activity_1.2 + explore_1.2 + set_rescor(TRUE), 
                        data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(), file = "./output/models/model1.2", file_refit = "on_change", # nolint: line_length_linter.
                        control = list(adapt_delta = 0.98))

  # Compare models  
    model1.2 <- add_criterion(model1.2 , c("loo", "waic"),  moment_match = TRUE)

    saveRDS(model1.2, file = "./output/models/model1.2.rds")

=
    