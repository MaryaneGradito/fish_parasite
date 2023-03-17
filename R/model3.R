### Script for MODEL 3

#############################
# Import processed data
#############################
all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

#############################
# Model 3: random slopes
#############################

# Load.packages
pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, tidyr, cmdstanr)

### Model 3:

boldness_3 <- bf(log_boldness ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_3 <- bf(log_activity ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_3 <- bf(exploration ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()

model3 <- brms::brm(boldness_3 + activity_3 + explore_3 + set_rescor(TRUE), 
                    data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(all = TRUE), file = "./output/models/model3", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

# Compare models  
model3 <- add_criterion(model3 , c("loo", "waic"), moment_match = TRUE)

saveRDS(model3, file = "./output/models/model3.rds")

loo(model3,model2) #model 2 is better but small differences
