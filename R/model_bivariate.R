# Bivariate models comparing before and after 

#############################
# Import processed data
#############################
all_data_bold <- read.table("./output/dat_bold.csv",header=T, sep=",")

#############################
# Model : before and after correlation
#############################

# Load.packages
pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, tidyr, cmdstanr)

### Model boldness before and after:

bold_before <- bf(before_bold ~ 1 + (1 | ID_fish) + (1 | cage)) + gaussian()
bold_after <- bf(after_bold ~ 1 + (1 | ID_fish) + (1 | cage)) + gaussian()


model_bold <- brms::brm(bold_before + bold_after + set_rescor(TRUE), 
                    data = all_data_bold, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(all = TRUE), file = "./output/models/model_bold", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))
