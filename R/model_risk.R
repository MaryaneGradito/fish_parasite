# Model that looks at the risk of infection

#############################
# Import processed data
#############################
all_data_bold <- read.table("./output/dat_bold.csv",header=T, sep=",")

#############################
# Model: risk of infection
#############################

# Load.packages
pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, tidyr, cmdstanr)

### Model boldness before and after:
### risk of infection according to boldness behaviours
bold_parasite <- bf(z_dens_tot ~ before_bold + (1 | ID_fish) + (1 | cage)) + gaussian()

model_risk_bold <- brms::brm(bold_parasite, 
                        data = all_data_bold, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(all = TRUE), file = "./output/models/model_risk_bold", file_refit = "on_change",
                        control = list(adapt_delta = 0.98))

### risk of infection according to exploratory behaviours
exp_parasite <- bf(z_dens_tot ~ before_exp + (1 | ID_fish) + (1 | cage)) + gaussian()

model_risk_exp <- brms::brm(exp_parasite, 
                             data = all_data_bold, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                             save_pars = save_pars(all = TRUE), file = "./output/models/model_risk_exp", file_refit = "on_change",
                             control = list(adapt_delta = 0.98))

### risk of infection according to activity
act_parasite <- bf(z_dens_tot ~ before_act + (1 | ID_fish) + (1 | cage)) + gaussian()

model_risk_act <- brms::brm(act_parasite,
                        data = all_data_bold, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(all = TRUE), file = "./output/models/model_risk_act", file_refit = "on_change",
                        control = list(adapt_delta = 0.98))
