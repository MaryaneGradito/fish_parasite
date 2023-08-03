### Script for MODEL 12

#############################
# Import processed data
#############################
#for model 12
dat_bc <- read.table("./output/dat_bc.csv",header=T, sep=",")

#for model 13
dat_parasite <- read.table("./output/dat_models_parasite.csv",header=T, sep=",")

#explore relationship between parasite density and body condition
plot(as.factor(dat_parasite$trial),dat_parasite$body_condition)
plot(as.factor(dat_bc$trial),dat_bc$body_condition)
plot(dat_parasite$dens_ces, dat_parasite$body_condition)
plot(dat_parasite$dens_bs2, dat_parasite$body_condition)
plot(dat_bc$dens_ces, dat_bc$body_condition)

#############################
# Model 12: how body condition changes over time (include all fish)
#############################

# Load.packages
pacman::p_load(lme4, rstan, tidyverse, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

### Model 12:

body_condition <- bf(z_bc ~ 1 + z_ces + z_bs2 + trial + (1 | ID_fish) + (1 | cage)) + gaussian()

model_12.1 <- brms::brm(body_condition, 
                       data = dat_bc, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./output/models/model_12.1", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_12)

#############################
# Model 13: how body condition change for infected individuals
#############################

body_condition1 <- bf(z_bc ~ 1 + z_ces + z_bs_2 + trial + (1 | ID_fish) + (1 | cage)) + gaussian()

model_13 <- brms::brm(body_condition1, 
                      data = dat_parasite, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_13", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_13)

R2_m_13 <- bayes_R2(model_13, re.form = NA) #marginal
R2_c_13 <- bayes_R2(model_13)
 
R2_m_12 <- bayes_R2(model_12.1, re.form = NA) #marginal
R2_c_12 <- bayes_R2(model_12.1)


