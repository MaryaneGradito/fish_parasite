### Project : Double trouble? Parasitic co-infection reduces body condition and activity levels in pumpkinseed sunfish
### Analyse of data and codes


############################
# Load packages
#############################
# import function
source("./func.R")
# load packages
pacman::p_load(tidyverse, brms, glmmTMB, gt, latex2exp, posterior, gt, glue, dplyr,magrittr, ggplot2, cowplot, jpeg, magick,rptR,webshot2,lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

#############################
# Import raw data
#############################
all_data <- read.table("./all_data_T.csv",header=T, sep=";") #with all fish

#############################
# Data processing
#############################
### Create a new column with total cestode (alive + dead)
all_data$P04_tot<-all_data$P04_alive + all_data$P04_dead

### Calculate adjusted fish mass (total fish mass - parasite mass)
### Mean average for adult cestodes: 0.003g, larval form: 0.0008g
### Adjusted mass after caging experiment
all_data$adj_mass3<-(all_data$mass_3-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

### Adjusted mass before sacrifice
all_data$adj_mass4<-(all_data$mass_4-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

### Number of black spot that we want in our models (pre - post number)
all_data$bs_mod<-(all_data$BS_post_tot - all_data$BS_pre)
all_data$bs_mod[all_data$bs_mod<0] <- 0

### Add new column for parasite density in total (co-infection)
tot_parasite<-(all_data$bs_mod + (all_data$P04_alive + all_data$P06))
all_data$dens_tot<-(tot_parasite/all_data$adj_mass4)

### Add new column for BS density only
all_data$dens_bs2<-(all_data$bs_mod/all_data$adj_mass4)

### Add new column for cestode density only
all_data$dens_ces<-((all_data$P04_alive + all_data$P06)/all_data$adj_mass4)

### FULTON INDEX : adjusted mass/standard length3 in CM (*0.1 to change mm to cm)
### Body condition when first arrived at the lab (with normal fish mass)
all_data$fulton1<-(all_data$mass_1/(all_data$SL_1*0.1)^3)

### Body condition before caging experiment (normal fish mass, we assume they don't have any parasite after the anti parasite treatment)
all_data$fulton2<-(all_data$mass_2/(all_data$SL_2*0.1)^3)

### Body condition after caging experiment
all_data$fulton3<-(all_data$adj_mass3/(all_data$SL_3*0.1)^3)

### Body condition before sacrifice
all_data$fulton4<-(all_data$adj_mass4/(all_data$SL_4*0.1)^3)

#############################
# Data distribution and normality
#############################
### Distribution for all fish
hist(all_data$exploration)
hist(all_data$activity)
hist(as.numeric(all_data$boldness))

# Distribution of exploration for group C and E
ggplot(all_data, aes(x = exploration)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "black")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "black"))

# Distribution of activity for group C and E
ggplot(all_data, aes(x = activity)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "black")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800","black"))

# Distribution of boldness for group C and E
ggplot(all_data, aes(x = boldness)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "black")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800","black"))

#NORMALITY AND TRANSFORMATION
# Exploration
qqnorm(all_data$exploration)
qqline(all_data$exploration) #looks normal

shapiro.test(all_data$exploration) #not normal but visually yes

# Activity
qqnorm(log(all_data$activity))
qqline(log(all_data$activity)) #visually normal with log transformation

# Test normality
shapiro.test(log(all_data$activity)) ## normality if we transform with log

# Boldness
qqnorm(log(all_data$boldness))
qqline(log(all_data$boldness))

# Test normality
shapiro.test(log(all_data$boldness)) ## we are visually CLOSE to normality with log

#############################
# Last processing + save the new data set
#############################
# Data transformation to meet normality assumptions
all_data$log_boldness<-scale(log(all_data$boldness))
all_data$log_activity<-scale(log(all_data$activity))

# combine and scale data
dat <- all_data
dat <- dat %>%
  dplyr::mutate(id = factor(ID_fish, levels = unique(ID_fish)),
                z_log_activity = scale(log_activity), z_log_boldness = scale(log_boldness),
                z_exploration = scale(exploration), tank1 = as.factor(bassin_bold), tank2 = as.factor(bassin_exp),(id = ID_fish))

### Save the new dataset with processed data
write.table(dat, file = "all_data_p_T.csv",
            sep = ",", row.names = F)

### Pivot data for each trial
# we need parasite density z-transform and body condition z-transform for the next model
all_dat <- read.table("./all_data_p_T.csv",header=T, sep=",")

dat_trial1<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, exploration, log_boldness, log_activity, z_log_boldness, z_log_activity, z_exploration, fulton1, dens_tot, dens_ces, BS_pre, BS_post_tot, dens_bs2) %>% 
  filter(trial == 1) %>% 
  pivot_longer("fulton1",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial2<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, exploration, log_boldness, log_activity, z_log_boldness, z_log_activity, z_exploration,fulton2, dens_tot, dens_ces, BS_pre, BS_post_tot, dens_bs2) %>%
  filter(trial == 2) %>% 
  pivot_longer("fulton2",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial3<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, exploration, log_boldness, log_activity, z_log_boldness, z_log_activity, z_exploration,fulton3, dens_tot, dens_ces, BS_pre, BS_post_tot, dens_bs2) %>%
  filter(trial == 3) %>% 
  pivot_longer("fulton3",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial4<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, exploration, log_boldness, log_activity, z_log_boldness, z_log_activity, z_exploration, fulton4, dens_tot, dens_ces, BS_pre, BS_post_tot, dens_bs2) %>%
  filter(trial == 4) %>% 
  pivot_longer("fulton4",
               values_to='body_condition') %>% arrange(ID_fish)

### rbind together to get dataset for the model
dat_trials <- rbind(dat_trial1, dat_trial2, dat_trial3, dat_trial4) %>% arrange(ID_fish)

dat_trials <- dat_trials %>%  mutate(z_bc = scale(body_condition),
                                     z_ces = scale(dens_ces),
                                     z_bs2 = scale(dens_bs2),
                                     z_dens = scale(dens_tot))

# Save dataset for model using all fish
write.table(dat_trials, file = "dat_bc.csv",
            sep = ",", row.names = F)

### for model 2 and 3- we don't include control fish as we loose a lot of statistical power
### processed data: same as before but control fish are not included
dat <- dat_trials %>% filter(treatment == "C" | treatment == "E") %>%  mutate(z_bc = scale(body_condition),z_dens = scale(dens_tot),z_ces = scale(dens_ces),z_bs2 = scale(dens_bs2))

write.table(dat, file = "dat.csv",
            sep = ",", row.names = F)

### Select for each group the data and scale 
### Infected group
dat_6 <- dat_trials %>% filter(treatment == "E") %>%  mutate(z_bc = scale(body_condition),
                                                            z_dens = scale(dens_tot),
                                                            z_ces = scale(dens_ces),
                                                            z_bs2 = scale(dens_bs2))

### Uninfected group
dat_5 <- dat_trials %>% filter(treatment == "C") %>%  mutate(z_bc = scale(body_condition))

### Save data (subset)
write.table(dat_6, file = "dat_models_infected.csv",
            sep = ",", row.names = F)

write.table(dat_5, file = "dat_models_uninfected.csv",
            sep = ",", row.names = F)

#############################
# Preliminary models
#############################
### With tank effect
boldness_1 <- bf(z_log_boldness ~ 1 + treatment + tank1 + (-1 + treatment|q| ID_fish) + (1 | cage),  sigma ~ -1 + treatment) + gaussian()
activity_1 <- bf(z_log_activity ~ 1 + treatment + tank2 + (-1 + treatment|q| ID_fish) + (1 | cage),  sigma ~ -1 + treatment) + gaussian()
explore_1 <- bf(z_exploration ~ 1 + treatment + tank2 + (-1 + treatment|q| ID_fish) + (1 | cage), sigma ~ -1 + treatment) + gaussian()

model1 <- brms::brm(boldness_1 + activity_1 + explore_1 + set_rescor(TRUE), 
                    data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./model1", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))
prior_summary(model1)

### Compare models  
model1 <- add_criterion(model1, c("loo", "waic"),  moment_match = TRUE)

saveRDS(model1, file = "./model1.rds")

### Without tank effect
boldness_2 <- bf(z_log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_2 <- bf(z_log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_2 <- bf(z_exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()

model2 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                    data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./model2", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

# Compare models  
model2 <- add_criterion(model2 , c("loo", "waic"), moment_match = TRUE)

saveRDS(model2, file = "./model2.rds")

# Compare model 1 and 2: should we keep tank effect ? 
model1 <- readRDS(file = "./model1.rds")
model2 <- readRDS(file = "./model2.rds")

loo(model1, model2)

### Interactions between variables of interest
#import data
# between both parasites
dat_infected <- read.table("./dat_models_infected.csv",header=T, sep=",")

boldness_int1 <- bf(z_log_boldness ~ 1 + z_ces + z_bs2 + z_ces*z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_int1 <- bf(z_log_activity ~ 1 + z_ces + z_bs2 + z_ces*z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_int1 <- bf(z_exploration  ~ 1 + z_ces + z_bs2 + z_ces*z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_int1 <- brms::brm(boldness_int1 + activity_int1 + explore_int1 + set_rescor(TRUE), 
                     data = dat_infected, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(), file = "./model_int1", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

#between black spot and body condition
boldness_int2 <- bf(z_log_boldness ~ 1 + z_bs2 + z_bs2*z_bc + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_int2 <- bf(z_log_activity ~ 1 + z_bs2 + z_bs2*z_bc + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_int2 <- bf(z_exploration  ~ 1 + z_bs2 + z_bs2*z_bc + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_int2 <- brms::brm(boldness_int2 + activity_int2 + explore_int2 + set_rescor(TRUE), 
                        data = dat_infected, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(), file = "./model_int2", file_refit = "on_change",
                        control = list(adapt_delta = 0.98))

#between cestode and body condition
boldness_int3 <- bf(z_log_boldness ~ 1 + z_ces + z_ces*z_bc + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_int3 <- bf(z_log_activity ~ 1 + z_ces + z_ces*z_bc + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_int3 <- bf(z_exploration  ~ 1 + z_ces + z_ces*z_bc + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_int3 <- brms::brm(boldness_int3 + activity_int3 + explore_int3 + set_rescor(TRUE), 
                        data = dat_infected, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(), file = "./model_int3", file_refit = "on_change",
                        control = list(adapt_delta = 0.98))

### We won't include interactions since they are not significant. Too heavy for our models and dataset. 

#############################
# Final models
#############################
### Model 1: to estimate repeatability
# import processed data
all_data_T <- read.table("./all_data_p_T.csv",header=T, sep=",")

boldness_2 <- bf(z_log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_2 <- bf(z_log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_2 <- bf(z_exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()
model_1 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                      data = all_data_T, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./model_1", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

### Model 2: extract correlations
# import processed data
dat <- read.table("./dat.csv",header=T, sep=",")

boldness_2 <- bf(z_log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_2 <- bf(z_log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_2 <- bf(z_exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()

model_2 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                    data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./model_2", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

### Model 3: random slope
boldness_3 <- bf(z_log_boldness ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_3 <- bf(z_log_activity ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_3 <- bf(z_exploration ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()
model_3 <- brms::brm(boldness_3 + activity_3 + explore_3 + set_rescor(TRUE), 
                    data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(all = TRUE), file = "./model_3", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

### Model 4: how parasite infection impact body condition
# import data
dat_bc <- read.table("./dat_bc.csv",header=T, sep=",")

body_condition <- bf(z_bc ~ 1 + z_ces + z_bs2 + treatment + (1 | ID_fish) + (1 | cage)) + gaussian()
model_4 <- brms::brm(body_condition, 
                        data = dat_bc, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(), file = "./model_4", file_refit = "on_change",
                        control = list(adapt_delta = 0.98))

### Model 5: how body condition influence personality (only uninfected fish)
### import data
dat_uninfected <- read.table("./dat_models_uninfected.csv",header=T, sep=",")

boldness_5 <- bf(z_log_boldness ~ 1 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_5 <- bf(z_log_activity ~ 1 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_5 <- bf(z_exploration  ~ 1 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_5 <- brms::brm(boldness_5 + activity_5 + explore_5 + set_rescor(TRUE), data = dat_uninfected, iter = 6000, warmup = 2000, chains = 4, cores = 4, save_pars = save_pars(), file = "./model_5", file_refit = "on_change", control = list(adapt_delta = 0.98))

### Model 6: how co-infection and body condition influence personality
### import data
boldness_6 <- bf(z_log_boldness ~ 1 + z_ces + z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_6 <- bf(z_log_activity ~ 1 + z_ces + z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_6 <- bf(z_exploration  ~ 1 + z_ces + z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_6 <- brms::brm(boldness_6 + activity_6 + explore_6 + set_rescor(TRUE), 
                       data = dat_infected, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./model_6", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

############################
# Load models
############################
model_1<-readRDS(file = "./model_1.rds") #repeatability
model_2 <- readRDS(file = "./model_2.rds") #correlations
model_3<-readRDS(file = "./model_3.rds") #random slope
model_4<-readRDS(file = "./model_4.rds") #body condition
model_5 <- readRDS(file = "./model_5.rds") # uninfected + bc
model_6 <- readRDS(file = "./model_6.rds") # infected + bc

############################
# Repeatability for each treatment
############################
# Extract posterior distribution from model 1 (repeatability)
post_sd_2 <- as_draws_df(model_1, variable = "^sd", regex = TRUE)
post_sd_C_2 <- post_sd_2[,grepl("C", colnames(post_sd_2))] #ID uninfected
post_sd_E_2 <- post_sd_2[,grepl("E", colnames(post_sd_2))] #ID experimentally infected
post_sd_T_2 <- post_sd_2[,grepl("T", colnames(post_sd_2))] #ID control
post_sd_cage_2 <- post_sd_2[,grepl("cage", colnames(post_sd_2))]
post_sd_sig_2 <- as_draws_df(model_1, variable = "^b_sigma", regex = TRUE)
post_sd_sig_C_2 <- exp(post_sd_sig_2[,grepl("C", colnames(post_sd_sig_2))]) #ID uninfected
post_sd_sig_E_2 <- exp(post_sd_sig_2[,grepl("E", colnames(post_sd_sig_2))]) #ID experimentally infected
post_sd_sig_T_2 <- exp(post_sd_sig_2[,grepl("T", colnames(post_sd_sig_2))]) #ID control

### REPEATABILITY FOR EACH TREATMENT ###
### Repeatability CONTROL GROUP & UNINFECTED ###
# Extract boldness for uninfected group
fish_c_b <- post_sd_C_2[,grep("zlogboldness", colnames(post_sd_C_2))]^2
cage_c_b <- post_sd_cage_2[ ,grep("zlogboldness", colnames(post_sd_cage_2))]^2
res_c_b <- post_sd_sig_C_2[,grep("b_sigma_zlogboldness_treatmentC", colnames(post_sd_sig_C_2))]^2

# confidence intervals
cage_c_b_low <- round(quantile(cage_c_b[,1], c(0.025, 0.975))[1], 3)
cage_c_b_high<- round(quantile(cage_c_b[,1], c(0.025, 0.975))[2], 3)
cage_c_b_mean<-mean(cage_c_b[,1])

fish_c_b_low <- round(quantile(fish_c_b[,1], c(0.025, 0.975))[1], 3)
fish_c_b_high<- round(quantile(fish_c_b[,1], c(0.025, 0.975))[2], 3)
fish_c_b_mean<-mean(fish_c_b[,1])

res_c_b_low <- round(quantile(res_c_b[,1], c(0.025, 0.975))[1], 3)
res_c_b_high <- round(quantile(res_c_b[,1], c(0.025, 0.975))[2], 3)
res_c_b_mean<-mean(res_c_b[,1])

# calculate repeatability
R_C_boldness <-   fish_c_b / (fish_c_b + cage_c_b + res_c_b)

hist(R_C_boldness[,1])
R_C_bold <- mean(R_C_boldness[,1])
R_Blb_bold_C <- round(quantile(R_C_boldness[,1], c(0.025, 0.975))[1], 3)
R_Bub_bold_C <- round(quantile(R_C_boldness[,1], c(0.025, 0.975))[2], 3)

# Extract boldness for control group
fish_t_b <- post_sd_T_2[,grep("zlogboldness", colnames(post_sd_T_2))]^2
cage_t_b <- post_sd_cage_2[ ,grep("zlogboldness", colnames(post_sd_cage_2))]^2
res_t_b <- post_sd_sig_T_2[,grep("b_sigma_zlogboldness_treatmentT", colnames(post_sd_sig_T_2))]^2

# calculate repeatability
R_T_boldness <-   fish_t_b / (fish_t_b + cage_t_b + res_t_b)

hist(R_T_boldness[,1])
R_T_bold <- mean(R_T_boldness[,1])
R_Blb_bold_T <- round(quantile(R_T_boldness[,1], c(0.025, 0.975))[1], 3)
R_Bub_bold_T <- round(quantile(R_T_boldness[,1], c(0.025, 0.975))[2], 3)

# Extract exploration for uninfected group
fish_c_ex <- post_sd_C_2[,grep("zexploration", colnames(post_sd_C_2))]^2
cage_c_ex <- post_sd_cage_2[ ,grep("zexploration", colnames(post_sd_cage_2))]^2
res_c_ex <- post_sd_sig_C_2[,grep("b_sigma_zexploration_treatmentC", colnames(post_sd_sig_C_2))]^2

# confidence intervals
fish_c_ex_low <- round(quantile(fish_c_ex[,1], c(0.025, 0.975))[1], 3)
fish_c_ex_high<- round(quantile(fish_c_ex[,1], c(0.025, 0.975))[2], 3)
fish_c_ex_mean<-mean(fish_c_ex[,1])

res_c_ex_low <- round(quantile(res_c_ex[,1], c(0.025, 0.975))[1], 3)
res_c_ex_high <- round(quantile(res_c_ex[,1], c(0.025, 0.975))[2], 3)
res_c_ex_mean<-mean(res_c_ex[,1])

# calculate repeatability
R_C_exploration <-   fish_c_ex / (fish_c_ex + cage_c_ex + res_c_ex)

hist(R_C_exploration[,1])
R_exp_C_mean<-mean(R_C_exploration[,1])
R_Blb_exp_C <- round(quantile(R_C_exploration[,1], c(0.025, 0.975))[1], 3)
R_Bub_exp_C <- round(quantile(R_C_exploration[,1], c(0.025, 0.975))[2], 3)

# Extract exploration for control group
fish_t_ex <- post_sd_T_2[,grep("zexploration", colnames(post_sd_T_2))]^2
cage_t_ex <- post_sd_cage_2[ ,grep("zexploration", colnames(post_sd_cage_2))]^2
res_t_ex <- post_sd_sig_T_2[,grep("b_sigma_zexploration_treatmentT", colnames(post_sd_sig_T_2))]^2

# calculate repeatability
R_T_exploration <-   fish_t_ex / (fish_t_ex + cage_t_ex + res_t_ex)

hist(R_T_exploration[,1])
R_T_exp <- mean(R_T_exploration[,1])
R_Blb_exp_T <- round(quantile(R_T_exploration[,1], c(0.025, 0.975))[1], 3)
R_Bub_exp_T <- round(quantile(R_T_exploration[,1], c(0.025, 0.975))[2], 3)

# Extract activity for uninfected group
fish_c_ac <- post_sd_C_2[,grep("zlogactivity", colnames(post_sd_C_2))]^2
cage_c_ac <- post_sd_cage_2[ ,grep("zlogactivity", colnames(post_sd_cage_2))]^2
res_c_ac <- post_sd_sig_C_2[,grep("b_sigma_zlogactivity_treatmentC", colnames(post_sd_sig_C_2))]^2

# confidence intervals
fish_c_ac_low <- round(quantile(fish_c_ac[,1], c(0.025, 0.975))[1], 3)
fish_c_ac_high<- round(quantile(fish_c_ac[,1], c(0.025, 0.975))[2], 3)
fish_c_ac_mean<-mean(fish_c_ac[,1])

res_c_ac_low <- round(quantile(res_c_ac[,1], c(0.025, 0.975))[1], 3)
res_c_ac_high <- round(quantile(res_c_ac[,1], c(0.025, 0.975))[2], 3)
res_c_ac_mean<-mean(res_c_ac[,1])

# calculate repeatability
R_C_activity <-   fish_c_ac / (fish_c_ac + cage_c_ac + res_c_ac)

hist(R_C_activity[,1])
R_act_C<- mean(R_C_activity[,1])
R_Blb_act_C <- round(quantile(R_C_activity[,1], c(0.025, 0.975))[1], 3)
R_Bub_act_C <- round(quantile(R_C_activity[,1], c(0.025, 0.975))[2], 3)

# Extract activity for control group
fish_t_act <- post_sd_T_2[,grep("zlogactivity", colnames(post_sd_T_2))]^2
cage_t_act <- post_sd_cage_2[ ,grep("zlogactivity", colnames(post_sd_cage_2))]^2
res_t_act <- post_sd_sig_T_2[,grep("b_sigma_zlogactivity_treatmentT", colnames(post_sd_sig_T_2))]^2

# calculate repeatability
R_T_activity <-   fish_t_act / (fish_t_act + cage_t_act + res_t_act)

hist(R_T_activity[,1])
R_T_act <- mean(R_T_activity[,1])
R_Blb_act_T <- round(quantile(R_T_activity[,1], c(0.025, 0.975))[1], 3)
R_Bub_act_T <- round(quantile(R_T_activity[,1], c(0.025, 0.975))[2], 3)

p_value_act_T<-pmcmc(R_T_activity[,1]*100, null = 10, twotail = FALSE) #not significant

### EXPERIMENTAL GROUP (INFECTED) ###
# Extract boldness for experimental group
fish_e_b <- post_sd_E_2[,grep("zlogboldness", colnames(post_sd_E_2))]^2
cage_e_b <- post_sd_cage_2[ ,grep("zlogboldness", colnames(post_sd_cage_2))]^2
res_e_b <- post_sd_sig_E_2[,grep("b_sigma_zlogboldness_treatmentE", colnames(post_sd_sig_E_2))]^2

# confidence intervals
cage_e_b_low <- round(quantile(cage_e_b[,1], c(0.025, 0.975))[1], 3)
cage_e_b_high<- round(quantile(cage_e_b[,1], c(0.025, 0.975))[2], 3)
cage_e_b_mean<-mean(cage_e_b[,1])

fish_e_b_low <- round(quantile(fish_e_b[,1], c(0.025, 0.975))[1], 3)
fish_e_b_high<- round(quantile(fish_e_b[,1], c(0.025, 0.975))[2], 3)
fish_e_b_mean<-mean(fish_e_b[,1])

res_e_b_low <- round(quantile(res_e_b[,1], c(0.025, 0.975))[1], 3)
res_e_b_high <- round(quantile(res_e_b[,1], c(0.025, 0.975))[2], 3)
res_e_b_mean<-mean(res_e_b[,1])

# calculate repeatability
R_E_boldness <-   fish_e_b / (fish_e_b + cage_e_b + res_e_b)

hist(R_E_boldness[,1])
R_E_bold<-mean(R_E_boldness[,1])
R_Blb_bold_E <- round(quantile(R_E_boldness[,1], c(0.025, 0.975))[1], 3)
R_Bub_bold_E <- round(quantile(R_E_boldness[,1], c(0.025, 0.975))[2], 3)

# Extract exploration for experimental group
fish_e_ex <- post_sd_E_2[,grep("zexploration", colnames(post_sd_E_2))]^2
cage_e_ex <- post_sd_cage_2[ ,grep("zexploration", colnames(post_sd_cage_2))]^2
res_e_ex <- post_sd_sig_E_2[,grep("b_sigma_zexploration_treatmentE", colnames(post_sd_sig_E_2))]^2

# confidence intervals
fish_e_ex_low <- round(quantile(fish_e_ex[,1], c(0.025, 0.975))[1], 3)
fish_e_ex_high<- round(quantile(fish_e_ex[,1], c(0.025, 0.975))[2], 3)
fish_e_ex_mean<-mean(fish_e_ex[,1])

res_e_ex_low <- round(quantile(res_e_ex[,1], c(0.025, 0.975))[1], 3)
res_e_ex_high <- round(quantile(res_e_ex[,1], c(0.025, 0.975))[2], 3)
res_e_ex_mean<-mean(res_e_ex[,1])

# calculate repeatability
R_E_exploration <-   fish_e_ex / (fish_e_ex + cage_e_ex + res_e_ex)

hist(R_E_exploration[,1])
R_exp_E_mean<-mean(R_E_exploration[,1])
R_Blb_exp_E <- round(quantile(R_E_exploration[,1], c(0.025, 0.975))[1], 3)
R_Bub_exp_E <- round(quantile(R_E_exploration[,1], c(0.025, 0.975))[2], 3)

# Extract activity for experimental group
fish_e_ac <- post_sd_E_2[,grep("zlogactivity", colnames(post_sd_E_2))]^2
cage_e_ac <- post_sd_cage_2[ ,grep("zlogactivity", colnames(post_sd_cage_2))]^2
res_e_ac <- post_sd_sig_E_2[,grep("b_sigma_zlogactivity_treatmentE", colnames(post_sd_sig_E_2))]^2

# confidence intervals
fish_e_ac_low <- round(quantile(fish_e_ac[,1], c(0.025, 0.975))[1], 3)
fish_e_ac_high<- round(quantile(fish_e_ac[,1], c(0.025, 0.975))[2], 3)
fish_e_ac_mean<-mean(fish_e_ac[,1])

res_e_ac_low <- round(quantile(res_e_ac[,1], c(0.025, 0.975))[1], 3)
res_e_ac_high <- round(quantile(res_e_ac[,1], c(0.025, 0.975))[2], 3)
res_e_ac_mean<-mean(res_e_ac[,1])

# calculate repeatability
R_E_activity <-   fish_e_ac / (fish_e_ac + cage_e_ac + res_e_ac)

hist(R_E_activity[,1])
R_act_E<-mean(R_E_activity[,1])
R_Blb_act_E <- round(quantile(R_E_activity[,1], c(0.025, 0.975))[1], 3)
R_Bub_act_E <- round(quantile(R_E_activity[,1], c(0.025, 0.975))[2], 3)

############################
# Marginal repeatability
############################
#Boldness
R_bold  <-  overall_repeatability(post_sd_C_2, post_sd_E_2, post_sd_cage_2, post_sd_sig_C_2, post_sd_sig_E_2, trait = "zlogboldness")

R_bold_mean <- mean(R_bold)
R_Blb_bold <- round(quantile(R_bold, c(0.025, 0.975))[1], 3)
R_Bub_bold <- round(quantile(R_bold, c(0.025, 0.975))[2], 3)

#Exploration
R_exp  <-  overall_repeatability(post_sd_C_2, post_sd_E_2, post_sd_cage_2, post_sd_sig_C_2, post_sd_sig_E_2, trait = "zexploration")

R_exp_mean<-mean(R_exp)
R_Blb_exp <- round(quantile(R_exp, c(0.025, 0.975))[1], 3)
R_Bub_exp <- round(quantile(R_exp, c(0.025, 0.975))[2], 3)

#Activity
R_act  <-  overall_repeatability(post_sd_C_2, post_sd_E_2, post_sd_cage_2, post_sd_sig_C_2, post_sd_sig_E_2, trait = "zlogactivity")

R_act_mean<-mean(R_act)
R_Blb_act <- round(quantile(R_act, c(0.025, 0.975))[1], 3)
R_Bub_act <- round(quantile(R_act, c(0.025, 0.975))[2], 3)

### Compare R between treatment group ###
#Boldness
p_value_bold<-pmcmc((R_C_boldness[,1]) - (R_E_boldness[,1])) # Not different
# That makes sense given:
par(mfrow = c(1,2))
hist(R_E_boldness[,1])
hist(R_C_boldness[,1])

#Exploration
p_value_exp<-pmcmc((R_C_exploration[,1]) - (R_E_exploration[,1])) #not different

# Histograms
hist(R_E_exploration[,1])
hist(R_C_exploration[,1])

#Activity
p_value_act<-pmcmc((R_C_activity[,1]) - (R_E_activity[,1])) #not different

# Histograms
hist(R_E_activity[,1])
hist(R_C_activity[,1])

#Is the repeatability different from 0.10?

#Boldness uninfected
p_value_bold_C<-pmcmc(R_C_boldness[,1]*100, null = 10, twotail = FALSE)#not significative
#Boldness infected
p_value_bold_E<-pmcmc(R_E_boldness[,1]*100, null = 10, twotail = FALSE)#not significative
#Average boldness
p_value_bold_a<-pmcmc(R_bold*100, null = 10, twotail = FALSE)#not significative

#Exploration uninfected
p_value_exp_C<-pmcmc(R_C_exploration[,1]*100, null = 10, twotail = FALSE)#significative
#Exploration infected
p_value_exp_E<-pmcmc(R_E_exploration[,1]*100, null = 10, twotail = FALSE)#not significative
#Average exploration
p_value_exp_a<-pmcmc(R_exp*100, null = 10, twotail = FALSE)#not significative

#Activity uninfected
p_value_act_C<-pmcmc(R_C_activity[,1]*100, null = 10, twotail = FALSE)##significative
hist(R_C_activity[,1])
#Activity infected
p_value_act_E<-pmcmc(R_E_activity[,1]*100, null = 10, twotail = FALSE)#significative
#Activity average
p_value_act_a<-pmcmc(R_act*100, null = 10,twotail = FALSE) #significative

#################################
# Behavioural Syndromes
#################################
# Extract correlations from model_2
post_cor  <- as_draws_df(model_2, variable = "^cor", regex= TRUE)
head(post_cor)

# Extract the C and E groups separately
cor_bold_treatC <-   post_cor[,grep("treatmentC__[a-z]*_treatmentC", colnames(post_cor))]
cor_bold_treatE <-   post_cor[,grep("treatmentE__[a-z]*_treatmentE", colnames(post_cor))]

### See if syndromes differ in C and E ###
### Boldness and activity
# Uninfected
bsyn_b_act_C <- data.frame(cor_bold_treatC[,1])
b_act_C_mean<-mean(bsyn_b_act_C[,1])
low_b_act_C<-quantile(bsyn_b_act_C[,1], 0.025)
high_b_act_C<-quantile(bsyn_b_act_C[,1], 0.975)
blb_bul_b_act_C<-quantile(bsyn_b_act_C[,1], c(0.025, 0.975))

# Infected                         
bsyn_b_act_E <- data.frame(cor_bold_treatE[,1])
b_act_E_mean<-mean(bsyn_b_act_E[,1])
blb_bul_b_act_E<-quantile(bsyn_b_act_E[,1], c(0.025, 0.975))
low_b_act_E<-quantile(bsyn_b_act_E[,1], 0.025)
high_b_act_E<-quantile(bsyn_b_act_E[,1], 0.975)

#is the difference significant
pmcmc_b_act<-pmcmc(bsyn_b_act_C[,1] - bsyn_b_act_E[,1], twotail = TRUE)

### Boldness and exploration
# Uninfected
bsyn_b_exp_C <- data.frame(cor_bold_treatC[,2])
b_exp_C_mean<-mean(bsyn_b_exp_C[,1])
blb_bul_b_exp_C<-quantile(bsyn_b_exp_C[,1], c(0.025, 0.975))
low_b_exp_C<-quantile(bsyn_b_exp_C[,1], 0.025)
high_b_exp_C<-quantile(bsyn_b_exp_C[,1], 0.975)

# Infected
bsyn_b_exp_E <- data.frame(cor_bold_treatE[,2])
b_exp_E_mean<-mean(bsyn_b_exp_E[,1])
blb_bul_b_exp_E<-quantile(bsyn_b_exp_E[,1], c(0.025, 0.975))
low_b_exp_E<-quantile(bsyn_b_exp_E[,1], 0.025)
high_b_exp_E<-quantile(bsyn_b_exp_E[,1], 0.975)

#is the difference significant
pmcmc_b_exp<-pmcmc(bsyn_b_exp_C[,1] - bsyn_b_exp_E[,1], twotail = TRUE)

### Activity and exploration
# Uninfected
bsyn_act_exp_C <- data.frame(cor_bold_treatC[,3])
act_exp_C_mean<-mean(bsyn_act_exp_C[,1])
blb_bul_act_exp_C<-quantile(bsyn_act_exp_C[,1], c(0.025, 0.975))
low_act_exp_C<-quantile(bsyn_act_exp_C[,1], 0.025)
high_act_exp_C<-quantile(bsyn_act_exp_C[,1], 0.975)

# Infected
bsyn_act_exp_E <- data.frame(cor_bold_treatE[,3])
act_exp_E_mean<-mean(bsyn_act_exp_E[,1])
blb_bul_act_exp_E<-quantile(bsyn_act_exp_E[,1], c(0.025, 0.975))  
low_act_exp_E<-quantile(bsyn_act_exp_E[,1], 0.025)
high_act_exp_E<-quantile(bsyn_act_exp_E[,1], 0.975)

#is the difference significant
pmcmc_act_exp<-pmcmc(bsyn_act_exp_E[,1] - bsyn_act_exp_C[,1], twotail = TRUE)#yes

### OVERALL SYNDROME ###
# Take average of the two treatments
bsyn_b_act   <- cbind(bsyn_b_act_C[,1], bsyn_b_act_E[,1]); mean(rowMeans(bsyn_b_act)); quantile(rowMeans(bsyn_b_act), c(0.025, 0.975))

# Pool the two MCMC chains from the treatments into a giant vector 32,000 rows (2*16,000)
# Boldness and activity
bsyn_b_act2  <- rbind(c(bsyn_b_act_C[,1], bsyn_b_act_E[,1]))
b_act_o<-mean(bsyn_b_act2)
q_b_act<-quantile(bsyn_b_act2, c(0.025, 0.975))

low_b_act_o<-quantile(bsyn_b_act2, 0.025)
high_b_act_o<-quantile(bsyn_b_act2, 0.975)

# Boldness and exploration
bsyn_b_exp  <- rbind(c(bsyn_b_exp_C[,1], bsyn_b_exp_E[,1]))
b_exp_o<-mean(bsyn_b_exp)
q_b_exp<-quantile(bsyn_b_exp, c(0.025, 0.975))

low_b_exp_o<-quantile(bsyn_b_exp, 0.025)
high_b_exp_o<-quantile(bsyn_b_exp, 0.975)

# Activity and exploration
bsyn_act_exp  <- rbind(c(bsyn_act_exp_C[,1], bsyn_act_exp_E[,1]))
act_exp_o<-mean(bsyn_act_exp)
q_act_exp<-quantile(bsyn_act_exp, c(0.025, 0.975))

low_act_exp_o<-quantile(bsyn_act_exp, 0.025)
high_act_exp_o<-quantile(bsyn_act_exp, 0.975)

############################
# FIGURES 1 : random slope
############################
# Create data frame with posterior distribution from model_3
# Exploration treatment E (infected)
test  <- ranef(model_3, summary = FALSE)$ID_fish

exploration_treatmentE_blup <-  test[, , "zexploration_treatmentE"]
posterior <- as_draws_df(model_3, vars = "^b")
exploration_treatmentE_b <- data.frame(posterior[,grep("b_zexploration_treatmentE", colnames(posterior))])

fishid_slope <- apply(exploration_treatmentE_blup, 2, function(x) rowSums(exploration_treatmentE_b + x))
fishid_slope_est <- apply(fishid_slope,2, function(x) mean(x))
fishid_slope_se <- apply(fishid_slope,2, function(x) sd(x))
fishid_slope <- cbind(fishid_slope_est,fishid_slope_se)

exp_E<-df(exploration_treatmentE_blup,exploration_treatmentE_b) #function works

# Boldness treatment E (infected)
logboldness_treatmentE_blup <-  test[, , "zlogboldness_treatmentE"]
logboldness_treatmentE_b <- data.frame(posterior[,grep("b_zlogboldness_treatmentE", colnames(posterior))])

bold_E<-df(logboldness_treatmentE_blup,logboldness_treatmentE_b)

# Activity treatment E (infected)
logactivity_treatmentE_blup <-  test[, , "zlogactivity_treatmentE"]
logactivity_treatmentE_b <- data.frame(posterior[,grep("b_zlogactivity_treatmentE", colnames(posterior))])

act_E<-df(logactivity_treatmentE_blup,logactivity_treatmentE_b)

# Bold Intercept
logboldness_Intercept_blup <-  test[, , "zlogboldness_Intercept"]
logboldness_Intercept_b <- data.frame(posterior[,grep("b_zlogboldness_Intercept", colnames(posterior))])

bold_I<-df(logboldness_Intercept_blup,logboldness_Intercept_b)

# Exploration Intercept
exploration_Intercept_blup <-  test[, , "zexploration_Intercept"]
exploration_Intercept_b <- data.frame(posterior[,grep("b_zexploration_Intercept", colnames(posterior))])

exp_I<-df(exploration_Intercept_blup,exploration_Intercept_b)

# Activity Intercept
logactivity_Intercept_blup <-  test[, , "zlogactivity_Intercept"]
logactivity_Intercept_b <- data.frame(posterior[,grep("b_zlogactivity_Intercept", colnames(posterior))])

act_I<-df(logactivity_Intercept_blup,logactivity_Intercept_b)

# combine data
data_slopes<-cbind(bold_E,exp_E,act_E,bold_I,exp_I,act_I)
dat_slopes<-as.data.frame(data_slopes)
dat_slopes$ID_fish <- rownames(dat_slopes)

# assigning new names to the columns of the data frame
colnames(dat_slopes) <- c('bold_E','se_b_E','exp_E','se_exp_E','act_E','se_act_E','bold_I','se_b_I','exp_I','se_exp_I','act_I','se_act_I', 'ID_fish')

write.table(dat_slopes, file = "dat_slopes.csv",
            sep = ",", row.names = F)

dat_slopes <- read.table("./dat_slopes.csv",header=T, sep=",")

### Plot dataframe
# Boldness and exploration (infected)
k1<-ggplot(data= dat_slopes)+
  theme_classic()+
  geom_point(aes(x=bold_E,y=exp_E), color ="#440154", size = 3)+
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E, y=exp_E, yend=exp_E+se_exp_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E, y=exp_E, yend=exp_E-se_exp_E), size = 0.2,
               color="#E19BF1"
  )+
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E+se_b_E, y=exp_E, yend=exp_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E-se_b_E, y=exp_E, yend=exp_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_vline(xintercept=0, color = "red2")+
  geom_hline(yintercept=0, color = "red2") +
  labs(y = TeX(paste0("Surface covered (%)", " Slope ", "$\\beta_{id}$")),
       x = TeX(paste0("Emergence time$_{log_{2}}$", " Slope ", "$\\beta_{id}$")),
       tag = "A")+
  annotate("text", x = -0.8, y = 1, label = TeX(paste0("$cor_{id}$ = ", " -0.14, ", " 95%CI = -0.76, 0.59")), size =3.8, color = "black") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# Boldness and activity (infected)
k2<-ggplot(data= dat_slopes)+
  theme_classic()+
  geom_point(aes(x=bold_E,y=act_E), color ="#440154", size = 3)+
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E, y=act_E, yend=act_E+se_act_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E, y=act_E, yend=act_E-se_act_E), size = 0.2,
               color="#E19BF1"
  )+
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E+se_b_E, y=act_E, yend=act_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=bold_E, xend=bold_E-se_b_E, y=act_E, yend=act_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_vline(xintercept=0, color = "red2")+
  geom_hline(yintercept=0, color = "red2") +
  labs(y = TeX(paste0("Distance swam$_{log_{2}}$", " Slope ", "$\\beta_{id}$")),
       x = TeX(paste0("Emergence time$_{log_{2}}$", " Slope ", "$\\beta_{id}$")),
       tag = "B")+
  annotate("text", x = -0.8, y = 1.2, label = TeX(paste0("$cor_{id}$ = ", " -0.31, ", " 95%CI = -0.82, 0.42")), size =3.8, color = "black") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# Activity and exploration (infected)
k3<-ggplot(data= dat_slopes)+
  theme_classic()+
  geom_point(aes(x=exp_E,y=act_E), color ="#440154", size = 3)+
  geom_segment(data =dat_slopes,
               aes(x=exp_E, xend=exp_E, y=act_E, yend=act_E+se_act_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=exp_E, xend=exp_E, y=act_E, yend=act_E-se_act_E), size = 0.2,
               color="#E19BF1"
  )+
  geom_segment(data =dat_slopes,
               aes(x=exp_E, xend=exp_E+se_exp_E, y=act_E, yend=act_E), size = 0.2,
               color="#E19BF1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=exp_E, xend=exp_E-se_exp_E, y=act_E, yend=act_E), size = 0.2,
               color="#E19BF1"
  )+
  geom_vline(xintercept=0, color = "red2")+
  geom_hline(yintercept=0, color = "red2") +
  labs(y = TeX(paste0("Distance swam$_{log_{2}}$", " Slope ", "$\\beta_{id}$")),
       x = TeX(paste0("Surface covered (%)", " Slope ", "$\\beta_{id}$")),
       tag = "C")+
  annotate("text", x = 0.6, y = 1.2, label = TeX(paste0("$cor_{id}$ = ", " 0.25, ", " 95%CI = -0.51, 0.80")), size =3.8, color = "black")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# combine traits
kk<- plot_grid(
  k1, k2, k3, ncol = 3)

# Boldness Intercept vs Bold infected
k4<-ggplot(data= dat_slopes)+
  theme_classic()+
  geom_point(aes(x=bold_I,y=bold_E), color ="#21918c", size = 3)+
  geom_segment(data =dat_slopes,
               aes(x=bold_I, xend=bold_I, y=bold_E, yend=bold_E+se_b_E), size = 0.2, color ="#75C4C1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=bold_I, xend=bold_I, y=bold_E, yend=bold_E-se_b_E), size = 0.2,color ="#75C4C1"
  )+
  geom_segment(data =dat_slopes,
               aes(x=bold_I, xend=bold_I+se_b_I, y=bold_E, yend=bold_E), size = 0.2,color ="#75C4C1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=bold_I, xend=bold_I-se_b_I, y=bold_E, yend=bold_E), size = 0.2,color ="#75C4C1"
  ) +
  geom_vline(xintercept=0, color = "red2")+
  geom_hline(yintercept=0, color = "red2") +
  labs(x = TeX(paste0("Emergence time$_{log_{2}}$", " (personality - intercept)")),
       y = TeX(paste0("Emergence time$_{log_{2}}$", " Slope ", " $\\beta_{id}$")),
       tag = "D")+
  annotate("text", x = 0.5, y = 0.5, label = TeX(paste0("$cor_{id}$ = ", " -0.20, ", " 95%CI = -0.74, 0.54")), size =3.8, color = "black")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

#Exploration intercept  vs exploration infected
k5<-ggplot(data= dat_slopes)+
  theme_classic()+
  geom_point(aes(x=exp_I,y=exp_E), color ="#21918c", size = 3)+
  geom_segment(data =dat_slopes,
               aes(x=exp_I, xend=exp_I, y=exp_E, yend=exp_E+se_exp_E), size = 0.2,
               color ="#75C4C1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=exp_I, xend=exp_I, y=exp_E, yend=exp_E-se_exp_E), size = 0.2,
               color ="#75C4C1"
  )+
  geom_segment(data =dat_slopes,
               aes(x=exp_I, xend=exp_I+se_exp_I, y=exp_E, yend=exp_E), size = 0.2,
               color ="#75C4C1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=exp_I, xend=exp_I-se_exp_I, y=exp_E, yend=exp_E), size = 0.2,
               color ="#75C4C1"
  ) +
  geom_vline(xintercept=0, color = "red2")+
  geom_hline(yintercept=0, color = "red2") +
  labs(x = TeX(paste0("Surface covered (%)", " (personality - intercept)")),
       y = TeX(paste0("Surface covered (%)", " Slope ", " $\\beta_{id}$")),
       tag = "E")+
  annotate("text", x = -0.7, y = 1, label = TeX(paste0("$cor_{id}$ = ", " -0.27, ", " 95%CI = -0.80, 0.51")), size =3.8, color = "black")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# Activity intercept  vs activity infected
k6<-ggplot(data= dat_slopes)+
  theme_classic()+
  geom_point(aes(x=act_I,y=act_E), color ="#21918c", size = 3)+
  geom_segment(data =dat_slopes,
               aes(x=act_I, xend=act_I, y=act_E, yend=act_E+se_act_E), size = 0.2,
               color ="#75C4C1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=act_I, xend=act_I, y=act_E, yend=act_E-se_act_E), size = 0.2,
               color ="#75C4C1"
  )+
  geom_segment(data =dat_slopes,
               aes(x=act_I, xend=act_I+se_act_I, y=act_E, yend=act_E), size = 0.2,
               color ="#75C4C1"
  ) +
  geom_segment(data =dat_slopes,
               aes(x=act_I, xend=act_I-se_act_I, y=act_E, yend=act_E), size = 0.2,
               color ="#75C4C1"
  ) +
  geom_vline(xintercept=0, color = "red2")+
  geom_hline(yintercept=0, color = "red2") +
  labs(x = TeX(paste0("Distance swam$_{log_{2}}$", " (personality - intercept)")),
       y = TeX(paste0("Distance swam$_{log_{2}}$", " Slope ", " $\\beta_{id}$")),
       tag = "F")+
  annotate("text", x = -0.5, y = 1.2, label = TeX(paste0("$cor_{id}$ = ", " -0.49, ", " 95%CI = -0.81, 0.11")), size =3.8, color = "black")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# combine traits
kkk<- plot_grid(
  k4, k5, k6, ncol = 3)

# combine both plot
all<-plot_grid(kk,kkk, ncol = 1)

# save figure
ggsave("./fig1.png", plot = all, width = 30, height = 20, units = "cm")

fig1 <- image_read("./fig1.png")  
fig1

############################
# FIGURES 2
############################
### Extract data from model + raw data
int_w <- as_draws_df(model_1, variable = "^b", regex = TRUE)

bold_int<-int_w[,grep("zlogboldness_Intercept", colnames(int_w))]
exp_int<-int_w[,grep("zexploration_Intercept", colnames(int_w))]
act_int<-int_w[,grep("zlogactivity_Intercept", colnames(int_w))]

bold_T<-int_w[,grep("b_zlogboldness_treatmentT", colnames(int_w))]
exp_T<-int_w[,grep("b_zexploration_treatmentT", colnames(int_w))]
act_T<-int_w[,grep("b_zlogactivity_treatmentT", colnames(int_w))]

bold_E<-int_w[,grep("b_zlogboldness_treatmentE", colnames(int_w))]
exp_E<-int_w[,grep("b_zexploration_treatmentE", colnames(int_w))]
act_E<-int_w[,grep("b_zlogactivity_treatmentE", colnames(int_w))]

### EXPLORATION ###
# EXPLORATION FOR UNINFECTED (INTERCEPT)
# intercept from model
int_exp_c_num<- as.numeric(unlist(exp_int))

# First, get the mean and sd of the un-transformed raw data that was used to scale.
mean_exploration <- mean(all_data_T$exploration)
sd_exploration <- sd(all_data_T$exploration)

# Now, exp_int is the mean of the z-transformed variable. So, we need to use the raw data mean and sd to convert back to raw scale
raw_int_exp_c<-(int_exp_c_num * sd_exploration) + mean_exploration
range(raw_int_exp_c)
raw_exp_c<- as.numeric(unlist(raw_int_exp_c))
raw_exp_c_mean<-mean(raw_exp_c)
 
# EXPLORATION FOR CONTRAST (TREATMENT T)
int_exp_con_T_num<- as.numeric(unlist(exp_T))

# EXPLORATION FOR CONTRAST (TREATMENT E)
int_exp_con_E_num<- as.numeric(unlist(exp_E))

# EXPLORATION FOR E
int_exp_E<-(int_exp_con_E_num + int_exp_c_num)
int_exp_E_num<- as.numeric(unlist(int_exp_E))

# raw units for exploration E
raw_int_exp_E<-(int_exp_E_num * sd_exploration) + mean_exploration
range(raw_int_exp_E)
raw_exp_E<- as.numeric(unlist(raw_int_exp_E))
raw_exp_E_mean<-mean(raw_exp_E)

# EXPLORATION FOR T
int_exp_T<-(int_exp_con_T_num + int_exp_c_num)
int_exp_T_num<- as.numeric(unlist(int_exp_T))

# Contrast Intercept - T
int_exp_E_T<-(int_exp_T_num - int_exp_E_num)
raw_int_exp_E_T<-(int_exp_E_T * sd_exploration) + mean_exploration
range(raw_int_exp_E_T)
raw_exp_E_T<- as.numeric(unlist(raw_int_exp_E_T))
raw_exp_E_T_mean<-mean(raw_int_exp_E_T)
blb_exp_E_T <- round(quantile(raw_int_exp_E_T, c(0.025, 0.975))[1], 3)
bub_exp_E_T <- round(quantile(raw_int_exp_E_T, c(0.025, 0.975))[2], 3)

# raw units
raw_int_exp_T<-(int_exp_T_num * sd_exploration) + mean_exploration
range(raw_int_exp_T)
raw_exp_T<- as.numeric(unlist(raw_int_exp_T))
raw_exp_T_mean<-mean(raw_exp_T)

# pmcmc test 
pm_exp_T<-pmcmc(int_exp_c_num - int_exp_T_num,twotail = TRUE) #significantly different

# Exploration confidence intervals
# Control
blb_exp_T <- round(quantile(raw_exp_T, c(0.025, 0.975))[1], 3)
bub_exp_T <- round(quantile(raw_exp_T, c(0.025, 0.975))[2], 3)
# infected
blb_exp_E <- round(quantile(raw_exp_E, c(0.025, 0.975))[1], 3)
bub_exp_E <- round(quantile(raw_exp_E, c(0.025, 0.975))[2], 3)
# uninfected
blb_exp_c <- round(quantile(raw_exp_c, c(0.025, 0.975))[1], 3)
bub_exp_c <- round(quantile(raw_exp_c, c(0.025, 0.975))[2], 3)

### BOLDNESS ###
#BOLDNESS FOR UNINFECTED (INTERCEPT)
int_bold_c_num<- as.numeric(unlist(bold_int))

# get raw mean and sd from raw data
mean_boldness <- mean(all_data_T$log_boldness)
sd_boldness <- sd(all_data_T$log_boldness)

# raw units
raw_int_b_c<-(int_bold_c_num * sd_boldness) + mean_boldness
range(raw_int_b_c)
raw_bold_c<- as.numeric(unlist(raw_int_b_c))
raw_bold_c_mean<-mean(raw_bold_c)

# BOLDNESS FOR CONTRAST (TREATMENT T) U-T
int_bold_con_T_num<- as.numeric(unlist(bold_T))

# EXPLORATION FOR CONTRAST (TREATMENT E)
int_bold_con_E_num<- as.numeric(unlist(bold_E))

# BOLDNESS FOR E
int_bold_E<-(int_bold_con_E_num + int_bold_c_num)
int_bold_E_num<- as.numeric(unlist(int_bold_E))

# raw units
raw_int_b_E<-(int_bold_E_num * sd_boldness) + mean_boldness
range(raw_int_b_E)
raw_bold_E<- as.numeric(unlist(raw_int_b_E))
raw_bold_E_mean<-mean(raw_bold_E)

# BOLDNESS FOR T
int_bold_T<-(int_bold_con_T_num + int_bold_c_num)
int_bold_T_num<- as.numeric(unlist(int_bold_T))

# Contrast T and E
int_bold_E_T<-(int_bold_T_num - int_bold_E_num)
raw_int_b_E_T<-(int_bold_E_T * sd_boldness) + mean_boldness
range(raw_int_b_E_T)
raw_bold_E_T<- as.numeric(unlist(raw_int_b_E_T))
raw_bold_E_T_mean<-mean(raw_int_b_E_T)
blb_bold_E_T <- round(quantile(raw_int_b_E_T, c(0.025, 0.975))[1], 3)
bub_bold_E_T <- round(quantile(raw_int_b_E_T, c(0.025, 0.975))[2], 3)

# raw units
raw_int_b_T<-(int_bold_T_num * sd_boldness) + mean_boldness
range(raw_int_b_T)
raw_bold_T<- as.numeric(unlist(raw_int_b_T))
raw_bold_T_mean<-mean(raw_bold_T)

# Boldness confidence intervals
# Control
blb_bold_T <- round(quantile(raw_bold_T, c(0.025, 0.975))[1], 3)
bub_bold_T <- round(quantile(raw_bold_T, c(0.025, 0.975))[2], 3)
# infected
blb_bold_E <- round(quantile(raw_bold_E, c(0.025, 0.975))[1], 3)
bub_bold_E <- round(quantile(raw_bold_E, c(0.025, 0.975))[2], 3)
# uninfected
blb_bold_c <- round(quantile(raw_bold_c, c(0.025, 0.975))[1], 3)
bub_bold_c <- round(quantile(raw_bold_c, c(0.025, 0.975))[2], 3)

### ACTIVITY ###
# ACTIVITY FOR UNINFECTED (INTERCEPT)
int_act_c_num<- as.numeric(unlist(act_int))

# mean and sd from raw data
mean_activity <- mean(all_data_T$log_activity)
sd_activity <- sd(all_data_T$log_activity)

# raw units
raw_int_act_c<-(int_act_c_num * sd_activity) + mean_activity
range(raw_int_act_c)
raw_act_c<- as.numeric(unlist(raw_int_act_c))
raw_act_c_mean<-mean(raw_act_c)

# ACTIVITY FOR CONTRAST (TREATMENT T)
int_act_con_T_num<- as.numeric(unlist(act_T))

# ACTIVITY FOR T
int_act_T<-(int_act_con_T_num + int_act_c_num)
int_act_T_num<- as.numeric(unlist(int_act_T))

# raw units
raw_int_act_T<-(int_act_T_num * sd_activity) + mean_activity
range(raw_int_act_T)
raw_act_T<- as.numeric(unlist(raw_int_act_T))
raw_act_T_mean<-mean(raw_act_T)

# ACTIVITY FOR CONTRAST (TREATMENT E)
int_act_con_E_num<- as.numeric(unlist(act_E))

# ACTIVITY FOR E
int_act_E<-(int_act_con_E_num + int_act_c_num)
int_act_E_num<- as.numeric(unlist(int_act_E))

# Contrast T and E
int_act_E_T<-(int_act_T_num - int_act_E_num)
raw_int_act_E_T<-(int_act_E_T * sd_activity) + mean_activity
range(raw_int_act_E_T)
raw_act_E_T<- as.numeric(unlist(raw_int_act_E_T))
raw_act_E_T_mean<-mean(raw_int_act_E_T)
blb_act_E_T <- round(quantile(raw_int_act_E_T, c(0.025, 0.975))[1], 3)
bub_act_E_T <- round(quantile(raw_int_act_E_T, c(0.025, 0.975))[2], 3)

# raw units
raw_int_act_E<-(int_act_E_num * sd_activity) + mean_activity
range(raw_int_act_E)
raw_act_E<- as.numeric(unlist(raw_int_act_E))
raw_act_E_mean<-mean(raw_act_E)

# Activity confidence intervals
# Control
blb_act_T <- round(quantile(raw_act_T, c(0.025, 0.975))[1], 3)
bub_act_T <- round(quantile(raw_act_T, c(0.025, 0.975))[2], 3)
# infected
blb_act_E <- round(quantile(raw_act_E, c(0.025, 0.975))[1], 3)
bub_act_E <- round(quantile(raw_act_E, c(0.025, 0.975))[2], 3)
# uninfected
blb_act_c <- round(quantile(raw_act_c, c(0.025, 0.975))[1], 3)
bub_act_c <- round(quantile(raw_act_c, c(0.025, 0.975))[2], 3)

### Create data frame
dat_model<-data.frame(
  trait=c("logboldness","logboldness","exploration","exploration","logactivity","logactivity","exploration","logboldness","logactivity"),
  treatment=c("Uninfected/Control","Infected","Uninfected/Control","Infected","Uninfected/Control","Infected","Control","Control","Control"), 
  estimate = c(raw_bold_c_mean,raw_bold_E_mean,raw_exp_c_mean,raw_exp_E_mean, raw_act_c_mean, raw_act_E_mean, raw_exp_T_mean,raw_bold_T_mean,raw_act_T_mean), 
  Lower_CI = c(blb_bold_c, blb_bold_E, blb_exp_c, blb_exp_E, blb_act_c, blb_act_E,blb_exp_T,blb_bold_T,blb_act_T),
  Higher_CI = c(bub_bold_c,bub_bold_E,bub_exp_c,bub_exp_E,bub_act_c,bub_act_E,bub_exp_T,bub_bold_T,bub_act_T))

all_data_T[all_data_T == 'C'] <- 'Uninfected/Control'
all_data_T[all_data_T == 'E'] <- 'Infected'
all_data_T[all_data_T == 'T'] <- 'Control'

### FIGURE 3 ###
all_data_T$treatment <- factor(all_data_T$treatment, levels=c("Uninfected/Control","Infected", "Control"))
trt <- as.factor(all_data_T$treatment)

dat1<-dat_model %>% 
  filter(trait == "logboldness")

# boldness
m1<-
  ggplot(all_data_T, aes(x = trt, y = log_boldness, group = trt, color = trt, fill = trt)) +
  geom_violin(aes(),trim = FALSE) +
  geom_jitter(aes(colour = trt),shape=16, position=position_jitter(0.1))+
  #scale_color_manual(labels = c("Uninfected/Control","Infected","Control"), values = c("#9A6FA5", "#6FA5A4","#FAFA8B"))
  scale_color_manual(labels = c("Uninfected/Control","Infected","Control"), values = c("#8A05A9", "#26ABA5","#FEE502"))+
  scale_fill_manual(values = c("#D4A5DF", "#91E5E1","#FFFBD6")) +
  geom_errorbar(data = dat1,aes(x = treatment,y = estimate, ymin= Lower_CI, ymax=Higher_CI), width=0.25, linewidth =1, color = "black", inherit.aes = FALSE)+
  geom_point(data=dat1, mapping=aes(x= treatment, y= estimate), size=3, shape=21, fill = "white", inherit.aes = FALSE)+
  labs(x = ("Treatment"), 
       y = TeX("Emergence time$_{log_{2}}$(sec)")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  geom_segment(aes(x = 1, xend = 2, y = 3.5, yend = 3.5), color = "black") +
  annotate("text",  x = 1.5, y = 3.7, label = TeX(paste0("$Contrast_{U - I}$ = -0.47,", " 95%CI = -0.72, -0.22**")), size =4) +
  geom_segment(aes(x = 1, xend = 3, y = 4.2, yend = 4.2), color = "black") +
  annotate("text",  x = 2, y = 4.4, label = TeX(paste0("$Contrast_{U - C}$ = -1.11,", " 95%CI = -1.70, -0.50**")), size =4) + 
  geom_segment(aes(x = 2, xend = 3, y = 2.9, yend = 2.9), color = "black") +
  annotate("text",  x = 2.5, y = 3.1, label = TeX(paste0("$Contrast_{C - I}$ = -0.64,"," 95%CI = -1.24, -0.025**")), size =4) +
  annotate("text",  x = 1, y = -4.2, label = TeX(paste0("T1 & T2")), size =6) +
  annotate("text",  x = 2, y = -4.2, label = TeX(paste0("T3 & T4")), size =6) +
  annotate("text",  x = 3, y = -4.2, label = TeX(paste0("T3 & T4")), size =6) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))

dat2<-dat_model %>% 
  filter(trait == "exploration")
trt <- as.factor(all_data_T$treatment)

# exploration
m2<-ggplot(all_data_T, aes(x = trt, y = exploration, group = trt, col = trt, fill = trt)) +
  geom_violin(aes(), trim = FALSE) +
  geom_jitter(aes(colour = trt),shape=16, position=position_jitter(0.1)) +  scale_color_manual(labels = c("Uninfected/Control","Infected","Control"), values = c("#8A05A9", "#26ABA5","#FEE502"))+
  scale_fill_manual(values = c("#D4A5DF", "#91E5E1","#FFFBD6")) +
  geom_errorbar(data = dat2,aes(x = treatment,y = estimate, ymin= Lower_CI, ymax=Higher_CI), width=0.25, linewidth = 1, color = "black", inherit.aes = FALSE)+
  geom_point(data=dat2, mapping=aes(x= treatment, y= estimate), size=3, shape=21, fill = "white", inherit.aes = FALSE)+
  labs(x = "Treatment", 
       y = "Surface covered (%)") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank())+
  geom_segment(aes(x = 1, xend = 2, y = 0.58, yend = 0.58), color = "black") +
  annotate("text",  x = 1.5, y = 0.6, label = TeX(paste0("$Contrast_{U - I}$ = 0.42,", " 95%CI = 0.18, 0.67**")), size =4) + 
  geom_segment(aes(x = 1, xend = 3, y = 0.64, yend = 0.64), color = "black") + 
  annotate("text",  x = 2, y = 0.66, label = TeX(paste0("$Contrast_{U - C}$ = 1.13,", " 95%CI =  0.67, 1.62**")), size =4) + 
  geom_segment(aes(x = 2, xend = 3, y = 0.52, yend = 0.52), color = "black") + 
  annotate("text",  x = 2.5, y = 0.54, label = TeX(paste0("$Contrast_{C - I}$ = 0.34,", " 95%CI =  0.30, 0.37**")), size =4) + 
  annotate("text",  x = 1, y = -0.05, label = TeX(paste0("T1 & T2")), size =6) +
  annotate("text",  x = 2, y = -0.05, label = TeX(paste0("T3 & T4")), size =6) +
  annotate("text",  x = 3, y = -0.05, label = TeX(paste0("T3 & T4")), size =6)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16))

# activity
dat3<-dat_model %>% 
  filter(trait == "logactivity")

m3<-ggplot(all_data_T, aes(x = trt, y = z_log_activity, group = trt, col = trt, fill = trt)) +
  geom_violin(aes(), trim = FALSE) +
  geom_jitter(aes(colour = trt), shape=16, position=position_jitter(0.1)) +
  scale_color_manual(labels = c("Uninfected/Control","Infected","Control"), values = c("#8A05A9", "#26ABA5","#FEE502"))+
  scale_fill_manual(values = c("#D4A5DF", "#91E5E1","#FFFBD6")) +
  geom_errorbar(data = dat3,aes(x = treatment,y = estimate, ymin= Lower_CI, ymax=Higher_CI), width=0.25, linewidth =1, color = "black", inherit.aes = FALSE)+
  geom_point(data=dat3, mapping=aes(x= treatment, y= estimate), size=3, shape=21, fill = "white", inherit.aes = FALSE)+
  labs(x = "Treatment", 
       y = TeX("Distance swam$_{log_{2}}$ (cm)")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  geom_segment(aes(x = 1, xend = 2, y = 3.6, yend = 3.6), color = "black") +
  annotate("text",  x = 1.5, y = 3.8, label = TeX(paste0("$Contrast_{U - I}$ = -0.26,", " 95%CI = -0.54, 0.02")), size =4) + 
  geom_segment(aes(x = 1, xend = 3, y = 4.4, yend = 4.4), color = "black") + 
  annotate("text",  x = 2, y = 4.6, label = TeX(paste0("$Contrast_{U - C}$ = -0.22,", " 95%CI = -0.69, 0.25")), size =4) + 
  geom_segment(aes(x = 2, xend = 3, y = 2.8, yend = 2.8), color = "black") + 
  annotate("text",  x = 2.5, y = 3, label = TeX(paste0("$Contrast_{C - I}$ = 0.04,", " 95%CI = -0.43, 0.53")), size =4) + 
  annotate("text",  x = 1, y = -4, label = TeX(paste0("T1 & T2")), size =6) +
  annotate("text",  x = 2, y = -4, label = TeX(paste0("T3 & T4")), size =6) +
  annotate("text",  x = 3, y = -4, label = TeX(paste0("T3 & T4")), size =6)+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"))

# combine all plots
m<-plot_grid(m1,m2,m3,labels = "AUTO", ncol = 1)

# save figure
ggsave("./fig2.png", plot = m, width = 20, height = 30, units = "cm")

fig2 <- image_read("./fig2.png") 
fig2
############################
# FIGURE 3
############################
# Lets create the predictions with the posterior for each variable. Here, we want to vary ces, but keep the other variables at the mean
# First we want to create a design matrix. 
dat_parasite <- read.table("./dat_models_infected.csv",header=T, sep=",")
newdat <- data.frame(intercept = 1, z_ces = seq(min(dat_parasite$z_ces), max(dat_parasite$z_ces), length.out = 100),
                     z_bs2 = 0,
                     z_bc = 0)
 
### BOLDNESS PREDICTIONS ###
# Make predictions from our model to build slope and confidence intervals
# FOR CESTODES
pred_ces_brms <- posterior_epred(model_6, re_formula = NA, newdata = newdat)
bold_ces <- pred_ces_brms[,,"zlogboldness"]
summ_bold_ces <- apply(bold_ces, 2, function(x) mean(x))
summ_boldL95_ces <- apply(bold_ces, 2, function(x) quantile(x, 0.025))
summ_boldU95_ces <- apply(bold_ces, 2, function(x) quantile(x, 0.975))

# mean and sd for boldness and parasite density
# boldness
mean_bold <- mean(dat_parasite$log_boldness)
sd_bold <- sd(dat_parasite$log_boldness)
# bs
mean_bs<- mean(dat_parasite$dens_bs2)
sd_bs<- sd(dat_parasite$dens_bs2)
# cestode
mean_ces<- mean(dat_parasite$dens_ces)
sd_ces<- sd(dat_parasite$dens_ces)

# raw units
# for cestode
newdat$ces<-(newdat$z_ces * sd_ces) + mean_ces
range(newdat$ces)

# for boldness-ces
raw_bold_ces<-(summ_bold_ces * sd_bold) + mean_bold
range(raw_bold_ces) 
raw_bold_ces_L95<-(summ_boldL95_ces * sd_bold) + mean_bold
range(raw_bold_ces_L95) 
raw_bold_ces_U95<-(summ_boldU95_ces * sd_bold) + mean_bold
range(raw_bold_ces_U95) 

# create dataframe
boldness_ces_dat <- cbind(newdat, raw_bold_ces, raw_bold_ces_L95, raw_bold_ces_U95)

#FOR BLACK SPOT NOW
newdat2 <- data.frame(intercept = 1, z_ces = 0,
                      z_bs2 = seq(min(dat_parasite$z_bs2), max(dat_parasite$z_bs2), length.out = 100),
                      z_bc = 0)

pred_bs_brms <- posterior_epred(model_6, re_formula = NA, newdata = newdat2)
boldness_bs <- pred_bs_brms[,,"zlogboldness"]
summ_bold_bs <- apply(boldness_bs, 2, function(x) mean(x))
summ_boldL95_bs <- apply(boldness_bs, 2, function(x) quantile(x, 0.025))
summ_boldU95_bs <- apply(boldness_bs, 2, function(x) quantile(x, 0.975))

# raw units
# for bs
newdat2$bs<-(newdat2$z_bs2 * sd_bs) + mean_bs
range(newdat2$bs)

# for boldness-bs
raw_bold_bs<-(summ_bold_bs * sd_bold) + mean_bold
range(raw_bold_bs) 
raw_bold_bs_L95<-(summ_boldL95_bs * sd_bold) + mean_bold
range(raw_bold_bs_L95) 
raw_bold_bs_U95<-(summ_boldU95_bs * sd_bold) + mean_bold
range(raw_bold_bs_U95) 

boldness_bs_dat <- cbind(newdat2, raw_bold_bs, raw_bold_bs_L95, raw_bold_bs_U95)

# figure boldness
n1<- ggplot(data=dat_parasite)+
  geom_point(mapping = aes(x = dens_ces, y = log_boldness), color = "#26ABA5")+
  geom_smooth(data = boldness_ces_dat, ggplot2::aes(x = ces, y = raw_bold_ces), method =  "loess", formula = y~x, se = FALSE,lty = "solid", color = "#26ABA5", size=1) +
  geom_smooth(data = boldness_ces_dat, ggplot2::aes(x = ces, y = raw_bold_ces_L95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#26ABA5", size=0.8)+
  geom_smooth(data = boldness_ces_dat, ggplot2::aes(x = ces, y = raw_bold_ces_U95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#26ABA5", size=0.8) + 
  geom_point(mapping = aes(x = dens_bs2, y = log_boldness), color = "#8A05A9")+
  geom_smooth(data = boldness_bs_dat, ggplot2::aes(x = bs, y = raw_bold_bs), method =  "loess", formula = y~x, se = FALSE,lty = "solid", color = "#8A05A9", size=1) +
  geom_smooth(data = boldness_bs_dat, ggplot2::aes(x = bs, y = raw_bold_bs_L95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#8A05A9", size=0.8)+
  geom_smooth(data = boldness_bs_dat, ggplot2::aes(x = bs, y = raw_bold_bs_U95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#8A05A9", size=0.8)+
  theme_classic() +
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position="none") + labs(x = TeX("Parasite density (no/g)"), y = TeX("Emergence time$_{log_{2}}$"), title = "") +
  annotate("text", x = 15, y = 1.6, label = TeX(paste0("$beta_{bs}$ = ", " -0.09, ", " 95%CI = -0.29, 0.10")), size =4, color = "#440154") + 
  annotate("text", x = 15, y = 1.8, label = TeX(paste0("$beta_{ces}$ = "," 0.11, ", " 95%CI = -0.10, 0.30")), size=4, color = "#21918c")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16, face="bold"))

### EXPLORATION PREDICTIONS ###
# for cestode
pred_ces_brms <- posterior_epred(model_6, re_formula = NA, newdata = newdat)
exp_ces <- pred_ces_brms[,,"zexploration"]
summ_exp_ces <- apply(exp_ces, 2, function(x) mean(x))
summ_expL95_ces <- apply(exp_ces, 2, function(x) quantile(x, 0.025))
summ_expU95_ces <- apply(exp_ces, 2, function(x) quantile(x, 0.975))

# mean and sd for exploration
mean_exp<-mean(dat_parasite$exploration)
sd_exp<-sd(dat_parasite$exploration)

# raw units
# for ces
newdat$ces<-(newdat$z_ces * sd_ces) + mean_ces
range(newdat$ces)

# for exploration-ces
raw_exp_ces<-(summ_exp_ces * sd_exp) + mean_exp
range(raw_exp_ces) 
raw_exp_ces_L95<-(summ_expL95_ces * sd_exp) + mean_exp
range(raw_exp_ces_L95) 
raw_exp_ces_U95<-(summ_expU95_ces * sd_exp) + mean_exp
range(raw_exp_ces_U95) 

# create dataframe
exp_ces_dat <- cbind(newdat, raw_exp_ces, raw_exp_ces_L95, raw_exp_ces_U95)

# for black spot
pred_bs_brms <- posterior_epred(model_6, re_formula = NA, newdata = newdat2)
exp_bs <- pred_bs_brms[,,"zexploration"]
summ_exp_bs <- apply(exp_bs, 2, function(x) mean(x))
summ_expL95_bs <- apply(exp_bs, 2, function(x) quantile(x, 0.025))
summ_expU95_bs <- apply(exp_bs, 2, function(x) quantile(x, 0.975))

# for exploration-bs
raw_exp_bs<-(summ_exp_bs * sd_exp) + mean_exp
range(raw_exp_bs) 
raw_exp_bs_L95<-(summ_expL95_bs * sd_exp) + mean_exp
range(raw_exp_ces_L95) 
raw_exp_bs_U95<-(summ_expU95_bs * sd_exp) + mean_exp
range(raw_exp_bs_U95) 

# create dataframe
exp_bs_dat <- cbind(newdat2, raw_exp_bs, raw_exp_bs_L95, raw_exp_bs_U95)

# figure exploration
n2<-ggplot(data=dat_parasite)+
  geom_point(mapping = aes(x = dens_ces, y = exploration), color = "#26ABA5")+
  geom_smooth(data = exp_ces_dat, ggplot2::aes(x = ces, y = raw_exp_ces), method =  "loess", formula = y~x, se = FALSE,lty = "solid", color = "#26ABA5", size=1) +
  geom_smooth(data = exp_ces_dat, ggplot2::aes(x = ces, y = raw_exp_ces_L95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#26ABA5", size=0.8)+
  geom_smooth(data = exp_ces_dat, ggplot2::aes(x = ces, y = raw_exp_ces_U95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#26ABA5", size=0.8)+
  geom_point(mapping = aes(x = dens_bs2, y = exploration), color = "#8A05A9") +
  geom_smooth(data = exp_bs_dat, ggplot2::aes(x = bs, y = raw_exp_bs), method =  "loess", formula = y~x, se = FALSE,lty = "solid", color = "#8A05A9", size=1) +
  geom_smooth(data = exp_bs_dat, ggplot2::aes(x = bs, y = raw_exp_bs_L95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#8A05A9", size=0.8)+
  geom_smooth(data = exp_bs_dat, ggplot2::aes(x = bs, y = raw_exp_bs_U95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#8A05A9", size=0.8)+
  theme_classic() +
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position="none") + labs(x = TeX("Parasite density (no/g)"), y = TeX("Surface covered (%)"), title = "") +
  annotate("text", x = 15, y = 0.45, label = TeX(paste0("$beta_{bs}$ = ", " 0.10, ", " 95%CI = -0.07, 0.27")), size =4, color = "#440154") + 
  annotate("text", x = 15, y = 0.47, label = TeX(paste0("$beta_{ces}$ = "," -0.11,  ", " 95%CI = -0.29, 0.07")), size=4, color = "#21918c")+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16, face="bold"))

### ACTIVITY PREDICTIONS ###
# for cestode
pred_ces_brms <- posterior_epred(model_6, re_formula = NA, newdata = newdat)
act_ces <- pred_ces_brms[,,"zlogactivity"]
summ_act_ces <- apply(act_ces, 2, function(x) mean(x))
summ_actL95_ces <- apply(act_ces, 2, function(x) quantile(x, 0.025))
summ_actU95_ces <- apply(act_ces, 2, function(x) quantile(x, 0.975))

# mean and sd for activity
mean_act<-mean(dat_parasite$log_activity)
sd_act<-sd(dat_parasite$log_activity)

# for activity-ces
raw_act_ces<-(summ_act_ces * sd_act) + mean_act
range(raw_act_ces) 
raw_act_ces_L95<-(summ_actL95_ces * sd_act) + mean_act
range(raw_act_ces_L95) 
raw_act_ces_U95<-(summ_actU95_ces * sd_act) + mean_act
range(raw_act_ces_U95) 

# create dataframe
act_ces_dat <- cbind(newdat, raw_act_ces, raw_act_ces_L95, raw_act_ces_U95)

# for black spot
pred_bs_brms <- posterior_epred(model_6, re_formula = NA, newdata = newdat2)
act_bs <- pred_bs_brms[,,"zlogactivity"]
summ_act_bs <- apply(act_bs, 2, function(x) mean(x))
summ_actL95_bs <- apply(act_bs, 2, function(x) quantile(x, 0.025))
summ_actU95_bs <- apply(act_bs, 2, function(x) quantile(x, 0.975))

# for activity-bs
raw_act_bs<-(summ_act_bs * sd_act) + mean_act
range(raw_act_bs) 
raw_act_bs_L95<-(summ_actL95_bs * sd_act) + mean_act
range(raw_act_bs_L95) 
raw_act_bs_U95<-(summ_actU95_bs * sd_act) + mean_act
range(raw_act_bs_U95) 

# create dataframe
act_bs_dat <- cbind(newdat2, raw_act_bs, raw_act_bs_L95, raw_act_bs_U95)

# figure activity
n3<-ggplot(data=dat_parasite)+
  geom_point(mapping = aes(x = dens_ces, y = log_activity),color = "#26ABA5")+
  geom_smooth(data = act_ces_dat, ggplot2::aes(x = ces, y = raw_act_ces), method =  "loess", formula = y~x, se = FALSE,lty = "solid", color = "#26ABA5", size=1) +
  geom_smooth(data = act_ces_dat, ggplot2::aes(x = ces, y = raw_act_ces_L95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#26ABA5", size=0.8)+
  geom_smooth(data = act_ces_dat, ggplot2::aes(x = ces, y = raw_act_ces_U95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#26ABA5", size=0.8)+
  geom_point(mapping = aes(x = dens_bs2, y = log_activity), color = "#8A05A9") +
  geom_smooth(data = act_bs_dat, ggplot2::aes(x = bs, y = raw_act_bs), method =  "loess", formula = y~x, se = FALSE,lty = "solid", color = "#8A05A9", size=1) +
  geom_smooth(data = act_bs_dat, ggplot2::aes(x = bs, y = raw_act_bs_L95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#8A05A9", size=0.8)+
  geom_smooth(data = act_bs_dat, ggplot2::aes(x = bs, y = raw_act_bs_U95), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", color = "#8A05A9", size=0.8)+
  theme_classic() +
  theme(strip.background = element_rect(fill = "white", colour = "white")) +
  theme(legend.position="none") + labs(x = TeX("Parasite density (no/g)"), y = TeX("Distance swam$_{log_{2}}$"), title = "") +
  annotate("text", x = 15, y = 1.6, label = TeX(paste0("$beta_{bs}$ = ", " -0.21, ", " 95%CI = -0.39, -0.03 **")), size =4, color = "#440154") +
  annotate("text", x = 15, y = 1.8, label = TeX(paste0("$beta_{ces}$ = "," 0.05, ", " 95%CI = -0.13, 0.24")), size=4, color = "#21918c")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16, face="bold"))

# combine plots
p<-plot_grid(
  n1,n2,n3,NULL,
  labels = c("A","B","C",""), ncol = 2)

# save plot
ggsave("./fig3.png", plot = p, width = 21, height = 22, units = "cm")

fig3 <- image_read("./fig3.png")
fig3
#################################
# SUPPLEMENTARY MATERIAL: FIGURE CAGES & MAP
#################################
#Script from Juliane Vigneault

# ----- Loading packages ----- #
pacman::p_load(sf,dplyr,measurements,ggplot2,stringr,tmap,leaflet,ggspatial,magick,cowplot, gridExtra)

# ----- Loading data ----- #
CombinedData <- read.csv("./cromwell.csv",header=T, sep=";",stringsAsFactors=FALSE, fileEncoding="latin1")

# ---- 1) Load GPS coordinates

## Attribute table ##
# ---- 2) Select variables
attributes <- CombinedData %>% 
  select_("cage", "lat", "long", "dens_tot","dens_ces","dens_bs")

# Coordinates conversion #
# ---- 3) Change to decimals
attributes$lat <- str_replace(attributes$lat, "°", " ")
attributes$lat <- str_remove(attributes$lat, "'")
attributes$lat <- conv_unit(attributes$lat, from = "deg_dec_min", to = "dec_deg")
attributes$lat <- as.numeric(attributes$lat)

attributes$long <- str_replace(attributes$long, "°", " ")
attributes$long <- str_remove(attributes$long, "'")
attributes$long <- conv_unit(attributes$long, from = "deg_dec_min", to = "dec_deg")
attributes$long <- as.numeric(attributes$long)*(-1) #Add negative sign as coordinates are from western hemisphere

#save new data
write.table(attributes, file = "map_data.csv",
            sep = ";", row.names = F)

attributes <- read.csv("./map_data.csv",header=T, sep=";")
quai <- read.csv("./quai.csv",header=T, sep=";")

# Cromwell #
# ---- 4) Load lake map
CROM <- st_read("./Cromwell.shp")

#CROM.att <- attributes %>% filter(Lake == "Cromwell")
cage_number<-as.factor(attributes$cage)
# ---- 5) Visualization
# Specify the number of decimal places in the labels
# map for black spot density
CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "#DFEFFE", color = "black")+
  scale_x_continuous(breaks=c(-74.002,-73.998, -73.994),labels = function(x) paste0(format(x, digit = 5), "\u00B0","W")) +
  scale_y_continuous(breaks=c(45.988,45.989, 45.990),labels = function(y) paste0(format(y, digit = 5), "\u00B0","N")) +
  geom_point(data = attributes, aes(x = long, y = lat, color = dens_bs), size = 9)+ #Points GPS de tes cages
  geom_text(data = attributes, aes(x = long, y = lat, label = cage), size = 7) +
  geom_point(data = quai, aes(x = long, y = lat),size=5, shape=22, fill = "black", inherit.aes = FALSE) + 
  theme_classic() +
  labs(title = "", 
       subtitle = "") +
  xlab("") + ylab("")
CROM.plot<-CROM.plot + theme(plot.title = element_text(size=14, face="bold")) + theme(panel.background = element_rect(fill = '#F7F7F7', color = 'black')) + scale_colour_gradient(low = "#FEFFD0",high = "#B80A0A")

p1<-CROM.plot +annotation_scale(location = "bl", bar_cols = c("black","white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical(fill =c("black","white"),line_col = "black"))+ theme(legend.position = c(0.88, 0.40), legend.background = element_rect(fill = "#F7F7F7", color = "black"), legend.key.size = unit(0.8, 'cm'), legend.title = element_text(size=15,face = "bold"), legend.text = element_text(size=12),plot.margin=unit(c(1,1,-0.5,1), "cm")) +
  labs(color = "Mean \n black spot \n density (no/g) \n per cage")

#map for cestode density
CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "#DFEFFE", color = "black")+
  scale_x_continuous(breaks=c(-74.002,-73.998, -73.994),labels = function(x) paste0(format(x, digit = 5), "\u00B0","W")) +
  scale_y_continuous(breaks=c(45.988,45.989, 45.990),labels = function(y) paste0(format(y, digit = 5), "\u00B0","N")) +
  geom_point(data = attributes, aes(x = long, y = lat, color = dens_ces), size = 9)+ #Points GPS de tes cages
  geom_text(data = attributes, aes(x = long, y = lat, label = cage), size = 7) +
  geom_point(data = quai, aes(x = long, y = lat),size=5, shape=22, fill = "black", inherit.aes = FALSE) +
  theme_classic() +
  labs(title = "", 
       subtitle = "") +
  xlab("") + ylab("")

CROM.plot<-CROM.plot + theme(plot.title = element_text(size=14, face="bold")) + theme(panel.background = element_rect(fill = '#F7F7F7', color = 'black')) + scale_colour_gradient(low = "#FEFFD0",high = "#B80A0A")

p2<-CROM.plot +annotation_scale(location = "bl", bar_cols = c("black","white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical(fill =c("black","white"),line_col = "black")) + theme(legend.position = c(0.88, 0.40), legend.background = element_rect(fill = "#F7F7F7", color = "black"), legend.key.size = unit(0.8, 'cm'), legend.title = element_text(size=15, face = "bold"), legend.text = element_text(size=12), plot.margin=unit(c(-0.5,1,1,1), "cm")) + labs(color = "Mean \n cestode \n density (no/g) \n per cage")

# combine maps
p<-grid.arrange(p1,p2)

# save
ggsave("./map.png", plot = p, width = 40, height = 30, units = "cm")

map <- image_read("./map.png")

#################################
# SUPPLEMENTARY MATERIAL: FIGURE R & VARIANCE
#################################
# create dataset for all repeatability measures
dat_rep<-data.frame(
  "R"=c("Emergence time","Surface covered","Distance swam","Emergence time","Surface covered","Distance swam","Emergence time","Surface covered","Distance swam","Emergence time","Surface covered","Distance swam"),
  "Treatment" = c("Overall", "Overall","Overall","Uninfected","Uninfected","Uninfected","Infected","Infected","Infected","Control","Control","Control"),
  "Repeatability"=c(R_bold_mean,R_exp_mean,R_act_mean
                    ,R_C_bold,R_exp_C_mean,R_act_C,R_E_bold,R_exp_E_mean,R_act_E,R_T_bold,R_T_exp,R_T_act), 
  "Lower_CI" = c(R_Blb_bold,R_Blb_exp,R_Blb_act,R_Blb_bold_C,R_Blb_exp_C,R_Blb_act_C,R_Blb_bold_E,R_Blb_exp_E,R_Blb_act_E,R_Blb_bold_T,R_Blb_exp_T,R_Blb_act_T),
  "Higher_CI" = c(R_Bub_bold,R_Bub_exp,R_Bub_act,R_Bub_bold_C,R_Bub_exp_C,R_Bub_act_C,R_Bub_bold_E,R_Bub_exp_E,R_Bub_act_E,R_Bub_bold_T,R_Bub_exp_T,R_Bub_act_T))

# create dataframe for boldness variance (among and within)
dat_variance<-data.frame(
  "Treatment"=c("Uninfected","Uninfected","Infected","Infected"),
  "Variance_type" = c("within-individual","among-individual","within-individual","among-individual"),
  "Variance" = c(res_c_b_mean, fish_c_b_mean, res_e_b_mean, fish_e_b_mean),
  "low" = c(res_c_b_low, fish_c_b_low,res_e_b_low, fish_e_b_low),
  "high" = c(res_c_b_high,fish_c_b_high, res_e_b_high, fish_e_b_high)
)

dat_r_b<-data.frame(
  "Treatment"=c("Uninfected1","Infected1"),
  "Repeatability"=c(R_C_bold,R_E_bold),
  "Lower_CI" = c(R_Blb_bold_C,R_Blb_bold_E),
  "Higher_CI" = c(R_Bub_bold_C,R_Bub_bold_E))

# variance and repeatability for boldness
v1<-ggplot(data = dat_variance, aes(x = Treatment, y = Variance, fill = Variance_type)) +
  geom_crossbar(aes(ymin = low, ymax = high), width = 0.5,  position = position_dodge(width = 0.7)) +
  theme_classic() + theme(legend.position = "none") + 
  ylab("Variance") +
  xlab("") +
  geom_point(data=dat_r_b, mapping=aes(x= Treatment , y= Repeatability), size=3,inherit.aes = FALSE)+
  geom_errorbar(data=dat_r_b, mapping = aes(x = Treatment, ymin= Lower_CI, ymax=Higher_CI),width = 0,inherit.aes = FALSE) +
  scale_fill_manual(values = c("#8DDCCA","#DC8DB6")) + scale_x_discrete(limits=c("Uninfected","Uninfected1","Infected","Infected1"), labels = c("Uninfected","","Infected", ""))

# create dataframe for exploration variance (among and within)
dat_variance_exp<-data.frame(
  "Treatment"=c("Uninfected","Uninfected","Infected","Infected"),
  "Variance_type" = c("within-individual","among-individual","within-individual","among-individual"),
  "Variance" = c(res_c_ex_mean, fish_c_ex_mean, res_e_ex_mean, fish_e_ex_mean),
  "low" = c(res_c_ex_low, fish_c_ex_low,res_e_ex_low, fish_e_ex_low),
  "high" = c(res_c_ex_high,fish_c_ex_high, res_e_ex_high, fish_e_ex_high)
)

dat_r_exp<-data.frame(
  "Treatment"=c("Uninfected1","Infected1"),
  "Repeatability"=c(R_exp_C_mean,R_exp_E_mean),
  "Lower_CI" = c(R_Blb_exp_C,R_Blb_exp_E),
  "Higher_CI" = c(R_Bub_exp_C,R_Bub_exp_E))

# variance and repeatability for exploration
v2<-ggplot(data = dat_variance_exp, aes(x = Treatment, y = Variance, fill = Variance_type)) +
  geom_crossbar(aes(ymin = low, ymax = high), width = 0.5, position = position_dodge(width = 0.7)) +
  theme_classic() + theme(legend.position = "none") +
  ylab("") +
  xlab("Treatment") +
  geom_point(data=dat_r_exp, mapping=aes(x= Treatment , y= Repeatability), size=3,inherit.aes = FALSE)+
  geom_errorbar(data=dat_r_exp, mapping = aes(x = Treatment, ymin= Lower_CI, ymax=Higher_CI),width = 0,inherit.aes = FALSE) +
  scale_fill_manual(values = c("#8DDCCA","#DC8DB6")) + scale_x_discrete(limits=c("Uninfected","Uninfected1","Infected","Infected1"), labels = c("Uninfected","","Infected", ""))

# create dataframe for activity variance (among and within)
dat_variance_act<-data.frame(
  "Treatment"=c("Uninfected","Uninfected","Infected","Infected"),
  "Variance_type" = c("within-individual","among-individual","within-individual","among-individual"),
  "Variance" = c(res_c_ac_mean, fish_c_ac_mean, res_e_ac_mean, fish_e_ac_mean),
  "low" = c(res_c_ac_low, fish_c_ac_low,res_e_ac_low, fish_e_ac_low),
  "high" = c(res_c_ac_high,fish_c_ac_high, res_e_ac_high, fish_e_ac_high)
)

dat_r_act<-data.frame(
  "Treatment"=c("Uninfected1","Infected1"),
  "Repeatability"=c(R_act_C,R_act_E),
  "Lower_CI" = c(R_Blb_act_C,R_Blb_act_E),
  "Higher_CI" = c(R_Bub_act_C,R_Bub_act_E))

# variance and repeatability for activity
v3<-ggplot(data = dat_variance_act, aes(x = Treatment, y = Variance, fill = Variance_type)) +
  geom_crossbar(aes(ymin = low, ymax = high), width = 0.5,  position = position_dodge(width = 0.7)) +
  theme_classic() + theme(legend.position = "none") +
  ylab("") +
  xlab("") +
  geom_point(data=dat_r_act, mapping=aes(x= Treatment , y= Repeatability), size=3,inherit.aes = FALSE) +
  geom_errorbar(data=dat_r_act, mapping = aes(x = Treatment, ymin= Lower_CI, ymax=Higher_CI),width = 0,inherit.aes = FALSE) + 
  scale_fill_manual(values = c("#8DDCCA","#DC8DB6")) +scale_x_discrete(limits=c("Uninfected","Uninfected1","Infected","Infected1"), labels = c("Uninfected","","Infected", ""))

# combine plots
v4<-plot_grid(
  v1, v2, v3,
  labels = c("Boldness","Exploration","Activity"), label_size = 8,
  align = "h", ncol = 3)

# save
ggsave("./fig_var.png", plot = v4, width = 17, height = 7, units = "cm")

# figure for repeatability only
dat_rep$Treatment = factor(dat_rep$Treatment, levels=c('Overall','Uninfected','Infected','Control'))

w1<-
  ggplot(dat_rep)+ 
  geom_point(data=dat_rep, mapping=aes(x= Repeatability, y= R), size=3)+
  geom_errorbarh(aes(y = R, xmin= Lower_CI, xmax=Higher_CI), height = 0.2, linewidth =0.9, inherit.aes = FALSE)+scale_x_continuous()+facet_grid(Treatment ~ .)+geom_vline(xintercept=0.1, color = "red2", linetype = 2)+geom_hline(yintercept= "Shyness_Exploration", color = "red2", linetype = 2)+ xlab("Repeatability (R)") + ylab("")+ theme_classic()+ theme(strip.text.y = element_text(size=12, face="bold", angle = 360),strip.background = element_rect(colour="white", fill="white"))

# save
ggsave("./fig_rep.png", plot = w1, width = 17, height = 12, units = "cm")

# visualize 
fig_rep <- image_read("./fig_rep.png")  
fig_rep

#################################
# SUPPLEMENTARY MATERIAL: FIGURE correlations
#################################
#created dataset for all correlations
dat_cor<-data.frame(
  "BS"=c("Emergence time-Distance swam","Emergence time-Surface covered","Distance swam-Surface covered","Emergence time-Distance swam","Emergence time-Surface covered","Distance swam-Surface covered","Emergence time-Distance swam","Emergence time-Surface covered","Distance swam-Surface covered"),
  "Treatment" = c("Overall", "Overall","Overall","Uninfected","Uninfected","Uninfected","Infected","Infected","Infected"),
  "Correlation"=c(b_act_o,b_exp_o,act_exp_o,b_act_C_mean,b_exp_C_mean,act_exp_C_mean,b_act_E_mean,b_exp_E_mean,act_exp_E_mean), 
  "Lower_CI_o" = c(low_b_act_o,low_b_exp_o,low_act_exp_o,low_b_act_C,low_b_exp_C,low_act_exp_C,low_b_act_E,low_b_exp_E,low_act_exp_E),
  "Higher_CI_o" = c(high_b_act_o,high_b_exp_o,high_act_exp_o,high_b_act_C,high_b_exp_C,high_act_exp_C,high_b_act_E,high_b_exp_E,high_act_exp_E))

dat_cor$Treatment = factor(dat_cor$Treatment, levels=c('Overall','Uninfected','Infected'))

# figure
m1<-
  ggplot(dat_cor)+ 
  geom_point(data=dat_cor, mapping=aes(x= Correlation, y= BS), size=3)+
  geom_errorbarh(aes(y = BS, xmin= Lower_CI_o, xmax=Higher_CI_o), height = 0.2, linewidth =0.9, inherit.aes = FALSE)+scale_x_continuous()+facet_grid(Treatment ~ .)+geom_vline(xintercept=0, color = "red2", linetype = 2)+geom_hline(yintercept= "Shyness_Exploration", color = "red2", linetype = 2)+ xlab("Correlation") + ylab("")+ theme_classic()+ theme(strip.text.y = element_text(size=12, face="bold", angle = 360),strip.background = element_rect(colour="white", fill="white"))

# save
ggsave("./fig_cor.png", plot = m1, width = 17, height = 12, units = "cm")

# visualize
fig_cor <- image_read("./fig_cor.png")  
fig_cor

#################END OF SCRIPT##################
