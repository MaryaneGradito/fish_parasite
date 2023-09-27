### Project : Double trouble? Parasitic co-infection reduces body condition and activity levels in pumpkinseed sunfish (Lepomis gibbosus)
### Contributors: Maryane Gradito, Frédérique Dubois, Daniel Noble, Sandra Binning
### Analyse of data and codes
## Target journal: Proc B

############################
# Load packages
#############################
source("./R/func.R")
pacman::p_load(tidyverse, brms, glmmTMB, gt, latex2exp, posterior, gt, glue, dplyr,magrittr, ggplot2, cowplot, jpeg, magick,rptR,webshot2,lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander)

#############################
# Import raw data
#############################
all_data <- read.table("./data_raw/all_data_T.csv",header=T, sep=";") #with control fish
all_data

all_data_1 <- read.table("./data_raw/all_data.csv",header=T, sep=";") #without control fish
all_data_1

#############################
# Data processing
#############################
### Create a new column with total cestode (alive + dead)
all_data$P04_tot<-all_data$P04_alive + all_data$P04_dead
all_data$P04_tot #we have all cestodes together, dead or alive, to know their weight

### Calculate adjusted fish mass (total fish mass - parasite mass)
### Mean average for adult cestodes: 0.003g, larval form: 0.0008g
## P04_tot*0.003 / P06*0008
### I can't calculate the adjusted mass for when they arrive in the lab and before the caging experiment, since we don't know their internal parasite loads yet

### Adjusted mass after caging experiment
all_data$adj_mass3<-(all_data$mass_3-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

### Adjusted mass before sacrifice
all_data$adj_mass4<-(all_data$mass_4-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

# Number of black spot that we want in our models (pre - post number)
all_data$bs_mod<-(all_data$BS_post_tot - all_data$BS_pre)
all_data$bs_mod[all_data$bs_mod<0] <- 0

### Add new column for parasite density in total (co-infection)
tot_parasite<-(all_data$bs_mod + (all_data$P04_alive + all_data$P06))
all_data$dens_tot<-(tot_parasite/all_data$adj_mass4)

### Add new column for BS density
all_data$dens_bs2<-(all_data$bs_mod/all_data$adj_mass4)

### Add new column for cestode density
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

#Boldness
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

dat <- all_data
dat <- dat %>%
  dplyr::mutate(id = factor(ID_fish, levels = unique(ID_fish)),
                z_log_activity = scale(log_activity), z_log_boldness = scale(log_boldness),
                z_exploration = scale(exploration), tank1 = as.factor(bassin_bold), tank2 = as.factor(bassin_exp),(id = ID_fish))

### Save the new dataset with processed data
write.table(dat, file = "all_data_p_T.csv",
            sep = ",", row.names = F)

### Pivot data for each trial
#we need parasite density z-transform and body condition z-transform for the next model
all_dat <- read.table("./output/all_data_p_T.csv",header=T, sep=",")

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


write.table(dat_trials, file = "dat_bc.csv",
            sep = ",", row.names = F)
### Select for each group the data and scale 
### Infected group
dat_6 <- dat_trials %>% filter(treatment == "E") %>%  mutate(z_bc = scale(body_condition),
                                                             z_dens = scale(dens_tot),
                                                             z_ces = scale(dens_ces),
                                                             z_bs2 = scale(dens_bs2))
### Uninfected group
dat_5 <- dat_trials %>% filter(treatment == "C") %>% mutate(z_bc = scale(body_condition))

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
                    save_pars = save_pars(), file = "./output/models/model1", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))
prior_summary(model1)

### Compare models  
model1 <- add_criterion(model1, c("loo", "waic"),  moment_match = TRUE)

saveRDS(model1, file = "./output/models/model1.rds")

### Without tank effect
boldness_2 <- bf(z_log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_2 <- bf(z_log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_2 <- bf(z_exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
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

#############################
# Final models
#############################
### Model 1: to estimate repeatability
#import processed data
all_data_T <- read.table("./output/all_data_p._Tcsv",header=T, sep=",")

boldness_2 <- bf(z_log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_2 <- bf(z_log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_2 <- bf(z_exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()
model_1 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                      data = all_data_T, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_1", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

### Model 2: extract correlations
#import processed data
all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")

boldness_2 <- bf(z_log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_2 <- bf(z_log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_2 <- bf(z_exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()

model_2 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                    data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./output/models/model_2", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

### Model 3: random slope
boldness_3 <- bf(z_log_boldness ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
activity_3 <- bf(z_log_activity ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                 sigma ~ -1 + treatment) + gaussian()
explore_3 <- bf(z_exploration ~ 1 + treatment + (treatment |q| ID_fish) + (1 | cage),
                sigma ~ -1 + treatment) + gaussian()
model3 <- brms::brm(boldness_3 + activity_3 + explore_3 + set_rescor(TRUE), 
                    data = all_data, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(all = TRUE), file = "./output/models/model3", file_refit = "on_change",
                    control = list(adapt_delta = 0.98))

### Model 4: how parasite infection impact body condition
### import data
dat_bc <- read.table("./output/dat_bc.csv",header=T, sep=",")

body_condition <- bf(z_bc ~ 1 + z_ces + z_bs2 + treatment + (1 | ID_fish) + (1 | cage)) + gaussian()
model_4 <- brms::brm(body_condition, 
                        data = dat_bc, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(), file = "./output/models/model_4", file_refit = "on_change",
                        control = list(adapt_delta = 0.98))

### Model 5: how body condition influence personality (only uninfected fish)
### import data
dat_C <- read.table("./output/dat_models_C.csv",header=T, sep=",")

boldness_5 <- bf(z_log_boldness ~ 1 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_5 <- bf(z_log_activity ~ 1 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_5 <- bf(z_exploration  ~ 1 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_5 <- brms::brm(boldness_5 + activity_5 + explore_5 + set_rescor(TRUE), data = dat_C, iter = 6000, warmup = 2000, chains = 4, cores = 4, save_pars = save_pars(), file = "./output/models/model_5", file_refit = "on_change", control = list(adapt_delta = 0.98))

### Model 6: how co-infection influence personality
### import data
dat_E <- read.table("./output/dat_models_E.csv",header=T, sep=",")

boldness_6 <- bf(z_log_boldness ~ 1 + z_dens + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_6 <- bf(z_log_activity ~ 1 + z_dens + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_6 <- bf(z_exploration  ~ 1 + z_dens + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_6 <- brms::brm(boldness_6 + activity_6 + explore_6 + set_rescor(TRUE), 
                       data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./output/models/model_6", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

### Model 7: looking at both parasite type 
boldness_7 <- bf(z_log_boldness ~ 1 + z_ces + z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_7 <- bf(z_log_activity ~ 1 + z_ces + z_bs2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_7 <- bf(z_exploration  ~ 1 + z_ces + z_bs_2 + z_bc + (1 | ID_fish) + (1 | cage)) + gaussian()

model_7 <- brms::brm(boldness_7 + activity_7 + explore_7 + set_rescor(TRUE), 
                       data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                       save_pars = save_pars(), file = "./output/models/model_7", file_refit = "on_change",
                       control = list(adapt_delta = 0.98))

############################
# Load models
############################
model_1<-readRDS(file = "./output/models/model_1.rds")
model2 <- readRDS(file = "./output/models/model2.rds")
model3<-readRDS(file = "./output/models/model3.rds")
model4<-readRDS(file = "./output/models/model_4.rds")
model5 <- readRDS(file = "./output/models/model_5.rds")
model6 <- readRDS(file = "./output/models/model_6.rds")
model7<-readRDS(file = "./output/models/model_7.rds")

############################
# Repeatability C vs E
############################
#Extract posterior distribution from model 1 (repeatability)
post_sd_2 <- as_draws_df(model_1, variable = "^sd", regex = TRUE)
post_sd_C_2 <- post_sd_2[,grepl("C", colnames(post_sd_2))] #ID uninfected
post_sd_E_2 <- post_sd_2[,grepl("E", colnames(post_sd_2))] #ID experimentally infected
post_sd_T_2 <- post_sd_2[,grepl("T", colnames(post_sd_2))] #ID control
post_sd_cage_2 <- post_sd_2[,grepl("cage", colnames(post_sd_2))]
post_sd_sig_2 <- as_draws_df(model_1, variable = "^b_sigma", regex = TRUE)
post_sd_sig_C_2 <- exp(post_sd_sig_2[,grepl("C", colnames(post_sd_sig_2))]) #ID uninfected
post_sd_sig_E_2 <- exp(post_sd_sig_2[,grepl("E", colnames(post_sd_sig_2))]) #ID experimentally infected
post_sd_sig_T_2 <- exp(post_sd_sig_2[,grepl("T", colnames(post_sd_sig_2))]) #ID control

###################################
##  CONTROL GROUP & UNINFECTED
###################################

#Boldness for uninfected group
fish_c_b <- post_sd_C_2[,grep("zlogboldness", colnames(post_sd_C_2))]^2
cage_c_b <- post_sd_cage_2[ ,grep("zlogboldness", colnames(post_sd_cage_2))]^2
res_c_b <- post_sd_sig_C_2[,grep("b_sigma_zlogboldness_treatmentC", colnames(post_sd_sig_C_2))]^2

cage_c_b_low <- round(quantile(cage_c_b[,1], c(0.025, 0.975))[1], 3)
cage_c_b_high<- round(quantile(cage_c_b[,1], c(0.025, 0.975))[2], 3)
cage_c_b_mean<-mean(cage_c_b[,1])

fish_c_b_low <- round(quantile(fish_c_b[,1], c(0.025, 0.975))[1], 3)
fish_c_b_high<- round(quantile(fish_c_b[,1], c(0.025, 0.975))[2], 3)
fish_c_b_mean<-mean(fish_c_b[,1])

res_c_b_low <- round(quantile(res_c_b[,1], c(0.025, 0.975))[1], 3)
res_c_b_high <- round(quantile(res_c_b[,1], c(0.025, 0.975))[2], 3)
res_c_b_mean<-mean(res_c_b[,1])

#repeatability
R_C_boldness <-   fish_c_b / (fish_c_b + cage_c_b + res_c_b)

hist(R_C_boldness[,1])
R_C_bold <- mean(R_C_boldness[,1])
R_Blb_bold_C <- round(quantile(R_C_boldness[,1], c(0.025, 0.975))[1], 3)
R_Bub_bold_C <- round(quantile(R_C_boldness[,1], c(0.025, 0.975))[2], 3)

#Boldness for control group
fish_t_b <- post_sd_T_2[,grep("zlogboldness", colnames(post_sd_T_2))]^2
cage_t_b <- post_sd_cage_2[ ,grep("zlogboldness", colnames(post_sd_cage_2))]^2
res_t_b <- post_sd_sig_T_2[,grep("b_sigma_zlogboldness_treatmentT", colnames(post_sd_sig_T_2))]^2

#repeatability
R_T_boldness <-   fish_t_b / (fish_t_b + cage_t_b + res_t_b)

hist(R_T_boldness[,1])
R_T_bold <- mean(R_T_boldness[,1])
R_Blb_bold_T <- round(quantile(R_T_boldness[,1], c(0.025, 0.975))[1], 3)
R_Bub_bold_T <- round(quantile(R_T_boldness[,1], c(0.025, 0.975))[2], 3)

#exploration for uninfected group
fish_c_ex <- post_sd_C_2[,grep("zexploration", colnames(post_sd_C_2))]^2
cage_c_ex <- post_sd_cage_2[ ,grep("zexploration", colnames(post_sd_cage_2))]^2
res_c_ex <- post_sd_sig_C_2[,grep("b_sigma_zexploration_treatmentC", colnames(post_sd_sig_C_2))]^2

fish_c_ex_low <- round(quantile(fish_c_ex[,1], c(0.025, 0.975))[1], 3)
fish_c_ex_high<- round(quantile(fish_c_ex[,1], c(0.025, 0.975))[2], 3)
fish_c_ex_mean<-mean(fish_c_ex[,1])

res_c_ex_low <- round(quantile(res_c_ex[,1], c(0.025, 0.975))[1], 3)
res_c_ex_high <- round(quantile(res_c_ex[,1], c(0.025, 0.975))[2], 3)
res_c_ex_mean<-mean(res_c_ex[,1])

#repeatability
R_C_exploration <-   fish_c_ex / (fish_c_ex + cage_c_ex + res_c_ex)

hist(R_C_exploration[,1])
R_exp_C_mean<-mean(R_C_exploration[,1])
R_Blb_exp_C <- round(quantile(R_C_exploration[,1], c(0.025, 0.975))[1], 3)
R_Bub_exp_C <- round(quantile(R_C_exploration[,1], c(0.025, 0.975))[2], 3)

#Exploration for control group
fish_t_ex <- post_sd_T_2[,grep("zexploration", colnames(post_sd_T_2))]^2
cage_t_ex <- post_sd_cage_2[ ,grep("zexploration", colnames(post_sd_cage_2))]^2
res_t_ex <- post_sd_sig_T_2[,grep("b_sigma_zexploration_treatmentT", colnames(post_sd_sig_T_2))]^2

#repeatability
R_T_exploration <-   fish_t_ex / (fish_t_ex + cage_t_ex + res_t_ex)

hist(R_T_exploration[,1])
R_T_exp <- mean(R_T_exploration[,1])
R_Blb_exp_T <- round(quantile(R_T_exploration[,1], c(0.025, 0.975))[1], 3)
R_Bub_exp_T <- round(quantile(R_T_exploration[,1], c(0.025, 0.975))[2], 3)

#activity for uninfected group
fish_c_ac <- post_sd_C_2[,grep("zlogactivity", colnames(post_sd_C_2))]^2
cage_c_ac <- post_sd_cage_2[ ,grep("zlogactivity", colnames(post_sd_cage_2))]^2
res_c_ac <- post_sd_sig_C_2[,grep("b_sigma_zlogactivity_treatmentC", colnames(post_sd_sig_C_2))]^2

fish_c_ac_low <- round(quantile(fish_c_ac[,1], c(0.025, 0.975))[1], 3)
fish_c_ac_high<- round(quantile(fish_c_ac[,1], c(0.025, 0.975))[2], 3)
fish_c_ac_mean<-mean(fish_c_ac[,1])

res_c_ac_low <- round(quantile(res_c_ac[,1], c(0.025, 0.975))[1], 3)
res_c_ac_high <- round(quantile(res_c_ac[,1], c(0.025, 0.975))[2], 3)
res_c_ac_mean<-mean(res_c_ac[,1])

#repeatability
R_C_activity <-   fish_c_ac / (fish_c_ac + cage_c_ac + res_c_ac)

hist(R_C_activity[,1])
R_act_C<- mean(R_C_activity[,1])
R_Blb_act_C <- round(quantile(R_C_activity[,1], c(0.025, 0.975))[1], 3)
R_Bub_act_C <- round(quantile(R_C_activity[,1], c(0.025, 0.975))[2], 3)

#Activity for control group
fish_t_act <- post_sd_T_2[,grep("zlogactivity", colnames(post_sd_T_2))]^2
cage_t_act <- post_sd_cage_2[ ,grep("zlogactivity", colnames(post_sd_cage_2))]^2
res_t_act <- post_sd_sig_T_2[,grep("b_sigma_zlogactivity_treatmentT", colnames(post_sd_sig_T_2))]^2

#repeatability
R_T_activity <-   fish_t_act / (fish_t_act + cage_t_act + res_t_act)

hist(R_T_activity[,1])
R_T_act <- mean(R_T_activity[,1])
R_Blb_act_T <- round(quantile(R_T_activity[,1], c(0.025, 0.975))[1], 3)
R_Bub_act_T <- round(quantile(R_T_activity[,1], c(0.025, 0.975))[2], 3)

p_value_act_T<-pmcmc(R_T_activity[,1]*100, null = 10, twotail = FALSE) #not significant

############################
# EXPERIMENTAL GROUP
############################
#Boldness for experimental group
fish_e_b <- post_sd_E_2[,grep("zlogboldness", colnames(post_sd_E_2))]^2
cage_e_b <- post_sd_cage_2[ ,grep("zlogboldness", colnames(post_sd_cage_2))]^2
res_e_b <- post_sd_sig_E_2[,grep("b_sigma_zlogboldness_treatmentE", colnames(post_sd_sig_E_2))]^2

cage_e_b_low <- round(quantile(cage_e_b[,1], c(0.025, 0.975))[1], 3)
cage_e_b_high<- round(quantile(cage_e_b[,1], c(0.025, 0.975))[2], 3)
cage_e_b_mean<-mean(cage_e_b[,1])

fish_e_b_low <- round(quantile(fish_e_b[,1], c(0.025, 0.975))[1], 3)
fish_e_b_high<- round(quantile(fish_e_b[,1], c(0.025, 0.975))[2], 3)
fish_e_b_mean<-mean(fish_e_b[,1])

res_e_b_low <- round(quantile(res_e_b[,1], c(0.025, 0.975))[1], 3)
res_e_b_high <- round(quantile(res_e_b[,1], c(0.025, 0.975))[2], 3)
res_e_b_mean<-mean(res_e_b[,1])

#repeatability
R_E_boldness <-   fish_e_b / (fish_e_b + cage_e_b + res_e_b)

hist(R_E_boldness[,1])
R_E_bold<-mean(R_E_boldness[,1])
R_Blb_bold_E <- round(quantile(R_E_boldness[,1], c(0.025, 0.975))[1], 3)
R_Bub_bold_E <- round(quantile(R_E_boldness[,1], c(0.025, 0.975))[2], 3)

#exploration for experimental group
fish_e_ex <- post_sd_E_2[,grep("zexploration", colnames(post_sd_E_2))]^2
cage_e_ex <- post_sd_cage_2[ ,grep("zexploration", colnames(post_sd_cage_2))]^2
res_e_ex <- post_sd_sig_E_2[,grep("b_sigma_zexploration_treatmentE", colnames(post_sd_sig_E_2))]^2

fish_e_ex_low <- round(quantile(fish_e_ex[,1], c(0.025, 0.975))[1], 3)
fish_e_ex_high<- round(quantile(fish_e_ex[,1], c(0.025, 0.975))[2], 3)
fish_e_ex_mean<-mean(fish_e_ex[,1])

res_e_ex_low <- round(quantile(res_e_ex[,1], c(0.025, 0.975))[1], 3)
res_e_ex_high <- round(quantile(res_e_ex[,1], c(0.025, 0.975))[2], 3)
res_e_ex_mean<-mean(res_e_ex[,1])

#repeatability
R_E_exploration <-   fish_e_ex / (fish_e_ex + cage_e_ex + res_e_ex)

hist(R_E_exploration[,1])
R_exp_E_mean<-mean(R_E_exploration[,1])
R_Blb_exp_E <- round(quantile(R_E_exploration[,1], c(0.025, 0.975))[1], 3)
R_Bub_exp_E <- round(quantile(R_E_exploration[,1], c(0.025, 0.975))[2], 3)

#activity for experimental group
fish_e_ac <- post_sd_E_2[,grep("zlogactivity", colnames(post_sd_E_2))]^2
cage_e_ac <- post_sd_cage_2[ ,grep("zlogactivity", colnames(post_sd_cage_2))]^2
res_e_ac <- post_sd_sig_E_2[,grep("b_sigma_zlogactivity_treatmentE", colnames(post_sd_sig_E_2))]^2

fish_e_ac_low <- round(quantile(fish_e_ac[,1], c(0.025, 0.975))[1], 3)
fish_e_ac_high<- round(quantile(fish_e_ac[,1], c(0.025, 0.975))[2], 3)
fish_e_ac_mean<-mean(fish_e_ac[,1])

res_e_ac_low <- round(quantile(res_e_ac[,1], c(0.025, 0.975))[1], 3)
res_e_ac_high <- round(quantile(res_e_ac[,1], c(0.025, 0.975))[2], 3)
res_e_ac_mean<-mean(res_e_ac[,1])

#repeatability
R_E_activity <-   fish_e_ac / (fish_e_ac + cage_e_ac + res_e_ac)

hist(R_E_activity[,1])
R_act_E<-mean(R_E_activity[,1])
R_Blb_act_E <- round(quantile(R_E_activity[,1], c(0.025, 0.975))[1], 3)
R_Bub_act_E <- round(quantile(R_E_activity[,1], c(0.025, 0.975))[2], 3)

#############################
# Overall Repeatability
#############################
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

#################################
# Compare R in E and C treatments
#################################
#Is the repeatability different before and after treatment ? 

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

#Boldness control
p_value_bold_C<-pmcmc(R_C_boldness[,1]*100, null = 10, twotail = FALSE)#not significative
#Boldness experimental
p_value_bold_E<-pmcmc(R_E_boldness[,1]*100, null = 10, twotail = FALSE)#not significative
#Average boldness
p_value_bold_a<-pmcmc(R_bold*100, null = 10, twotail = FALSE)#not significative

#Exploration control
p_value_exp_C<-pmcmc(R_C_exploration[,1]*100, null = 10, twotail = FALSE)#significative

hist(R_C_exploration[,1])
hist(R_E_exploration[,1])
#Exploration experimental
p_value_exp_E<-pmcmc(R_E_exploration[,1]*100, null = 10, twotail = FALSE)#not significative
#Average exploration
p_value_exp_a<-pmcmc(R_exp*100, null = 10, twotail = FALSE)#not significative

#Activity control
p_value_act_C<-pmcmc(R_C_activity[,1]*100, null = 10, twotail = FALSE)##significative
hist(R_C_activity[,1])
#Activity experimental
p_value_act_E<-pmcmc(R_E_activity[,1]*100, null = 10, twotail = FALSE)#significative
#Activity average
p_value_act_a<-pmcmc(R_act*100, null = 10,twotail = FALSE) #significative


#################################
# Behavioural Syndromes
#################################
# Extract correlations from model 2
post_cor  <- as_draws_df(model2, variable = "^cor", regex= TRUE)
head(post_cor)

# Extract the C and E groups separately
cor_bold_treatC <-   post_cor[,grep("treatmentC__[a-z]*_treatmentC", colnames(post_cor))]
cor_bold_treatE <-   post_cor[,grep("treatmentE__[a-z]*_treatmentE", colnames(post_cor))]

# See if syndromes differ in C and E

#Boldness and activity
#Control
bsyn_b_act_C <- data.frame(cor_bold_treatC[,1])
b_act_C_mean<-mean(bsyn_b_act_C[,1])
low_b_act_C<-quantile(bsyn_b_act_C[,1], 0.025)
high_b_act_C<-quantile(bsyn_b_act_C[,1], 0.975)
blb_bul_b_act_C<-quantile(bsyn_b_act_C[,1], c(0.025, 0.975))

#Experimental                          
bsyn_b_act_E <- data.frame(cor_bold_treatE[,1])
b_act_E_mean<-mean(bsyn_b_act_E[,1])
blb_bul_b_act_E<-quantile(bsyn_b_act_E[,1], c(0.025, 0.975))
low_b_act_E<-quantile(bsyn_b_act_E[,1], 0.025)
high_b_act_E<-quantile(bsyn_b_act_E[,1], 0.975)

#is the difference significant
pmcmc_b_act<-pmcmc(bsyn_b_act_C[,1] - bsyn_b_act_E[,1], twotail = TRUE)

#Boldness and exploration
#Control
bsyn_b_exp_C <- data.frame(cor_bold_treatC[,2])
b_exp_C_mean<-mean(bsyn_b_exp_C[,1])
blb_bul_b_exp_C<-quantile(bsyn_b_exp_C[,1], c(0.025, 0.975))
low_b_exp_C<-quantile(bsyn_b_exp_C[,1], 0.025)
high_b_exp_C<-quantile(bsyn_b_exp_C[,1], 0.975)

#Experimental
bsyn_b_exp_E <- data.frame(cor_bold_treatE[,2])
b_exp_E_mean<-mean(bsyn_b_exp_E[,1])
blb_bul_b_exp_E<-quantile(bsyn_b_exp_E[,1], c(0.025, 0.975))
low_b_exp_E<-quantile(bsyn_b_exp_E[,1], 0.025)
high_b_exp_E<-quantile(bsyn_b_exp_E[,1], 0.975)

#is the difference significant
pmcmc_b_exp<-pmcmc(bsyn_b_exp_C[,1] - bsyn_b_exp_E[,1], twotail = TRUE)

#Activity and exploration
#Control
bsyn_act_exp_C <- data.frame(cor_bold_treatC[,3])
act_exp_C_mean<-mean(bsyn_act_exp_C[,1])
blb_bul_act_exp_C<-quantile(bsyn_act_exp_C[,1], c(0.025, 0.975))
low_act_exp_C<-quantile(bsyn_act_exp_C[,1], 0.025)
high_act_exp_C<-quantile(bsyn_act_exp_C[,1], 0.975)

#Experimental
bsyn_act_exp_E <- data.frame(cor_bold_treatE[,3])
act_exp_E_mean<-mean(bsyn_act_exp_E[,1])
blb_bul_act_exp_E<-quantile(bsyn_act_exp_E[,1], c(0.025, 0.975))  
low_act_exp_E<-quantile(bsyn_act_exp_E[,1], 0.025)
high_act_exp_E<-quantile(bsyn_act_exp_E[,1], 0.975)

#is the difference significant
pmcmc_act_exp<-pmcmc(bsyn_act_exp_E[,1] - bsyn_act_exp_C[,1], twotail = TRUE)#yes

# Overall syndrome
# Take average of the two treatments
bsyn_b_act   <- cbind(bsyn_b_act_C[,1], bsyn_b_act_E[,1]); mean(rowMeans(bsyn_b_act)); quantile(rowMeans(bsyn_b_act), c(0.025, 0.975))

# Pool the two MCMC chains from the treatments into a giant vector 32,000 rows (2*16,000)
#Boldness and activity
bsyn_b_act2  <- rbind(c(bsyn_b_act_C[,1], bsyn_b_act_E[,1]))
b_act_o<-mean(bsyn_b_act2)
q_b_act<-quantile(bsyn_b_act2, c(0.025, 0.975))

low_b_act_o<-quantile(bsyn_b_act2, 0.025)
high_b_act_o<-quantile(bsyn_b_act2, 0.975)

#Boldness and exploration
bsyn_b_exp  <- rbind(c(bsyn_b_exp_C[,1], bsyn_b_exp_E[,1]))
b_exp_o<-mean(bsyn_b_exp)
q_b_exp<-quantile(bsyn_b_exp, c(0.025, 0.975))

low_b_exp_o<-quantile(bsyn_b_exp, 0.025)
high_b_exp_o<-quantile(bsyn_b_exp, 0.975)

#Activity and exploration
bsyn_act_exp  <- rbind(c(bsyn_act_exp_C[,1], bsyn_act_exp_E[,1]))
act_exp_o<-mean(bsyn_act_exp)
q_act_exp<-quantile(bsyn_act_exp, c(0.025, 0.975))

low_act_exp_o<-quantile(bsyn_act_exp, 0.025)
high_act_exp_o<-quantile(bsyn_act_exp, 0.975)
