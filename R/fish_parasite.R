#############################
# Import processed data
#############################
  all_data <- read.table("./output/all_data_p.csv",header=T, sep=",")
  all_data
  
#############################
# Checking out the data
#############################

summary(all_data)
head(all_data)
str(all_data)

###
as.factor(all_data$num_fish)
as.factor(all_data$cage)
as.factor(all_data$trial)
as.factor(all_data$bassin_bold)
as.factor(all_data$bassin_exp)
as.factor(all_data$gonade)


#############################
# MODELS STEP 1
#############################

# install.packages
install.packages('pacman')
pacman::p_load(lme4, rstan, StanHeaders, jsonlite, rstantools, brms, Rcpp, dplyr, here, flextable, pander, cmdstanr)


# Get the raw data. We need this for back transformation from z-scale making
# sure fish_ID is coded as a factor

####I'm leaving this here for now, if we need to adjust for our models
dat <- all_data
dat <- dat %>%
  dplyr::mutate(id = factor(ID_fish, levels = unique(ID_fish)),
                log_activity = scale(log(activity)), log_boldness = scale(log(boldness)),
                exploration = scale(exploration), tank1 = as.factor(bassin_bold), tank2 = as.factor(bassin_exp),z_parasite_load = scale(parasite_load),
                (id = ID_fish))

##################### STEP 1 :
###Using all data (60 fish and 4 measurements / fish), we will fit the following models:
###Model 1: [B, E, A] = u + trtment_{E} + tank + (-1 + trtment_{E}| ID) + (1|Cage)
###Model 2: [B, E, A] = u + trtment_{E} + (-1 + trtment_{E}| ID) + (1|Cage)
###Above models allow us to 
###1) estimate repeatability for ALL traits; 2) estimate the behavioural trait correlations; 3) estimate these within EACH treatment group (C vs E).

### Model 1: with tank effect

  boldness_1 <- bf(log_boldness ~ 1 + treatment + tank1 + (-1 + treatment |q| ID_fish) + (1 | cage)) + gaussian()
  activity_1 <- bf(log_activity ~ 1 + treatment + tank2 + (-1 + treatment |q| ID_fish) + (1 | cage)) + gaussian()
   explore_1 <- bf(exploration ~ 1 + treatment + tank2 + (-1 + treatment |q| ID_fish) + (1 | cage)) + gaussian()
   
  model1 <- brms::brm(boldness_1 + activity_1 + explore_1 + set_rescor(TRUE), 
                      data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model1", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))
    
    # Compare models  
  model1 <- add_criterion(model1, c("loo", "waic"))
    
    #To look at the model already runned
  model1 <- readRDS(file = "./output/models/model1.rds")
  model1  
  
  # Look at the MCMC chains.
    plot(model1)
    
    # Look at the model
    summary(model1)

### Model 2: without tank effect

    boldness_2 <- bf(log_boldness ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                     sigma ~ -1 + treatment) + gaussian()
    activity_2 <- bf(log_activity ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                     sigma ~ -1 + treatment) + gaussian()
     explore_2 <- bf(exploration ~ 1 + treatment + (-1 + treatment |q| ID_fish) + (1 | cage),
                     sigma ~ -1 + treatment) + gaussian()
     
     model2 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                         data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                         save_pars = save_pars(), file = "./output/models/model2", file_refit = "on_change",
                         control = list(adapt_delta = 0.98))
     
#rerun doesn't work
if(rerun){
    model2 <- brms::brm(boldness_2 + activity_2 + explore_2 + set_rescor(TRUE), 
                        data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                        save_pars = save_pars(),
                        control = list(adapt_delta = 0.98))
    
    saveRDS(model2, file = "./output/models/model2.rds")
    
} else{
  model2 <- readRDS(file = "./output/models/model2.rds")
}
    model2 <- add_criterion(model2, c("loo", "waic"))
    
    # Look at the MCMC chains.
    plot(model2)
    
    # Look at the model
    summary(model2)
    
# Calculate repeatability
    post_sd <- as_draws_df(model2, variable = "^sd", regex = TRUE)
    post_sd_C <- post_sd[,grepl("C", colnames(post_sd))]
    post_sd_E <- post_sd[,grepl("E", colnames(post_sd))]
    post_sd_cage <- post_sd[,grepl("cage", colnames(post_sd))]
    post_sd_sig <- as_draws_df(model2, variable = "^sigma", regex = TRUE)
    
# Repeatability for the traits across treatment
    source("./R/func.R")
      
      R_boldness <- repeatability(post_sd_C, post_sd_E, post_sd_cage, post_sd_sig, trait = "logboldness")
       R_explore <- repeatability(post_sd_C, post_sd_E, post_sd_cage, post_sd_sig, trait = "exploration")
       R_activity <- repeatability(post_sd_C, post_sd_E, post_sd_cage, post_sd_sig, trait = "logactivity")
      
       #I don't remember what this is ? 
      R_contrast <- R_explore - R_boldness
      

#############################
# MODELS STEP 2
#############################
    
##################### STEP 2 :
###Subset the experimental and control fish into two datasets (60 fish and 4 measurements for each C and E group) then fit the following models:
###Model 1 (Experimental Group): [B, E, A] = u + z_body_condition + z_parasite_load + tank + (1 | ID) + + (1|Cage)
###Model 2 (Experimental Group): [B, E, A] = u + z_body_condition + z_parasite_load + z_parasite_load^2 + tank + (1 | ID) + + (1|Cage)
###Model 3 (Control Group): [B, E, A] = u + z_body_condition + tank + (1 | ID) + (1|Cage)

### Subset of dataset into two groups
# Experimental group
dat_E <-subset(dat, treatment == "E")
dat_E

# Control group
dat_C <-subset(dat, treatment == "C")
dat_C

### Model 1: experimental group

boldness_E1 <- bf(log_boldness ~ 1 + z_parasite_load + tank1 + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_E1 <- bf(log_activity ~ 1 + z_parasite_load + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_E1 <- bf(exploration ~ 1 + z_parasite_load + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()

model_E1 <- brms::brm(boldness_E1 + activity_E1 + explore_E1 + set_rescor(TRUE), 
                      data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./output/models/model_E1", file_refit = "on_change",
                   control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_E1)

# Look at the model
summary(model_E1)

boldness_E1 <- bf(log_boldness ~ 1 + z_parasite_load + tank1 + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_E1 <- bf(log_activity ~ 1 + z_parasite_load + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_E1 <- bf(exploration ~ 1 + z_parasite_load + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()

model_E1 <- brms::brm(boldness_E1 + activity_E1 + explore_E1 + set_rescor(TRUE), data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                    save_pars = save_pars(), file = "./output/models/model_E1", file_refit = "on_change",
                   control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_E1)

# Look at the model
summary(model_E1)

### Model 2: experimental group

boldness_E2 <- bf(log_boldness ~ 1 + z_parasite_load + z_parasite_load^2 + tank1 + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_E2 <- bf(log_activity ~ 1 + z_parasite_load + z_parasite_load^2 + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_E2 <- bf(exploration ~ 1 + z_parasite_load + z_parasite_load^2 + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()

model_E2 <- brms::brm(boldness_E2 + activity_E2 + explore_E2 + set_rescor(TRUE), data = dat_E, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_E2", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_E2)

# Look at the model
summary(model_E2)

### Model 3: control group 

boldness_C <- bf(log_boldness ~ 1  + tank1 + (1 | ID_fish) + (1 | cage)) + gaussian()
activity_C <- bf(log_activity ~ 1  + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()
explore_C <- bf(exploration ~ 1  + tank2 + (1 | ID_fish) + (1 | cage)) + gaussian()

model_C <- brms::brm(boldness_C + activity_C + explore_C + set_rescor(TRUE), data = dat_C, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model_C", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_C)

# Look at the model
summary(model_C)

### Model 4: body condition as a response variable

body_condition <- bf(z_body_condition ~ 1  + (1 | ID_fish) + (1 | cage)) + gaussian()


model_BC <- brms::brm(boldness_C + activity_C + explore_C, data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                     save_pars = save_pars(), file = "./output/models/model_BC", file_refit = "on_change",
                     control = list(adapt_delta = 0.98))

# Look at the MCMC chains.
plot(model_BC)

# Look at the model
summary(model_BC)


#############################
# MODELS STEP 3
#############################
#looking at different parasites that could possibly be in competition