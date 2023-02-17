#############################IMPORT RAW DATA
all_data <- read.table("./data_raw/all_data.csv",header=T, sep=";")
all_data

#############################Checking out the data
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

#############################EXPLORE THE DATA WITH GRAPH
#############################INSTALL PACKAGES
install.packages("ggplot2")
library(ggplot2)

#############################


### BS distribution
hist(all_data$BS_post_tot)

min(all_data$BS_post_tot)
max(all_data$BS_post_tot)
median(all_data$BS_post_tot)

### Cestodes distribution
hist(all_data$P04_alive)

min(all_data$P04_alive)
max(all_data$P04_alive)
median(all_data$P04_alive)
mean(all_data$P04_alive)

### Nematodes distribution
hist(all_data$P013_alive)

min(all_data$P013_alive)
max(all_data$P013_alive)

### BS gained after infection

all_data$BS_gain<- all_data$BS_post_tot - all_data$BS_pre
all_data

hist(all_data$BS_gain)
min(all_data$BS_gain) ### miscount, some fish have less BS after infection
max(all_data$BS_gain)
median(all_data$BS_gain)
mean(all_data$BS_gain)

### relationship between treatment (cage vs control) and number of parasites
boxplot(all_data$P04_alive ~ as.factor(all_data$cage))
boxplot(all_data$BS_post_tot ~ as.factor(all_data$cage))
boxplot(all_data$BS_pre ~ as.factor(all_data$cage))
###

### relationship between different parasites
plot(all_data$P04_tot ~ all_data$BS_post_tot) #cestode alive + BS after infection

###correlation between traits
ggplot(all_data, aes(x=exploration, y= boldness)) +
  geom_point() #relation positive ? 

ggplot(all_data, aes(x=exploration, y= activity)) +
  geom_point() #more explorative, more active 

ggplot(all_data, aes(x=activity, y= boldness)) +
  geom_point() #maybe a small positive relationship

###Exploration for the different trials
ggplot(data = all_data,
       aes(x = factor(trial),
           y = exploration))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)+
  stat_summary(fun = mean, shape = 13, size = 1, colour = "red")

#Activity for the different trials
ggplot(data = all_data,
       aes(x = factor(trial),
           y = activity))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)+
  stat_summary(fun = mean, shape = 13, size = 1, colour = "red")

#Boldness for the different trials
ggplot(data = all_data,
       aes(x = factor(trial),
           y = boldness))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)+
  stat_summary(fun = mean, shape = 13, size = 1, colour = "red")

#############################DATA PREP, CREATION OF DIFFERENT VARIABLES
###Create a new columm with total cestode (alive + dead)

all_data$P04_tot<-all_data$P04_alive + all_data$P04_dead
all_data$P04_tot #we have all cestodes together, dead or alive, to know their weight 

###Create a new columm with total nématode (alive + dead)
### We might not use this parasite in the analysis since there were not abundant, but they are heavy so need it for adjusted mass

all_data$P013_tot <- all_data$P013_alive + all_data$P013_dead
all_data$P013_tot

###Add new colum in dataset for parasite loads (BS post infection + cestodes alive combined)
all_data$parasite_load<- (all_data$BS_post_tot + (all_data$P04_alive + all_data$P06))
all_data$parasite_load #this will be useful later in the models

###Calculate adjusted fish mass (total fish mass - parasite mass)
###Mean average for adult cestodes: 0.003g, larval form: 0.0008g, nematode ___g
## P04_tot*0.003 / P06*0008 / P013_tot* _____ (need this info from Marie)

###I can't calculate the adjusted mass for when they arrive in the lab and before the caging experiment, since we don't know their internal parasite loads yet

###Adjusted mass after caging experiment
all_data$adj_mass3<-(all_data$mass_3-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

###Adjusted mass before sacrifice
all_data$adj_mass4<-(all_data$mass_4-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

#############################BODY CONDITION
#############################FULTON INDEX : adjusted mass/standard length3 in CM (*0.1 to change mm to cm)
###Body condition when first arrived at the lab (normal fish mass)
all_data$fulton1<-(all_data$mass_1/(all_data$SL_1*0.1)^3)
all_data$fulton1

###Body condition before caging experiment (normal fish mass, we assume they don't have any parasite after the anti parasite treatment)
all_data$fulton2<-(all_data$mass_2/(all_data$SL_2*0.1)^3)
all_data$fulton2

###Body condition after caging experiement
all_data$fulton3<-(all_data$adj_mass3/(all_data$SL_3*0.1)^3)
all_data$fulton3

###Body condition before sacrifice
all_data$fulton4<-(all_data$adj_mass4/(all_data$SL_4*0.1)^3)
all_data$fulton4

#############################DATA DISTRIBUTION & NORMALITY
###Let's look at the data distribution for our response variables (boldness, exploration and activity) for both treatment group

hist(all_data$exploration)
hist(all_data$activity)
hist(as.numeric(all_data$boldness))

# Distribution of exploration for group C and E
ggplot(all_data, aes(x = exploration)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# Density plot
ggplot(all_data, aes(x=exploration)) + geom_density(color="blue")

# Distribution of activity for group C and E
ggplot(all_data, aes(x = activity)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# Density plot
ggplot(all_data, aes(x=activity)) + geom_density(color="blue")

# Distribution of boldness for group C and E
ggplot(all_data, aes(x = boldness)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# Density plot
ggplot(all_data, aes(x=boldness)) + geom_density(color="blue")


#############################NORMALITY AND TRANSFORMATION
# Exploration
qqnorm(all_data$exploration)
qqline(all_data$exploration)

# Test normality
shapiro.test(all_data$exploration) #not normal, but the distribution was quite normal. I don't know if we should transform it

# Activity
qqnorm(log(all_data$activity))
qqline(log(all_data$activity))

# Test normality
shapiro.test(log(all_data$activity)) ## normality if we transform with log

#Boldness
qqnorm(log(all_data$boldness))
qqline(log(all_data$boldness))

# Test normality
shapiro.test(log(all_data$boldness)) ## we are visually CLOSE to normality with log

#############################CORRELATION BETWEEN TRAITS

# Exploration and activity
cor.test(all_data$exploration, log(all_data$activity), method = c("pearson")) #positive relationship

# Visualize the relationship 
ggplot(all_data, aes(exploration, log(activity)))+
  geom_smooth()+
  geom_point()

# Exploration and boldness
cor.test(all_data$exploration, log(all_data$boldness), method = c("pearson")) #negative, meaning more bold, more explorative

# Visualize the relationship 
ggplot(all_data, aes(exploration, log(boldness)))+
  geom_smooth()+
  geom_point()

geom_raster(aes(exploration, log(activity, log(boldness))))

# Activity and boldness
cor.test(log(all_data$activity), log(all_data$boldness), method = c("pearson"))

# Visualize the relationship 
ggplot(all_data, aes(log(activity), log(boldness)))+
  geom_smooth()+
  geom_point()

############################# MODELS ############ FIRST TRY
# install.packages('pacman') Load Libraries and relevant data
install.packages('pacman')
pacman::p_load(klippy, brms, dplyr, here, flextable, pander)


# Get the raw data. We need this for back transformation from z-scale making
# sure fish_ID is coded as a factor (done automatically before R v4+)
# square-root transforming novel and predator response variables z-scaling all
# variables

### DO I need to scale all my variables? ###
### WHICH body condition do we use in the models ? There is two body condition measure for each group (before trial 1 and 3, after trial 2 and 4)

dat <- read.table("./data_raw/all_data.csv",header=T, sep=";")
dat
dat <- dat %>%
  dplyr::mutate(ID_fish = factor(ID_fish, levels = unique(ID_fish)),
                activity = scale(log(activity)), boldness = scale(log(boldness)),
                exploration = scale(exploration), tank1 = scale(bassin_bold), tank2 = scale(bassin_exp),
                (id = ID_fish))
dat


##################### STEP 1 :
###Using all data (60 fish and 4 measurements / fish) we will fit the following models:
###Model 1: [B, E, A] = u + trtment_{E} + tank + (-1 + trtment_{E}| ID) + (1|Cage)
###Model 2: [B, E, A] = u + trtment_{E} + (-1 + trtment_{E}| ID) + (1|Cage)
###Above models allow us to 
###1) estimate repeatability for ALL traits; 2) estimate the behavioual trait correlations; 3) estimate these within EACH treatment group (C vs E).

###Can't run it. How do I include all traits in the y? ###
### I don't remember what is u ###
### There is two tanks, because one for exploration + activity and one for boldness ###

### Model 1: with tank effect

  boldness_1 <- bf(boldness ~ 1 + treatment + tank1 + (-1 + treatment |q| ID_fish) + (1 | cage)) + gaussian()
  activity_1 <- bf(activity ~ 1 + treatment + tank2 + (-1 + treatment |q| ID_fish) + (1 | cage)) + gaussian()
   explore_1 <- bf(exploration ~ 1 + treatment + tank2 + (-1 + treatment |q| ID_fish) + (1 | cage)) + gaussian()
   
  model1 <- brms::brm(boldness_1 + activity_1 + explore_1, data = dat, iter = 6000, warmup = 2000, chains = 4, cores = 4, 
                      save_pars = save_pars(), file = "./output/models/model1", file_refit = "on_change",
                      control = list(adapt_delta = 0.98))
    
    # Look at the MCMC chains.
    plot(model1)
    
    # Look at the model
    summary(model1)

### Model 2: without tank effect
if (rerun1) {
  formula_personality_shglm_boldness <- bf(boldness ~ u + treatment + (-1 + treatment | id) + (1 | cage))
  brms_personality_shglm_boldness <- brms::brm(formula_personality_shglm_boldness,
                                               data = dat, iter = 6000, warmup = 2000, chains = 3, cores = 3, save_pars = save_pars())
  saveRDS(brms_personality_shglm_boldness, "./Models/brms_models/brms_personality_shglm_boldness")
} else {
  brms_personality_shglm_boldness <- readRDS(here::here("Models/brms_models",
                                                        "brms_personality_shglm_boldness"))
}

summary(brms_personality_shglm_boldness)


##################### STEP 2 :
###Subset the experimental and control fish into two datasets (60 fish and 4 measurements for each C and E group) then fit the following models:
###Model 1 (Experimental Group): [B, E, A] = u + z_body_condition + z_parasite_load + tank + (1 | ID) + + (1|Cage)
###Model 2 (Experimental Group): [B, E, A] = u + z_body_condition + z_parasite_load + z_parasite_load^2 + tank + (1 | ID) + + (1|Cage)
###Model 3 (Control Group): [B, E, A] = u + z_body_condition + tank + (1 | ID) + (1|Cage)

### Which body condition we use ? I have 4 measures ###

### Subset of dataset into two groups
# Experimental group
dat_E <-subset(dat, treatment == "E")
dat_E

# Control group
dat_C <-subset(dat, treatment == "C")
dat_C

### Model 1: experimental group
if (rerun1) {
  formula_personality_shglm_boldness <- bf(boldness ~ u + z_body_condition + z_parasite_load + tank1 + tank2 + (1 | id) + (1 | cage))
  brms_personality_shglm_boldness <- brms::brm(formula_personality_shglm_boldness,
                                               data = dat_E, iter = 6000, warmup = 2000, chains = 3, cores = 3, save_pars = save_pars())
  saveRDS(brms_personality_shglm_boldness, "./Models/brms_models/brms_personality_shglm_boldness")
} else {
  brms_personality_shglm_boldness <- readRDS(here::here("Models/brms_models",
                                                        "brms_personality_shglm_boldness"))
}

summary(brms_personality_shglm_boldness)

### Model 2: experimental group
if (rerun1) {
  formula_personality_shglm_boldness <- bf(boldness ~ u + z_body_condition + z_parasite_load + z_parasite_load^2 + tank1 + tank2 + (1 | id) + (1 | cage))
  brms_personality_shglm_boldness <- brms::brm(formula_personality_shglm_boldness,
                                               data = dat_E, iter = 6000, warmup = 2000, chains = 3, cores = 3, save_pars = save_pars())
  saveRDS(brms_personality_shglm_boldness, "./Models/brms_models/brms_personality_shglm_boldness")
} else {
  brms_personality_shglm_boldness <- readRDS(here::here("Models/brms_models",
                                                        "brms_personality_shglm_boldness"))
}

summary(brms_personality_shglm_boldness)

### Model 3: control group 
if (rerun1) {
  formula_personality_shglm_boldness <- bf(boldness ~ u + z_body_condition + tank1 + tank2 + (1 | id) + (1 | cage))
  brms_personality_shglm_boldness <- brms::brm(formula_personality_shglm_boldness,
                                               data = dat_C, iter = 6000, warmup = 2000, chains = 3, cores = 3, save_pars = save_pars())
  saveRDS(brms_personality_shglm_boldness, "./Models/brms_models/brms_personality_shglm_boldness")
} else {
  brms_personality_shglm_boldness <- readRDS(here::here("Models/brms_models",
                                                        "brms_personality_shglm_boldness"))
}

summary(brms_personality_shglm_boldness)
