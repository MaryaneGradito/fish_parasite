#############################
# Import raw data
#############################
all_data <- read.table("./data_raw/all_data.csv",header=T, sep=";")
all_data

#############################
# Packages
#############################
library(ggplot2)
library(dplyr)
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
# Explore the data
#############################
# Need to install this package for the graph
install.packages("ggplot2")
library(ggplot2)

### Blackspots distribution
hist(all_data$BS_post_tot)

# Blackspots info
min(all_data$BS_post_tot)
max(all_data$BS_post_tot)
median(all_data$BS_post_tot)

### Cestodes distribution
hist(all_data$P04_alive)

# Cestodes info
min(all_data$P04_alive)
max(all_data$P04_alive)
median(all_data$P04_alive)
mean(all_data$P04_alive)

### Nematodes distribution
hist(all_data$P013_alive)

# Nematodes info
min(all_data$P013_alive)
max(all_data$P013_alive)

### Looking at BS gained after infection
# Create new columns in the dataset
all_data$BS_gain<- all_data$BS_post_tot - all_data$BS_pre

# BS gain distribution
hist(all_data$BS_gain)

# BS gain info
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

###Visual correlation between traits
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


#############################
# Data processing
#############################
###Create a new column with total cestode (alive + dead)

all_data$P04_tot<-all_data$P04_alive + all_data$P04_dead
all_data$P04_tot #we have all cestodes together, dead or alive, to know their weight 

###Add new column in dataset for parasite loads (BS post infection + cestodes alive combined)
all_data$parasite_load<- (all_data$BS_post_tot + (all_data$P04_alive + all_data$P06))
all_data$parasite_load #this will be useful later in the models

###Calculate adjusted fish mass (total fish mass - parasite mass)
###Mean average for adult cestodes: 0.003g, larval form: 0.0008g, nematode ___g
## P04_tot*0.003 / P06*0008
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

#############################
# Data distribution and normality
#############################
###Let's look at the data distribution for our response variables (boldness, exploration and activity) for both treatment group

### Distribution for all fish
hist(all_data$exploration)
hist(all_data$activity)
hist(as.numeric(all_data$boldness))

# Distribution of exploration for group C and E
ggplot(all_data, aes(x = exploration)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# Distribution of activity for group C and E
ggplot(all_data, aes(x = activity)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

# Distribution of boldness for group C and E
ggplot(all_data, aes(x = boldness)) +
  geom_histogram(aes(color = treatment, fill = treatment), 
                 position = "identity", bins = 30, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

#NORMALITY AND TRANSFORMATION
# Exploration
qqnorm(all_data$exploration)
qqline(all_data$exploration) #looks normal

shapiro.test(all_data$exploration) #not normal but visually yes

# Activity
qqnorm(log(all_data$activity))
qqline(log(all_data$activity)) #looks normal with log transformation

# Test normality
shapiro.test(log(all_data$activity)) ## normality if we transform with log

#Boldness
qqnorm(log(all_data$boldness))
qqline(log(all_data$boldness))

# Test normality
shapiro.test(log(all_data$boldness)) ## we are visually CLOSE to normality with log


#############################
# Last processing + save the new dataset
#############################
# Get the raw data. We need this for back transformation from z-scale making
# sure fish_ID is coded as a factor

dat <- all_data
dat <- dat %>%
  dplyr::mutate(id = factor(ID_fish, levels = unique(ID_fish)),
                log_activity = scale(log(activity)), log_boldness = scale(log(boldness)),
                exploration = scale(exploration), tank1 = as.factor(bassin_bold), tank2 = as.factor(bassin_exp),(id = ID_fish))

###Save the new dataset with processed data
write.table(dat, file = "all_data_p.csv",
            sep = ",", row.names = F)

#data processing for models
#pivot data for each trial
#we need parasite load z-transform and body condition z-transform for the next model
all_dat <- read.table("./output/all_data_p.csv",header=T, sep=",")

dat_trial1<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration, fulton1, parasite_load) %>% 
  filter(trial == 1) %>% 
  pivot_longer("fulton1",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial2<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,fulton2, parasite_load) %>%
  filter(trial == 2) %>% 
  pivot_longer("fulton2",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial3<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,fulton3, parasite_load) %>%
  filter(trial == 3) %>% 
  pivot_longer("fulton3",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial4<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration, fulton4, parasite_load) %>%
  filter(trial == 4) %>% 
  pivot_longer("fulton4",
               values_to='body_condition') %>% arrange(ID_fish)

#rbind together to get dataset for the model
dat_trials <- rbind(dat_trial1, dat_trial2, dat_trial3, dat_trial4) %>% arrange(ID_fish)

#Select for each group the data and scale 
#Experimental group
dat_6 <- dat_trials %>% filter(treatment == "E") %>%  mutate(z_bc = scale(body_condition),
                                                            z_pl = scale(parasite_load))
#Control group
dat_5 <- dat_trials %>% filter(treatment == "C") %>% mutate(z_bc = scale(body_condition))

#Save data
write.table(dat_6, file = "dat_models_E.csv",
            sep = ",", row.names = F)

write.table(dat_5, file = "dat_models_C.csv",
            sep = ",", row.names = F)

###Processing data for type of parasites
all_dat <- read.table("./output/all_data_p.csv",header=T, sep=",")
all_dat$ces_tot<- (all_dat$P04_alive + all_data$P06)

dat_trial1<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration, fulton1, parasite_load, BS_post_tot, ces_tot, BS_pre) %>% 
  filter(trial == 1) %>% 
  pivot_longer("fulton1",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial2<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,fulton2, parasite_load, BS_post_tot, ces_tot, BS_pre) %>%
  filter(trial == 2) %>% 
  pivot_longer("fulton2",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial3<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,fulton3, parasite_load, BS_post_tot, ces_tot, BS_pre) %>%
  filter(trial == 3) %>% 
  pivot_longer("fulton3",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial4<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration, fulton4, parasite_load, BS_post_tot, ces_tot, BS_pre) %>%
  filter(trial == 4) %>% 
  pivot_longer("fulton4",
               values_to='body_condition') %>% arrange(ID_fish)

#rbind together to get dataset for the model
dat_trials <- rbind(dat_trial1, dat_trial2, dat_trial3, dat_trial4) %>% arrange(ID_fish)

#Select for experimentally infected fish and scale parasite and body condition
dat_par_type <- dat_trials %>% filter(treatment == "E") %>%  mutate(z_bc = scale(body_condition),
                                                             z_pl = scale(parasite_load),
                                                             z_bs = scale(BS_post_tot),
                                                            z_ces = scale (ces_tot),
                                                            z_bs_pre = scale(BS_pre))
#Save data
write.table(dat_par_type, file = "dat_models_parasite.csv",
            sep = ",", row.names = F)

#############################
# Processing data for model3
#############################
#Create data frame with posterior distribution from model3
#exploration treatment E
test  <- ranef(model3, summary = FALSE)$ID_fish

exploration_treatmentE_blup <-  test[, , "exploration_treatmentE"]
posterior <- as_draws_df(model3, vars = "^b")
exploration_treatmentE_b <- data.frame(posterior[,grep("b_exploration_treatmentE", colnames(posterior))])

fishid_slope <- apply(exploration_treatmentE_blup, 2, function(x) rowSums(exploration_treatmentE_b + x))
fishid_slope_est <- apply(fishid_slope,2, function(x) mean(x))
fishid_slope_se <- apply(fishid_slope,2, function(x) sd(x))
fishid_slope <- cbind(fishid_slope_est,fishid_slope_se)

exp_E<-df(exploration_treatmentE_blup,exploration_treatmentE_b) #function works

#Boldness treatment E
logboldness_treatmentE_blup <-  test[, , "logboldness_treatmentE"]
logboldness_treatmentE_b <- data.frame(posterior[,grep("b_logboldness_treatmentE", colnames(posterior))])

bold_E<-df(logboldness_treatmentE_blup,logboldness_treatmentE_b)

#Activity treatmentE
logactivity_treatmentE_blup <-  test[, , "logactivity_treatmentE"]
logactivity_treatmentE_b <- data.frame(posterior[,grep("b_logactivity_treatmentE", colnames(posterior))])

act_E<-df(logactivity_treatmentE_blup,logactivity_treatmentE_b)


#Bold Intercept
logboldness_Intercept_blup <-  test[, , "logboldness_Intercept"]
logboldness_Intercept_b <- data.frame(posterior[,grep("b_logboldness_Intercept", colnames(posterior))])

bold_I<-df(logboldness_Intercept_blup,logboldness_Intercept_b)

#Exploration Intercept
exploration_Intercept_blup <-  test[, , "exploration_Intercept"]
exploration_Intercept_b <- data.frame(posterior[,grep("b_exploration_Intercept", colnames(posterior))])

exp_I<-df(exploration_Intercept_blup,exploration_Intercept_b)

#Activity Intercept
logactivity_Intercept_blup <-  test[, , "logactivity_Intercept"]
logactivity_Intercept_b <- data.frame(posterior[,grep("b_logactivity_Intercept", colnames(posterior))])

act_I<-df(logactivity_Intercept_blup,logactivity_Intercept_b)

#combine data
data_slopes<-cbind(bold_E,exp_E,act_E,bold_I,exp_I,act_I)
dat_slopes<-as.data.frame(data_slopes)
dat_slopes$ID_fish <- rownames(dat_slopes)

# assigning new names to the columns of the data frame
colnames(dat_slopes) <- c('bold_E','se_b_E','exp_E','se_exp_E','act_E','se_act_E','bold_I','se_b_I','exp_I','se_exp_I','act_I','se_act_I')

write.table(dat_slopes, file = "dat_slopes.csv",
            sep = ",", row.names = F)

#############################
# Processing data for model 10 
#############################
#New dataframe for model 10 
#random slope
#i need a new data set with the change for parasite load before and after infection,
# change of bc (first and last, before and after infection)
#60 fish (only the ones who did the infection)

all_dat <- read.table("./output/all_data_p.csv",header=T, sep=",")
all_dat$ces_tot<- (all_dat$P04_alive + all_data$P06)
all_dat$change_bc<- (all_dat$fulton2- all_dat$fulton4)
all_dat$change_bs<-(all_dat$BS_post_tot - all_dat$BS_pre)

dat_trial1<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration, fulton1, parasite_load, BS_post_tot, ces_tot, change_bc, change_bs) %>% 
  filter(trial == 1) %>% 
  pivot_longer("fulton1",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial2<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,fulton2, parasite_load, BS_post_tot, ces_tot, change_bc, change_bs) %>%
  filter(trial == 2) %>% 
  pivot_longer("fulton2",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial3<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration,fulton3, parasite_load, BS_post_tot, ces_tot, change_bc, change_bs) %>%
  filter(trial == 3) %>% 
  pivot_longer("fulton3",
               values_to='body_condition') %>% arrange(ID_fish)

dat_trial4<-all_dat  %>% 
  select(ID_fish, trial, cage, treatment, log_boldness, log_activity, exploration, fulton4, parasite_load, BS_post_tot, ces_tot, change_bc, change_bs) %>%
  filter(trial == 4) %>% 
  pivot_longer("fulton4",
               values_to='body_condition') %>% arrange(ID_fish)

#rbind together to get dataset for the model
dat_trials <- rbind(dat_trial1, dat_trial2, dat_trial3, dat_trial4) %>% arrange(ID_fish)

#Select for experimentally infected fish and scale parasite and body condition
dat_par_type <- dat_trials %>% filter(treatment == "E" & trial == 4) %>%  mutate(z_bc = scale(body_condition),
                                                                                 z_pl = scale(parasite_load),
                                                                                 z_bs = scale(BS_post_tot),
                                                                                 z_ces = scale (ces_tot),
                                                                                 z_change_bc = scale(change_bc), z_change_bs = scale(change_bs))

#Save data
write.table(dat_par_type, file = "new_data.csv",
            sep = ",", row.names = F)

dat_change <- read.table("./output/new_data.csv",header=T, sep=",")

#merge slopes and change in bs and bc
new_data<-merge(dat_slopes, dat_change, by = "ID_fish")

write.table(new_data, file = "new_data1.csv",
            sep = ",", row.names = F)





