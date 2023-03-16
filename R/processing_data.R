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
