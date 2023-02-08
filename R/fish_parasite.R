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

### relationship between treatment (cage vs control) and number of parasites
boxplot(all_data$P04_alive ~ as.factor(all_data$cage))
boxplot(all_data$BS_post_tot ~ as.factor(all_data$cage))
boxplot(all_data$BS_pre ~ as.factor(all_data$cage))
###

### relationship between different parasites
plot(all_data$P04_tot~ all_data$BS_post_tot) #cestode alive + BS after infection

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

