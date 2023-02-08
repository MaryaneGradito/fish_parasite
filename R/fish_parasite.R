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

#############################BODY CONDITION
###Create a new columm with total cestode (alive + dead)

all_data$P04_tot<-all_data$P04_alive + all_data$P04_dead
all_data$P04_tot

###Calculate adjusted fish mass (total fish mass - parasite mass)
###Mean average for adult cestodes: 0.003g, larval form: 0.0008g, nematode ___g

###dont know the parasite mass before the experimental infection###so I can use the adjusted mass only on mass 3 et 4
###Adjusted mass when first arrived at the lab
all_data$adj_mass1<-(all_data$mass_1-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))
all_data$adj_mass1
                
###Adjusted mass before caging experiment
all_data$adj_mass2<-(all_data$mass_2-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))
####

###Adjusted mass after caging experiment
all_data$adj_mass3<-(all_data$mass_3-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

###Adjusted mass before sacrifice
all_data$adj_mass4<-(all_data$mass_4-((0.003*all_data$P04_tot)+(0.0008*all_data$P06)))

#############################FULTON INDEX : adjusted mass/standard length3 in CM
###Body condition when first arrived at the lab
all_data$fulton1<-(all_data$adj_mass1/(all_data$SL_1*0.1)^3)
all_data$fulton1

###Body condition before caging experiment
all_data$fulton2<-(all_data$adj_mass2/(all_data$SL_2*0.1)^3)
all_data$fulton2

###Body condition after caging experiement
all_data$fulton3<-(all_data$adj_mass3/(all_data$SL_3*0.1)^3)
all_data$fulton3

###Body condition before sacrifice
all_data$fulton4<-(all_data$adj_mass4/(all_data$SL_4*0.1)^3)
all_data$fulton4

