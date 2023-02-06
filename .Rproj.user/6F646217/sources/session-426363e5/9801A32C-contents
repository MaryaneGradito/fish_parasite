###SET WORK DIRECTORY

setwd("C:/Users/gradi/Documents/Github/fish_parasite/R")

###IMPORT RAW DATA
all_data <- read.table("all_data.csv",header=T, sep=";")
all_data

###Checking out the data
summary(all_data)
head(all_data)

###
as.factor(all_data$num_fish)
as.factor(all_data$cage)
as.factor(all_data$trial)
as.factor(all_data$bassin_bold)
as.factor(all_data$bassin_exp)
as.factor(all_data$gonade)

###EXPLORE THE DATA WITH GRAPH
plot(all_data$exploration ~ all_data$P04_alive)
plot(all_data$P04 ~ all_data$exploration)
plot(all_data$P04_alive ~ all_data$exploration)
plot(all_data$P04_alive ~ all_data$boldness)

###INSTALL PACKAGES
install.packages("ggplot2")
library(ggplot2)

#############################EXPLORATORY GRAPH 
#Graph exploration trial
ggplot(data = all_data,
       aes(x = factor(trial),
           y = exploration))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)+
  stat_summary(fun = mean, shape = 13, size = 1, colour = "red")

#Graph activity trial
ggplot(data = all_data,
       aes(x = factor(trial),
           y = activity))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)+
  stat_summary(fun = mean, shape = 13, size = 1, colour = "red")

#Graph boldness trial
ggplot(data = all_data,
       aes(x = factor(trial),
           y = boldness))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA) +
  geom_jitter(alpha = 0.2, size = 2, width = 0.1, height = 0)+
  stat_summary(fun = mean, shape = 13, size = 1, colour = "red")

#############################BODY CONDITION
###first arrived at the lab
all_data$fulton1<-(all_data$mass_1/all_data$SL_1)^3
all_data$fulton1

###before caging experiment
all_data$fulton2<-(all_data$mass_2/all_data$SL_2)^3
all_data$fulton2

###after caging experiement
all_data$fulton3<-(all_data$mass_3/all_data$SL_3)^3
all_data$fulton3

###before sacrifice
all_data$fulton4<-(all_data$mass_4/all_data$SL_4)^3
all_data$fulton4