#Import dataset
setwd("/Users/arianecote/Documents/Logiciel R/C2")
Chem_C2<-read.csv2("DATA_C2_Chemical_test.csv", na.strings = c("NA"), header = TRUE) #na.strings to interpret character as NA values
Shoal_C2<-read.csv2("DATA_C2_StimuliShoal.csv", na.strings = c("NA"), header = TRUE)

#Packages to install
library(lme4)
library(glmmTMB)
require(AICcmodavg)
library(rptR)
library(MASS)
require(ggplot2)
require(car) # for vif(), Anova(), qqplot()
require(vegan) # for varpart()
library(tidyr)
library(dplyr)
library(magrittr)
library(tidyr)
library(dplyr)

#Dataset exploration
summary(Chem_C2)
head(Chem_C2)
summary(Shoal_C2)
head(Shoal_C2)

#add variables to dataset

#Ratio M:F for each shoal
Ratio1<-length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="1" & Shoal_C2$Sex=="M"]))/length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="1" & Shoal_C2$Sex=="F"]))
Ratio2<-length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="2" & Shoal_C2$Sex=="M"]))/length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="2" & Shoal_C2$Sex=="F"]))
Ratio3<-length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="3" & Shoal_C2$Sex=="M"]))/length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="3" & Shoal_C2$Sex=="F"]))
Ratio4<-length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="4" & Shoal_C2$Sex=="M"]))/length(na.omit(Shoal_C2$Sex[Shoal_C2$Shoal=="4" & Shoal_C2$Sex=="F"]))

Chem_C2<-
  mutate(Chem_C2, Infected_ratio = (Infected_zone)/(Total_time)) %>% #Ratio of time spent with infected conspecifics
  mutate(Chem_C2, None_ratio = (None_zone)/(Total_time)) %>% #Ratio of time spent with no conspecifics
  mutate(Chem_C2, LeCren = Mass/(a*((SL/10)^b))) #Body condition

Shoal_C2<-
  mutate(Shoal_C2, Shoal_Total_parasite = (BS_tot+BT_tot+Other)) %>% #Total parasites
  mutate(Shoal_C2, Parasite_den = (Shoal_Total_parasite/Mass)) %>% #Parasites density
  mutate(Shoal_C2, Shoal_Parasite_den = case_when(Shoal=="1"~mean(Parasite_den[Shoal=="1"]), Shoal=="2"~mean(Parasite_den[Shoal=="2"]), Shoal=="3"~mean(Parasite_den[Shoal=="3"]), Shoal=="4"~mean(Parasite_den[Shoal=="4"]))) %>% #Mean parasite density for each shoal
  mutate(Shoal_C2, Shoal_SL = case_when(Shoal=="1"~mean(SL[Shoal=="1"]), Shoal=="2"~mean(SL[Shoal=="2"]), Shoal=="3"~mean(SL[Shoal=="3"]), Shoal=="4"~mean(SL[Shoal=="4"]))) %>% #Mean SL for each shoal
  mutate(Shoal_C2, Shoal_Mass = case_when(Shoal=="1"~mean(Mass[Shoal=="1"]), Shoal=="2"~mean(Mass[Shoal=="2"]), Shoal=="3"~mean(Mass[Shoal=="3"]), Shoal=="4"~mean(Mass[Shoal=="4"]))) %>% #Mean mass for each shoal
  mutate(Shoal_C2, LeCren_stimuli = Mass/(a*((SL/10)^b))) %>% #Body condition
  mutate(Shoal_C2, Shoal_LeCren = case_when(Shoal=="1"~mean(LeCren_stimuli[Shoal=="1"]), Shoal=="2"~mean(LeCren_stimuli[Shoal=="2"]), Shoal=="3"~mean(LeCren_stimuli[Shoal=="3"]), Shoal=="4"~mean(LeCren_stimuli[Shoal=="4"]))) %>% #Mean LeCren for each shoal
  mutate(Shoal_C2, Shoal_RatioMF = case_when(Shoal=="1" ~ Ratio1, Shoal=="2" ~ Ratio2, Shoal=="3" ~ Ratio3, Shoal=="4" ~ Ratio4)) #Ratio M:F for each shoal


#Create a data frames with variables of interest
Chem_C2_interest<- select(Chem_C2,-X, -X.1)
Shoal_C2_interest<- select(Shoal_C2, Shoal, Shoal_Parasite_den, Shoal_SL, Shoal_Mass, Shoal_LeCren, Shoal_RatioMF)
#Merge in one data frame
dfC2_chem <- merge(Chem_C2_interest, unique(Shoal_C2_interest, by = 'Shoal'))

#Dataset exploration
head(dfC2_chem)
summary(dfC2_chem)

############################ CORRELATION #############################
#### Visualize explanatory variables to look for collinearity ####

####### Visualize response variables with plots #######


########## Generalized linear mixed model (GLMM) ##########
########## Generalized linear mixed model (GLMM) ##########
########## Generalized linear mixed model (GLMM) ##########

library(glmmTMB)

#Building a model with a beta distribution and logit link function using glmmTMB

#Response variable : proportion of time focal fish spent with infected conspecifics cue (i.e. attraction)
dfC2_chem$Infected_ratio<-as.numeric(dfC2_chem$Infected_ratio)
dfC2_chem$Lake<-as.factor(dfC2_chem$Lake)
dfC2_chem$Trial<-as.factor(dfC2_chem$Trial)
dfC2_chem$SL<-as.numeric(dfC2_chem$SL)
dfC2_chem$Shoal<-as.factor(dfC2_chem$Shoal)
dfC2_chem$Sex<-as.factor(dfC2_chem$Sex)
dfC2_chem$Shoal_RatioMF<-as.numeric(dfC2_chem$Shoal_RatioMF)
dfC2_chem$ID<-as.factor(dfC2_chem$ID)
dfC2_chem$Time_min<-as.numeric(dfC2_chem$Time_min)
dfC2_chem$Time_adj<-(dfC2_chem$Time_min)-500
dfC2_chem$Time_adj<-as.numeric(dfC2_chem$Time_adj)
dfC2_chem$Time_exp<-factor(dfC2_chem$Time_exp, levels = c("9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16"))

#Beta distribution
#Transform response variable so that it fits between 0 and 1 (0 < y < 1)
# Define a small adjustment value that won't affect the analyzes 
adj <- 1e-10
# Add the adjustment value to 0 and subtract it from 1
y<-dfC2_chem$Infected_ratio
y_adj <- ifelse(y == 0, y + adj, ifelse(y == 1, y - adj, y))

#Generalized Linear Mixed Models using Template Model Builder

#We previously tested the model with the explanatory variables sex,shoal sex ratio and shoal body condition, 
#but in order to respect parsimony, these variables were removed from the model since 
#they did not provide a much better fit to the data (in order to justify its increased complexity).

#Complete model
modC2_chem<-glmmTMB(y_adj ~ Lake + Trial + SL + Lake*SL + (1|ID), data=dfC2_chem, beta_family())
summary(modC2_chem)
summary(aov(modC2_chem))

#Confidence intervals for our model (for fixed factors)
library(MASS) #methods are based on profile likelihood since we use a glmmTMB model
confint(modC2_chem, levels =0.95, method = "profile") #warning is normal

#To obtain confidence intervals of variance (for random factor)
#For models of class glmmTMB, confidence intervals for 
#random effect variances use a Wald t-distribution approximation.
confint(modC2_chem, parm = 7, method = "Wald") #parm is the parameter for which you want to compute the confidence interval

## IC95 variance = (IC95 deviance)^2
#lower confidence limit
(0.5702976)^2
#upper confidence limit
(1.305976)^2

######## MODELS TO COMPARE WITH ANOVA (p-value) ################
#ANOVAs to compare models and get the likelihood ratio
#Null model
ModNull<-glmmTMB(y_adj ~ 1, data=dfC2_chem, beta_family())
summary(ModNull)

#Without Lake
ModA<-glmmTMB(y_adj ~ Trial + SL + Lake*SL + (1|ID), data=dfC2_chem, beta_family())
summary(ModA)

#Without Trial
ModB<-glmmTMB(y_adj ~ Lake + SL + Lake*SL + (1|ID), data=dfC2_chem, beta_family())
summary(ModB)

#Without SL 
ModC<-glmmTMB(y_adj ~ Lake + Trial + Lake*SL + (1|ID), data=dfC2_chem, beta_family())
summary(ModC)

#Without interaction
ModD<-glmmTMB(y_adj ~ Lake + Trial + SL + Lake*SL, data=dfC2_chem, beta_family())
summary(ModD)

#Without ID (random factor)
ModE<-glmmTMB(y_adj ~ Lake + Trial + SL + Lake*SL, data=dfC2_chem, beta_family())
summary(ModE)

#Compare all models with the complete model (to have p-value for each variables)
anova(modC2_chem,ModNull)# complete model is significant
anova(modC2_chem,ModA)# complete model is significant (lake)
anova(modC2_chem,ModB)# not significant (trial)
anova(modC2_chem,ModC)# not significant (SL)
anova(modC2_chem,ModD)# not significant (interaction)
anova(modC2_chem,ModE)# not significant (ID)
