#Code Maryane - carto :)
#Script de Juliane Vigneault

# ----- Loading packages ----- #
library(sf)
library(dplyr)
library(measurements)
library(ggplot2)
library(stringr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(ggspatial)
library(magick)

# ----- Loading data ----- #

CombinedData <- read.csv("./map/cromwell.csv",header=T, sep=";",stringsAsFactors=FALSE, fileEncoding="latin1")

 # ---- Étape 1) Load ton fichier de données avec tes coordonnées GPS

## Attribute table ##
# ---- Étape 2) Sélection les variables d'intérêt dans ton fichier
attributes <- CombinedData %>% 
  select_("cage", "lat", "long", "dens_tot","dens_ces","dens_bs")

# Coordinates conversion #
# ---- Étape 3) Les coordonnées que tu veux représenter doivent être en format degré décimale, alors ça se peut que tu ailles à convertir tes données de terrain
attributes$lat <- str_replace(attributes$lat, "°", " ")
attributes$lat <- str_remove(attributes$lat, "'")
attributes$lat <- conv_unit(attributes$lat, from = "deg_dec_min", to = "dec_deg")
attributes$lat <- as.numeric(attributes$lat)

attributes$long <- str_replace(attributes$long, "°", " ")
attributes$long <- str_remove(attributes$long, "'")
attributes$long <- conv_unit(attributes$long, from = "deg_dec_min", to = "dec_deg")
attributes$long <- as.numeric(attributes$long)*(-1) #Add negative sign as coordinates are from western hemisphere

# Cromwell #
# ---- Étape 4) Load la shape du lac. Pour que tu puisses importer le fichier, tu dois avoir dans ton directory les 4 fichiers que je t'ai envoyé
CROM <- st_read("./map/Cromwell.shp")

#CROM.att <- attributes %>% filter(Lake == "Cromwell")

cage_number<-as.factor(attributes$cage)
# ---- Étape 5) Représentation visuelle
# Specify the number of decimal places in the labels

#map for black spot density
CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "#DFEFFE", color = "black")+
scale_x_continuous(breaks=c(-74.002,-73.998, -73.994),labels = function(x) paste0(format(x, digit = 5), "\u00B0","W")) +
scale_y_continuous(breaks=c(45.988,45.989, 45.990),labels = function(y) paste0(format(y, digit = 5), "\u00B0","N")) +
  geom_point(data = attributes, aes(x = long, y = lat, color = dens_bs), size = 9)+ #Points GPS de tes cages
  geom_text(data = attributes, aes(x = long, y = lat, label = cage), size = 7) +
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
  theme_classic() +
  labs(title = "", 
       subtitle = "") +
  xlab("") + ylab("")

CROM.plot<-CROM.plot + theme(plot.title = element_text(size=14, face="bold")) + theme(panel.background = element_rect(fill = '#F7F7F7', color = 'black')) + scale_colour_gradient(low = "#FEFFD0",high = "#B80A0A")

p2<-CROM.plot +annotation_scale(location = "bl", bar_cols = c("black","white")) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_nautical(fill =c("black","white"),line_col = "black")) + theme(legend.position = c(0.88, 0.40), legend.background = element_rect(fill = "#F7F7F7", color = "black"), legend.key.size = unit(0.8, 'cm'), legend.title = element_text(size=15, face = "bold"), legend.text = element_text(size=12), plot.margin=unit(c(-0.5,1,1,1), "cm")) + labs(color = "Mean \n cestode \n density (no/g) \n per cage")


p<-grid.arrange(p1,p2)



ggsave("./output/figures/map.png", plot = p, width = 40, height = 30, units = "cm")

map <- image_read("./output/figures/map.png")
