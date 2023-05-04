#Code Maryane - carto :)
#Script de Juliane Vigneault

# ----- Loading packages ----- #
install.packages("sf")
library(sf)
library(dplyr)
install.packages("measurements")
library(measurements)
library(ggplot2)
library(stringr)

# ----- Loading data ----- #

CombinedData <- read.csv("./map/cromwell.csv",header=T, sep=";") # ---- Étape 1) Load ton fichier de données avec tes coordonnées GPS

## Attribute table ##
# ---- Étape 2) Sélection les variables d'intérêt dans ton fichier
attributes <- CombinedData %>% 
  select_("cage", "lat", "long")

# Coordinates conversion #
# ---- Étape 3) Les coordonnées que tu veux représenter doivent être en format degré décimale, alors ça se peut que tu ailles à convertir tes données de terrain
attributes$lat <- str_replace(attributes$lat, "�", " ")
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
CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "lightblue") + #Shape du lac
  geom_point(data = attributes, aes(x = long, y = lat, color = cage_number), size = 3.5) #Points GPS de tes cages
  #geom_text(data = attributes, aes(x = long, y = lat), size = 2) + #Si tu veux ajouter des étiquettes à tes points
  theme_void() #Pour mettre sur fond blanc plutôt qu'un graphique
CROM.plot
