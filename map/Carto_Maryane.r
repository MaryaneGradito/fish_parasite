#Code Maryane - carto :)

# ----- R Setup ----- #

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"

# ----- Loading packages ----- #

library(sf)
library(dplyr)
library(measurements)
library(ggplot2)

# ----- Loading data ----- #

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv")) # ---- Étape 1) Load ton fichier de données avec tes coordonnées GPS

#### Intra lake prevalences bubble map #### 
col.pal <- c("chocolate", "goldenrod", "olivedrab")

## Attribute table ##
# ---- Étape 2) Sélection les variables d'intérêt dans ton fichier
attributes <- CombinedData %>% 
  select_("Sampling_ID", "Lake", "Sampling_method", "Latitude", "Longitude", "tot_LeGi", "inf_LeGi") %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") 

# Coordinates conversion #
# ---- Étape 3) Les coordonnées que tu veux représenter doivent être en format degré décimale, alors ça se peut que tu ailles à convertir tes données de terrain
attributes$Latitude <- str_replace(attributes$Latitude, "°", " ")
attributes$Latitude <- str_remove(attributes$Latitude, "'")
attributes$Latitude <- conv_unit(attributes$Latitude, from = "deg_dec_min", to = "dec_deg")
attributes$Latitude <- as.numeric(attributes$Latitude)

attributes$Longitude <- str_replace(attributes$Longitude, "°", " ")
attributes$Longitude <- str_remove(attributes$Longitude, "'")
attributes$Longitude <- conv_unit(attributes$Longitude, from = "deg_dec_min", to = "dec_deg")
attributes$Longitude <- as.numeric(attributes$Longitude)*(-1) #Add negative sign as coordinates are from western hemisphere

# Cromwell #
# ---- Étape 4) Load la shape du lac. Pour que tu puisses importer le fichier, tu dois avoir dans ton directory les 4 fichiers que je t'ai envoyé
CROM <- st_read(paste0(to.carto, "Lake_shapes/Cromwell.shp"))

CROM.att <- attributes %>% filter(Lake == "Cromwell")

# ---- Étape 5) Représentation visuelle
CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "lightblue") + #Shape du lac
  geom_point(data = CROM.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + #Points GPS de tes cages
  scale_color_manual(values = col.pal) + 
  geom_text(data = CROM.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) + #Si tu veux ajouter des étiquettes à tes points
  theme_void() #Pour mettre sur fond blanc plutôt qu'un graphique
CROM.plot
