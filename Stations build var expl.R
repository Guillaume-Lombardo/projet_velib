############################################################################
# Construction d'une matrice de variables explivcatives pour les stations ##
############################################################################

#library
library(jsonlite)
library(curl)
library(cluster)

#pour avoir les altitudes et la clé d'API associée
library(googleway)
api_key <- "AIzaSyDIJyuMBFCW-BAMZYfjmITgDB07j1tCDDY"

library(ggmap)
library(RgoogleMaps)

#récupération de la liste officielle des stations
#################################################

url="http://vlsstats.ifsttar.fr/data/input_Paris.json"
stations<-fromJSON(sprintf("[%s]", paste(readLines(url), collapse=",")))
stations<-stations[[1]]

#récupération de la latitude et de la longitude 
# et création comme une véritable variable
#(initialement dans un vecteur position)
stations$lat<-stations$position$lat
stations$lon<-stations$position$lng




#récupération de la hauteur des stations
#########################################

#vecteur temporaire des altitudes des stations
alt=rep(0,nrow(stations))
nstations200<-floor(nrow(stations)/200)
i<-0
#on fait une boucle car le programme e gère pas toue la liste des stations d'un coup
for (i in 0:(nstations200-1))
{
  df_locations <- stations[(i*200+1):((i+1)*200),c(13,14)]
  alt[(i*200+1):((i+1)*200)]<-google_elevation(df_locations = df_locations, key = api_key)$results[,1]
}
# on complète par les statins restantes
df_locations <- stations[(nstations200*200+1):(nrow(stations)),c(13,14)]
alt[(nstations200*200+1):(nrow(stations))]<-google_elevation(df_locations = df_locations, key = api_key)$results[,1]
#on affecte au dataframe station et on nettoie
stations$alt<-alt
rm(df_locations)
rm(alt)
rm(nstations200)



# Fonction qui donne la liste des stations dans un rayon de x km
#################################################################

#Fonction voisindist (numstation, dist)
#argument : numéro d'une station numstation, rayon dist autour de la station initiale dans lequel on regarde
#sortie : une liste de numéro de stations

voisindist <- function(numstation, dist)
{
  return(0)
}

voisindist(1,1)


