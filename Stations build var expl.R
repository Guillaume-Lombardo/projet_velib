############################################################################
# Construction d'une matrice de variables explivcatives pour les stations ##
############################################################################

#library
library(jsonlite)
library(curl)
library(cluster)
require(geosphere)

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

#Fonction voisindist(dist)
#argument : rayon 'dist' autour des stations  dans lequel on regarde (en m)
#sortie : une liste de vecteurs de numéro de stations (numéros dans l'ordre de la matrice station)
# (pour la station i, listedist[[i]] est un vecteur des stations proches de la station de la ième ligne de la matrice stations)
#la matrice station est considérée comme une variable globale
voisindist <- function(dist)
{
  listedist<-vector('list', nrow(stations))
  for (i in 1:nrow(stations))
  {
    listedist[[i]]<-which(distGeo(stations[i,13:14], stations[-i,13:14] )<(dist))
  }
  return(listedist)
}


# Fonction qui donne la liste des k plus proches stations
#################################################################

#Fonction voisinnum(n)
#argument : nombre n de stations à retenir
#sortie : une matrice de nrow(stations) lignes et n colonnes
# la première colonne donne la station la plus proche parmi les n et la n est la plus éloignée
#la matrice station est considérée comme une variable globale
voisinnum <- function(n)
{
  out<-matrix(0,nrow=nrow(stations), ncol=n )
  for (i in 1:nrow(stations))
  {
    temp<-distGeo(stations[i,13:14], stations[-i,13:14] )
    out[i,]<-which(rank(temp) <= n)
  }
  return(out)
}



# bike_stands proches
#######################

# Enrichissement de stations avec le nombre de bike_stands dans un rayon de x m
# (+ ou - notion de densité de bike_stand)
# Essai avec 3 variables 200m, 500m, et 1000m

liste<-voisindist(200)
stations$nbstand200<-0
for (i in (1:nrow(stations)))
{
  n_liste <- length(liste[[i]])
  if (n_liste!=0)
  {
    for (j in 1:n_liste)
    {
      stations$nbstand200[i]<-stations$nbstand200[i] + (stations[(liste[[i]][j]),9])
    }
  }
}

liste<-voisindist(500)
stations$nbstand500<-0
for (i in (1:nrow(stations)))
{
  n_liste <- length(liste[[i]])
  if (n_liste!=0)
  {
    for (j in 1:n_liste)
    {
      stations$nbstand500[i]<-stations$nbstand500[i] + (stations[(liste[[i]][j]),9])
    }
  }
}
liste<-voisindist(1000)
stations$nbstand1000<-0
for (i in (1:nrow(stations)))
{
  n_liste <- length(liste[[i]])
  if (n_liste!=0)
  {
    for (j in 1:n_liste)
    {
      stations$nbstand1000[i]<-stations$nbstand1000[i] + (stations[(liste[[i]][j]),9])
    }
  }
}
rm(liste)


# hauteur relative par rapport au n plus proches voisins
#########################################################

#essai avec les 5 et les 10 plus proches voisins

matrixvoisin<-voisinnum(5)
for (i  in (1:nrow(stations)))
{
  stations$hr5[i]<-stations[i,"alt"]-sum(stations[matrixvoisin[i,],"alt"])/5
}

matrixvoisin<-voisinnum(10)
for (i  in (1:nrow(stations)))
{
  stations$hr10[i]<-stations[i,"alt"]-sum(stations[matrixvoisin[i,],"alt"])/5
}




# Distance à la station de métro la plus proche
#################################################

#récupération des données RATP stockées dans le fichier coordRATP.json
#a priori, on se sert pour l'instant seulement des latitudes et longiudes des stations
url="data/coordRATP.json"
RATP<-fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=",")))
RATPlonlat<-cbind(RATP$elements$lat,RATP$elements$lon)
RATPna<-is.na(RATPlonlat[,1])*is.na(RATPlonlat[,2])
RATPlonlat<-RATPlonlat[RATPna==0,]

#visualisation des stations 
center<-c(mean(range(RATPlonlat[,1])),mean(range(RATPlonlat[,2])))
zoom<-MaxZoom(range(RATPlonlat[,1])*1.1,range(RATPlonlat[,2])*1.1 )
carte<-GetMap(center=center, zoom=10)
PlotOnStaticMap(carte, lat=RATPlonlat[,1], lon=RATPlonlat[,2], pch=16, cex=1, col="red")

#création de la variable $RATP
#distance à la sation de métro/RER/tram la plus proche
for (i  in (1:nrow(stations)))
{
  stations$RATP[i]<-min(distGeo(stations[i,13:14], RATPlonlat ))
}



i<-1
