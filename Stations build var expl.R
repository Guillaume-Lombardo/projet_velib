############################################################################
# Construction d'une matrice de variables explivcatives pour les stations ##
############################################################################

#library
library(jsonlite)
library(curl)
library(cluster)
library(stringr)

#pour avoir les altitudes et la clé d'API associée
library(googleway)
api_key <- "AIzaSyDIJyuMBFCW-BAMZYfjmITgDB07j1tCDDY"

library(foreign) # pour lire le dbf equipement
require(geosphere)
library(ggmap)
library(RgoogleMaps)

library(rgeos)
library(sp)

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

stations$position<-NULL


#récupération de la hauteur des stations
#########################################

#vecteur temporaire des altitudes des stations
alt=rep(0,nrow(stations))
nstations200<-floor(nrow(stations)/200)
i<-0
#on fait une boucle car le programme e gère pas toue la liste des stations d'un coup
for (i in 0:(nstations200-1))
{
  df_locations <- stations[(i*200+1):((i+1)*200),c("lat","lon")]
  alt[(i*200+1):((i+1)*200)]<-google_elevation(df_locations = df_locations, key = api_key)$results[,1]
}
# on complète par les statins restantes
df_locations <- stations[(nstations200*200+1):(nrow(stations)),c("lat","lon")]
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
    listedist[[i]]<-which(distGeo(stations[i,c("lat","lon")], stations[-i,c("lat","lon")] )<(dist))
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
    temp<-distGeo(stations[i,c("lat","lon")], stations[-i,c("lat","lon")] )
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

rm(matrixvoisin)

#mesure du degré d'excentricité
###############################

#distance au centre
##################
center<-c(mean(range(stations[,"lat"])),mean(range(stations[,"lon"])))

stations$distcentre<-apply(stations[,c("lat","lon")], MARGIN=1, p2=center, FUN=distGeo)

distanceatoueslesstations<-function(coord)
{
  sum(distGeo(coord, stations[,c("lat","lon")] ))
}

stations$distcentre2<-apply(stations[,c("lat","lon")], MARGIN=1, FUN=distanceatoueslesstations)/1227

# distance au bord
####################

#selection des stations formant l'enveloppe convexe
envconv<-chull(stations[,c("lon", "lat")])
#transformation de ces points en un objet SpatialPolygons
pol = SpatialPolygons(list(Polygons(list(Polygon(cbind(lat=stations[envconv,"lat"], lon=stations[envconv,"lon"]))),
                                     "ID")),
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# transformation de toutes les coordonnées des stations en objet SpatialPoints
pts=SpatialPoints(cbind(lat=stations[,"lat"], lon=stations[,"lon"]),
                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#passage à une projection cartesienne
pol<- spTransform(pol, CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +no_defs"))
pts<- spTransform(pts, CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +no_defs"))

#calcul de la distance au bord et affectation au data.frame
stations$distbord<-as.vector(gDistance(pts, as(pol, "SpatialLines"), byid = TRUE))


# Distance à la station de métro la plus proche
#################################################

#récupération des données RATP stockées dans le fichier coordRATP.json
#a priori, on se sert pour l'instant seulement des latitudes et longiudes des stations
url="data/coordRATP.json"
RATP<-fromJSON(url)
RATPlonlat<-cbind(RATP$elements$lat,RATP$elements$lon)
RATPna<-is.na(RATPlonlat[,1])*is.na(RATPlonlat[,2])
RATPlonlat<-RATPlonlat[RATPna==0,]
RATPlonlat<-unique(RATPlonlat)

#visualisation des stations 
center<-c(mean(range(RATPlonlat[,1])),mean(range(RATPlonlat[,2])))
zoom<-MaxZoom(range(RATPlonlat[,1])*1.1,range(RATPlonlat[,2])*1.1 )
carte<-GetMap(center=center, zoom=10)
PlotOnStaticMap(carte, lat=RATPlonlat[,1], lon=RATPlonlat[,2], pch=16, cex=1, col="red")

#création de la variable $RATP
#distance à la sation de métro/RER/tram la plus proche
for (i  in (1:nrow(stations)))
{
  stations$RATP[i]<-min(distGeo(stations[i,c("lat","lon")], RATPlonlat ))
}


#création d'une base de donnée de stations en fonction du trafic
#on récupère la base de donnée trafic
url="data/trafic-RATP-2015.json"
RATPtrafic<-fromJSON(url)
RATPtrafic<-RATPtrafic$fields[,c("station","trafic")]
names(RATPtrafic)<-c("stations","trafic")
RATPtrafic<-RATPtrafic[!is.na(RATPtrafic$stations),]
RATPtrafic$stations<-str_replace(RATPtrafic$stations, "-RER","")
for (i in 1:10){
  RATPtrafic$stations<-str_replace(RATPtrafic$stations, " ","")
  RATPtrafic$stations<-str_replace(RATPtrafic$stations, "-","")
}
RATPtrafic$stations<-tolower(RATPtrafic$stations)
RATPtrafic<-aggregate(RATPtrafic$trafic, by=list(RATPtrafic$stations), sum)
names(RATPtrafic)<-c("stations","trafic")
#on récupère les coordonnées par stations qui sont absentes de la base précédente
RATPnamelonlat<-cbind.data.frame(RATP$elements$tags$name,RATP$elements$lat,RATP$elements$lon)
RATPna<-is.na(RATPnamelonlat[,1])*is.na(RATPnamelonlat[,2])
RATPnamelonlat<-RATPnamelonlat[RATPna==0,]
names(RATPnamelonlat)<-c("stations", "lat", "lon")
RATPnamelonlat<-RATPnamelonlat[1:655,]
#on fait plein de traitement de texte pour mettre les stations sous le même format que la base précédente
RATPnamelonlat$stations<-as.character(RATPnamelonlat$stations)
RATPnamelonlat$stations[12] <- "Parc de Saint-Cloud"
RATPnamelonlat$stations[419] <- "Parc de Saint-Cloud"
RATPnamelonlat$stations[431] <- "Suresnes - Longchamp"
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " \\(RER\\)","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " \\(métro\\)","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " \\(metro\\)","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " \\(métro 1\\)","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " \\(RER A\\)","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " RER A","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " RER D","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, "\\(","")
RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, "\\)","")
tiret<-str_sub(RATPnamelonlat$stations[3],8,8)
tiret2<-str_sub(RATPnamelonlat$stations[21],8,8)
tiret3<-str_sub(RATPnamelonlat$stations[29],25,25)
for (i in 1:10){
  RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, " ","")
  RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, "-","")
  RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, tiret ,"")
  RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, tiret2 ,"")
  RATPnamelonlat$stations<-str_replace(RATPnamelonlat$stations, tiret3 ,"")
}
RATPnamelonlat<-RATPnamelonlat[!duplicated(RATPnamelonlat$stations),]
RATPnamelonlat$stations<-tolower(RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("[éèëê]", "e", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("[àâä]", "a", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("[îï]", "i", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("[ùüû]", "u", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("[ôö]", "o", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("[ç]", "c", RATPnamelonlat$stations)
RATPnamelonlat[which(str_sub(RATPnamelonlat$stations,1,30)=="bibliothequefrancoismitterrand"),"stations"]<-"bibliotheque"
RATPnamelonlat<-unique(RATPnamelonlat)
RATPnamelonlat$stations<-gsub("creteilprefecturehoteldeville", "creteilprefecture", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("creteill'echathopitalhenrimondor", "creteill'echat", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("ladefensegrandearche", "ladefense", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("louvrerivoli", "louvre", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("marnelavalleechessyparcdisneyland", "marnelavalleechessy", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("noisielleluzard", "noisiel", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("noisychampschampynesles", "noisychamps", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("palaisroyalmuseedulouvre", "palaisroyal", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("pierreetmariecurie", "pierrecurie", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("placedeclichy", "placeclichy", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("saintmandetourelle", "saintmande", RATPnamelonlat$stations)
RATPnamelonlat$stations<-gsub("logneslemandinet", "lognes", RATPnamelonlat$stations)
RATPtrafic<- merge(RATPtrafic, RATPnamelonlat, by="stations" )

#on créé une variable RATP2 qui sera un peu différente car il n'y a pas les trasm et les RER zone SNCF
stations$RATP2<-0
#RATPtrafic donne la fréquentation (en entrée hors correspondance) de la station RATP2 la plus proche
stations$RATPtrafic<-0
for (i in 1:nrow(stations))
{
  temp<-distGeo(stations[i,c("lat","lon")], RATPtrafic[,c("lat","lon")] )
  n<-which(rank(temp) == 1)
  stations$RATP2[i]<-temp[n]
  stations$RATPtrafic[i]<-RATPtrafic[n,"trafic"]
}

rm(RATP)
rm(RATPlonlat)
rm(RATPnamelonlat)
rm(RATPtrafic)

which(stations$RATP!=stations$RATP2)
# Récupération de la base de donnée équipement
#################################################

equip<-read.dbf(file = "data/bpe_ensemble_xy.dbf")
equip<-equip[equip$ancreg== 11,]
equip<-equip[(is.na(equip$typequ)!=T),]

equip$nacoord <- 1
equip[(is.na(equip$lambert_x)==T),"nacoord"] <- 0

table(equip$typequ, equip$nacoord)
# résultat : il n'y a pas tout. En services et commerces, ce n'est pas trop grave car 
# ce ne sont pas les variables a priori les + pertinentes (à vérifier ex post)
# sinon quelques équipements autres non géolocalisés mais en faible proportion
# sauf les cinémas qui sont donc la variable a priori la plus manquante
# les bases des années précédentes n'apportent pas plus d'info

#enrichissement de la base par des catégories plus grosses choisies à la main
equip$typequ2<-""
equip[substring(equip$typequ,1,1)== "A","typequ2"]<-"A" #services
equip[substring(equip$typequ,1,1)== "B","typequ2"]<-"B" #commerces
equip[substring(equip$typequ,1,2)== "B1","typequ2"]<-"B1" #grandes surfaces
equip[substring(equip$typequ,1,2)== "C1","typequ2"]<-"C1" # écoles maternelles et élémentaires
equip[substring(equip$typequ,1,2)== "C2","typequ2"]<-"C2" # collège
equip[substring(equip$typequ,1,2)== "C3","typequ2"]<-"C3" # Lycées
equip[substring(equip$typequ,1,2)== "C4","typequ2"]<-"C4" # Formations post bac non universitaire
equip[substring(equip$typequ,1,2)== "C5","typequ2"]<-"C5" # Enseignement supérieur
equip[substring(equip$typequ,1,2)== "C6","typequ2"]<-"C6" # Autres formations
equip[substring(equip$typequ,1,2)== "C7","typequ2"]<-"C7" # résidence et restau U
equip[substring(equip$typequ,1,1)== "D","typequ2"]<-"D" # Soin et social
equip[substring(equip$typequ,1,2)== "D1","typequ2"]<-"D1" # Hopitaux et assimilés
equip[substring(equip$typequ,1,2)== "E1","typequ2"]<-"E1" # Taxis
equip[substring(equip$typequ,1,2)== "E2","typequ2"]<-"E2" # Aéroports
equip[substring(equip$typequ,1,2)== "E3","typequ2"]<-"E3" # Gares
equip[substring(equip$typequ,1,2)== "F1","typequ2"]<-"F1" # Sport
equip[substring(equip$typequ,1,2)== "F2","typequ2"]<-"F2" # Nautique
equip[substring(equip$typequ,1,2)== "F3","typequ2"]<-"F3" # Culture (Ciné théatre musées)
equip[((substring(equip$typequ,1,4)== "G103") | (substring(equip$typequ,1,4)== "G104")),"typequ2"]<-"Ga" # Hotels et campings
equip[((substring(equip$typequ,1,4)== "G101") | (substring(equip$typequ,1,4)== "G102")),"typequ2"]<-"Gb" # Equipement pour tourisme

#fonction qui convertit les coordonnées lambert 93  en latitude longitude
lambert93ToWgs84 <- function(x, y)
{
  b6  <- 6378137.0000
  b7  <- 298.257222101
  b8  <- 1/b7
  b9  <- 2*b8-b8*b8
  b10 <- sqrt(b9)
  b13 <- 3.000000000
  b14 <- 700000.0000
  b15 <- 12655612.0499
  b16 <- 0.7256077650532670
  b17 <- 11754255.426096
  delx <- x - b14
  dely <- y - b15
  gamma <- atan( -(delx) / dely )
  r <- sqrt((delx*delx)+(dely*dely))
  latiso <- log(b17/r)/b16
  sinphiit0 <- tanh(latiso+b10*atanh(b10*sin(1)))
  sinphiit1 <- tanh(latiso+b10*atanh(b10*sinphiit0))
  sinphiit2 <- tanh(latiso+b10*atanh(b10*sinphiit1))
  sinphiit3 <- tanh(latiso+b10*atanh(b10*sinphiit2))
  sinphiit4 <- tanh(latiso+b10*atanh(b10*sinphiit3))
  sinphiit5 <- tanh(latiso+b10*atanh(b10*sinphiit4))
  sinphiit6 <- tanh(latiso+b10*atanh(b10*sinphiit5))
  longrad <- gamma/b16+b13/180*pi
  latrad <- asin(sinphiit6)
  long <- (longrad/pi*180)
  lat  <- (latrad/pi*180)
  return (cbind(lat,long))
}

#enrichissement du dataframe avec les latitudes longitudes
latlonequip<-lambert93ToWgs84(equip$lambert_x,equip$lambert_y)
equip$lat<-latlonequip[,1]
equip$lon<-latlonequip[,2]
rm(latlonequip)

equip$typequ2<-as.factor(equip$typequ2)

type<-"F2"
center<-c(48.85,2.35)
carte<-GetMap(center=center, zoom=11)
PlotOnStaticMap(carte, lat=equip[((equip$typequ2==type)&(!is.na(equip$lat))),13], lon=equip[((equip$typequ2==type)&(!is.na(equip$lat))),14], pch=16, cex=3, col="red")

#equip[equip$typequ2=="E3",]

typeequip<-levels(equip$typequ2)
n_equip <- length(typeequip)

#fonction qui donne la distance minimale entre une coordonnées et un équipement de type "type"
#@param coord : un vecteur de lat/lon,
#type un type d'équipement correspondant à la colonne typequ2 de la matrice equip (ex "E2")
#@res : un nombre : la distance minimale
distanceequip <- function(coord,type)
{
  min(distGeo(coord, equip[((equip$typequ2==type)&(!is.na(equip$lat))),13:14] ))
}

# distanceequip(stations[1,(13:14)],typeequip[1])

for (i in 1:n_equip)
{
  stations$temp<-0
  names(stations)[ncol(stations)]<-typeequip[i]
  stations[,ncol(stations)]<-apply(stations[,c("lat","lon")], MARGIN=1, type=typeequip[i], FUN=distanceequip)
}


# les gares
###########

gares<-cbind.data.frame(
  "nom"=c("gare d'austerlitz", "gare de l'est", "gare de lyon", "gare du nord", "bercy", "montparnasse", "st lazare" ), 
  "lat"=c(48.84230,48.87620,48.84592, 48.87951, 48.84015, 48.84398, 48.87576),
  "lon"=c(2.365483, 2.35791, 2.374191, 2.357285, 2.379947, 2.324265, 2.324258)) 

for (i  in (1:nrow(stations)))
{
  stations$gare[i]<-min(distGeo(stations[i,c("lat","lon")], gares[,c("lat","lon")] ))
}

#les cinémas
############

cinemas<-read.csv("data/Cinemas.csv", sep=";")
cinemas<-cinemas[cinemas$REGION=="ILE-DE-FRANCE",]
cinemas<-cinemas[cinemas$CODE.DEPARTEMENT %in% c(75,92,93,94),]
cinemas$ADRESSE<-gsub("ÈME", "EME", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("- CE 169", "", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("TER", "", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("4/6 BD BEAUMARCHAI", "", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("ET 1 RUE ETIENNE DOLET", "", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("31 BD BONNE NLL", "", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("SAMUEL DESBORDES", "DESBORDES", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("QUARTIER DU ", "", cinemas$ADRESSE)
cinemas$ADRESSE<-gsub("1 RUE DE L'ORIENT-EXPRESS", "UGC Orient-Express", cinemas$ADRESSE)

cinemas$lat<-0
cinemas$lon<-0
cinemas<-cinemas[!is.na(cinemas$ADRESSE),]
cinemas[,c("lon","lat")]<-geocode(paste(cinemas$ADRESSE, cinemas$CODE.POSTAL))

stations$cinemas<-0
stations$cinemasfauteuils<-0
stations$cinemasseances<-0
for (i  in (1:nrow(stations)))
{
  temp<-distGeo(stations[i,c("lat","lon")], cinemas[,c("lat","lon")] )
  n<-which(rank(temp) == 1)
  stations$cinemas[i]<-temp[n]
  stations$cinemasfauteuils[i]<-cinemas[n,"FAUTEUILS"]
  stations$cinemasseances[i]<-cinemas[n,"SEANCES"]
}


# Pour les variables distance au bord, RATP,  equipements, gares et cinema
# au lieu de prendre la distance, on rajoute une fonction de la distance
# ici exp(-distance²/sigma)
nomcol<-colnames(stations)
first<-which(nomcol[]=="distbord")
last<-which(nomcol[]=="cinemas")

#la fonction de distance
expdist <- function(x, sigma)
{
  exp(-x*x/sigma)
}
#sigma le paramètre de l'exponentielle de la fonction
#qui transforme la distance
sigma<-100000

for (i in first:last)
{
  stations$temp<-0
  names(stations)[ncol(stations)]<-paste0("exp_",nomcol[i])
  stations[,ncol(stations)]<-expdist(stations[,i],sigma)
}
stations$exp_RATPtrafic<-NULL


#sortie de la matrice complète dans un csv
############################################

write.csv(stations, file = "Sortie/Jeuvarexpli.csv")

