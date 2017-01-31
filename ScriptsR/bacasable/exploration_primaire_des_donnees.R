library(jsonlite)
library(curl)
library(cluster)
library(RgoogleMaps)

# taille des objets en memoire : 
# sapply(ls(),function(x) format(object.size(get(x)),'auto'))

# importation d'un mois de données historique velib
url <- "http://vlsstats.ifsttar.fr/rawdata/RawData/data_all_Paris.jjson"
url2 <- "D:/projet_velib/Data/data_all_Paris.jjson_2017-01-01-1483248351"
list_data <- fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=","))) ## recupere une liste de data frame
data_frame <- eval(parse(text=paste('rbind(',paste('list_data[[',1:length(list_data),']]',sep = '',collapse = ', '),')',sep = '')))
rm(list_data) ## empile les data frames et supprime la liste

# importation des stations velib
url_station <- "http://vlsstats.ifsttar.fr/data/input_Paris.json"
url2_station <- "D:/projet_velib/Data/input_Paris.json"
stations <- fromJSON(sprintf("[%s]", paste(readLines(url2_station), collapse=",")))[[1]]
stations$lat <- stations$position$lat
stations$lon <- stations$position$lng

sapply(data_frame,class); head(data_frame)
sapply(stations,class); head(stations)

# fusion des bases 
coordstations <- merge(data_frame, stations[,c('number','lat','lon')], by="number", all.x=T)
# stations_sans_coords <- unique(coordstations[,c(1,9,10)])
# stations_sans_coords <- test[is.na(stations_sans_coords$lat) | is.na(stations_sans_coords$lon),]

# retrait des stations sans coordonnées de la base 
lignemanquantecoord <- which(is.na(coordstations[,'lat']) | is.na(coordstations[,'lon']))
coordstations <- coordstations[-lignemanquantecoord,]

# representation des station sur la carte de paris
representation_basique <- unique(coordstations[,c('lat','lon')])
center<-c(mean(range(representation_basique[,'lat'])),mean(range(representation_basique[,'lon'])))
zoom<-MaxZoom(range(representation_basique[,'lat'])*1.1,range(representation_basique[,'lon'])*1.1 )
carte<-GetMap(center=center, zoom=zoom)
PlotOnStaticMap(carte, lat=representation_basique[,'lat'], lon=representation_basique[,'lon'], pch=16, cex=1, col='blue')

