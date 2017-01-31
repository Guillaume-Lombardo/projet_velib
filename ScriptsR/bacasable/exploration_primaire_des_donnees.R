library(jsonlite)
library(curl)
library(cluster)
library(RgoogleMaps)
library(reshape2)
library(dplyr)

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

# base de données par date par station :

# ajout des proportions
coordstations$proportion <- coordstations$available_bikes / max((coordstations$available_bike_stands+coordstations$available_bikes),1)
# ajout d'une variable de temps raisonable et d'une variable de temps modulo 7 jours
coordstations$time <- as.POSIXct(coordstations$last_update/1000, origin="1970-01-01")
coordstations$date_mod_7j <- paste('T', 
                                   format(coordstations$time, "%w"), 
                                   format(coordstations$time, "%H"), 
                                   floor(as.numeric(format(coordstations$time, "%M"))/20)*20, 
                                   sep = '_')
# head(coordstations$date_mod_7j,10)
# names(coordstations)

# transposition en dataframe dont les noms de colonnes sont les temps de mesure (toute les 20 minutes),
# les lignes sont les stations et les mesures les moyennes de proportions.

coordstations_melt <- melt(data = coordstations,id.vars = c('number','date_mod_7j'), measure.vars = 'proportion')
stations_x_data_mod_7j <- dcast(data = coordstations_melt,formula = number~date_mod_7j,fun.aggregate = mean)
data_mod_7j_x_station <- dcast(data = coordstations_melt,formula = date_mod_7j~number,fun.aggregate = mean)

max(sapply(data_mod_7j_x_station[,-1],max))

plot(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,2],type = 'l',ylim = c(0,1))
for(i in 3:ncol(data_mod_7j_x_station)){
  lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,i])
}


### lissage de data_mod_7j_x_station en data_mod_7j_x_station_L
test <- runmed(data_mod_7j_x_station[,2],7)

tail(test)
plot(seq_along(test),test, type = 'l', ylim = c(0,.2))
lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,2],col=1)

data_mod_7j_x_station_L <- sapply(2:ncol(data_mod_7j_x_station),function(x) runmed(data_mod_7j_x_station[,x],7)) %>%
  as.data.frame()
names(data_mod_7j_x_station_L) <- names(data_mod_7j_x_station[,-1])
data_mod_7j_x_station_L$date_mod_7j <- data_mod_7j_x_station$date_mod_7j

plot(seq_along(data_mod_7j_x_station_L$date_mod_7j),data_mod_7j_x_station[,2],type = 'l',ylim = c(0,1))
for(i in 3:ncol(data_mod_7j_x_station_L)){
  lines(seq_along(data_mod_7j_x_station_L$date_mod_7j),data_mod_7j_x_station_L[,i])
}

cluster <- lapply(1:10,function(x) kmeans(data_mod_7j_x_station_L[,c(-1,-910)],x,iter.max = 20))
cluster2 <- kmeans(data_mod_7j_x_station_L[,c(-1,-910)],2,iter.max = 20)

test <- sapply(data_mod_7j_x_station_L[,c(-1,-910)],function(x) any(is.infinite(x)))

which(names(data_mod_7j_x_station_L)=='19108')
test[test]
which(test)
summary(data_mod_7j_x_station_L[,'19108'])
sort(data_mod_7j_x_station_L[,909])

lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,3])
lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,4])
lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,2])

