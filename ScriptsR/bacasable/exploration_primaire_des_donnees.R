library(jsonlite)

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

coordstations <- merge(data_frame, stations[,c('number','lat','lon')], by="number", all.x=T)
coordstations <- coordstations[,c(1,9,10)]
#on n'a pas les coordonn?es de certaines stations donc on va les enlever pour l'analyse (surtout l'analyse g?ographique)
lignemanquantecoord<-which(is.na(coordstations[,2]))
#coordstations<-coordstations[-lignemanquantecoord,]
coordstations[lignemanquantecoord,c(2)]<-mean(coordstations[,2], na.rm = T)
coordstations[lignemanquantecoord,c(3)]<-mean(coordstations[,3], na.rm = T)
