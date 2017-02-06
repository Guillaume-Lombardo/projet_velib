library(jsonlite)
library(curl)
library(cluster)
library(RColorBrewer)
library(RgoogleMaps)
library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)

# taille des objets en memoire : 
# sapply(ls(),function(.x) format(object.size(get(.x)),'auto'))

# importation d'un mois de données historique velib
# url <- "http://vlsstats.ifsttar.fr/rawdata/RawData/data_all_Paris.jjson"
# url2 <- "D:/projet_velib/Data/data_all_Paris.jjson_2017-01-01-1483248351"
url2 <- "./Data/data_all_Paris.jjson_2017-01-01-1483248351.gz"
# list_data <- fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=","))) ## recupere une liste de data frame
# data_frame <- eval(parse(text=paste('rbind(',paste('list_data[[',1:length(list_data),']]',sep = '',collapse = ', '),')',sep = '')))
## reformulation pour ne pas avoir creer un vecteur de charactere trop long : 
list_data <- lapply(readLines(url2), function(.x) fromJSON(sprintf("[%s]", .x))) ## recupere une liste de liste contenant 1 data frame
data_frame <- eval(parse(text=paste('rbind(',paste('list_data[[',1:length(list_data),']][[1]]',sep = '',collapse = ', '),')',sep = '')))
rm(list_data) ## empile les data frames et supprime la liste

# importation des stations velib
# url_station <- "http://vlsstats.ifsttar.fr/data/input_Paris.json"
# url2_station <- "D:/projet_velib/Data/input_Paris.json"
url2_station <- "./Data/input_Paris.json"
stations <- fromJSON(sprintf("[%s]", paste(readLines(url2_station), collapse=",")))[[1]]
stations$lat <- stations$position$lat
stations$lon <- stations$position$lng

sapply(data_frame,class); head(data_frame)
sapply(stations,class); head(stations)

# fusion des bases 
coordstations <- merge(data_frame, stations[,c('number','lat','lon')], by="number", all.x=T)
# stations_sans_coords <- unique(coordstations[,c(1,9,10)])
# stations_sans_coords <- test[is.na(stations_sans_coords$lat) | is.na(stations_sans_coords$lon),]

# retrait des stations sans coordonn?es de la base 
lignemanquantecoord <- which(is.na(coordstations[,'lat']) | is.na(coordstations[,'lon']))
unique(coordstations$number[lignemanquantecoord])
coordstations <- coordstations[-lignemanquantecoord,]

# representation des station sur la carte de paris
representation_basique <- unique(coordstations[,c('lat','lon')])
center<-c(mean(range(representation_basique[,'lat'])),mean(range(representation_basique[,'lon'])))
zoom<-MaxZoom(range(representation_basique[,'lat'])*1,range(representation_basique[,'lon'])*1 )
carte<-GetMap(center=center, zoom=zoom)
# PlotOnStaticMap(carte, lat=representation_basique[,'lat'], lon=representation_basique[,'lon'], pch=16, cex=1, col='blue')
# dev.off()
# base de données par date par station :

# ajout des proportions
coordstations$proportion <- coordstations$available_bikes / max((coordstations$available_bike_stands+coordstations$available_bikes),1)
coordstations$proportion <- ifelse(is.na(coordstations$proportion),0,coordstations$proportion)
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

eval(parse(text=paste('stations_x_data_mod_7j[,',
											1:ncol(stations_x_data_mod_7j),
											'] <- ifelse(is.na(stations_x_data_mod_7j[,',
											1:ncol(stations_x_data_mod_7j),
											']),0,stations_x_data_mod_7j[,',
											1:ncol(stations_x_data_mod_7j),
											'])',
											sep = '')))
eval(parse(text=paste('data_mod_7j_x_station[,',
											1:ncol(data_mod_7j_x_station),
											'] <- ifelse(is.na(data_mod_7j_x_station[,',
											1:ncol(data_mod_7j_x_station),
											']),0,data_mod_7j_x_station[,',
											1:ncol(data_mod_7j_x_station),
											'])',
											sep = '')))

unique(data_mod_7j_x_station$date_mod_7j)
sapply(data_mod_7j_x_station[,-1],max) %>% sort(.) %>% max(.) 

# plot(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,2],type = 'l',ylim = c(0,1))
# for(i in 3:ncol(data_mod_7j_x_station)){
#   lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,i])
# }


### lissage de data_mod_7j_x_station en data_mod_7j_x_station_L
# test <- runmed(data_mod_7j_x_station[,2],7)
# tail(test)
# plot(seq_along(test),test, type = 'l', ylim = c(0,.2))
# lines(seq_along(data_mod_7j_x_station$date_mod_7j),data_mod_7j_x_station[,2],col=1)
# 
# data_mod_7j_x_station_L <- sapply(2:ncol(data_mod_7j_x_station),function(x) runmed(data_mod_7j_x_station[,x],7)) %>%
#   as.data.frame()
# names(data_mod_7j_x_station_L) <- names(data_mod_7j_x_station[,-1])
# data_mod_7j_x_station_L$date_mod_7j <- data_mod_7j_x_station$date_mod_7j

### representation graphique 'pour voir'
# plot(seq_along(data_mod_7j_x_station_L$date_mod_7j),data_mod_7j_x_station[,2],type = 'l',ylim = c(0,1))
# for(i in 3:ncol(data_mod_7j_x_station_L)){
#   lines(seq_along(data_mod_7j_x_station_L$date_mod_7j),data_mod_7j_x_station_L[,i])
# }

### remise dans le sens stations~date_mod_7J

# data_mod_7j_x_station_melt <- melt(data = data_mod_7j_x_station_L,id.vars = c('date_mod_7j'),measure.vars = 2:ncol(data_mod_7j_x_station_L))
# data_mod_7j_x_station_melt$value <- as.numeric(data_mod_7j_x_station_melt$value)
# stations_x_data_mod_7j_L <- dcast(data = data_mod_7j_x_station_melt,formula = variable~date_mod_7j)
# cluster <- lapply(1:12,function(x) kmeans(stations_x_data_mod_7j_L[c(-909,-1219) ,2:ncol(stations_x_data_mod_7j_L)],x,iter.max = 30))
# representation_kmeans <- lapply(1:12,function(x) merge(data.frame(number = stations_x_data_mod_7j_L$variable[c(-909,-1219)],cluster = cluster[[x]]$cluster,stations_x_data_mod_7j_L[c(-909,-1219),-1]),
# 																											 stations[,c('number','lat','lon')],
# 																											 by="number", all.x=T))

cluster <- lapply(1:12,function(.x) kmeans(stations_x_data_mod_7j[ ,2:ncol(stations_x_data_mod_7j)],.x,iter.max = 30))
representation_kmeans <- lapply(1:12,function(.x) merge(data.frame(number = stations_x_data_mod_7j$number,
																																	 cluster = cluster[[.x]]$cluster,
																																	 stations_x_data_mod_7j[,-1]), 
																												stations[,c('number','lat','lon')], by="number", all.x=T))


### representation graphique des kmeans en fonction du nombre de groupe
# tracer le graphique grid a : grid.draw(a)
for(i in 2:10){
	ifelse(is.null(dev.list()),1,dev.off())
  PlotOnStaticMap(carte, lat=representation_kmeans[[i]][,'lat'], lon=representation_kmeans[[i]][,'lon'], pch=16, cex=1, col=brewer.pal(i, 'Spectral')[as.numeric(representation_kmeans[[i]]$cluster)])
	grid.echo()
	eval(parse(text = paste('map_plot_',i,'_cluster <- grid.grab()',sep = '')))
	dev.off()
}

ifelse(is.null(dev.list()),1,dev.off())
plot(x = 1:12,y = sapply(1:12,function(.x) cluster[[.x]]$betweenss/cluster[[.x]]$totss), type = 'l',ylim = c(0,1),ylab = 'part de la variance expliqué par les groupes', xlab = 'nombre de classe')
abline(h=(0:5)/5)
abline(v=(0:6)*2)
grid.echo()
Quantite_inter_sur_totale <- grid.grab()
dev.off()

### profil par classe

moyenne_par_classe <- function(x){
  liste_variable_temps <- names(representation_kmeans[[x]])[which(substr(names(representation_kmeans[[x]]),1,1)=='T')]
  liste_moyenne <- paste('M', substring(liste_variable_temps,2)," = mean(",liste_variable_temps,")",sep = '') %>%
    paste(collapse = ', ')
  chaine_dplyr <-  paste('representation_kmeans[[',x,']] %>% group_by(cluster) %>% summarise(',liste_moyenne,')',sep = '')
  res <- eval(parse(text =chaine_dplyr))
  return(res)
}

profil_par_classe <- lapply(1:12,moyenne_par_classe)

# tracer profil de classe en fonction du nombre de classes
for(i in 2:10){
	ifelse(is.null(dev.list()),1,dev.off())
  n <- ncol(profil_par_classe[[i]])
  xx <- seq_along(names(profil_par_classe[[i]])[-1])
  plot(xx,as.data.frame(profil_par_classe[[i]][1,2:n]),type = 'l',col = 1,ylim = c(0,1),ylab = 'proportion de velib dans la classe')
  for(j in 2:i){
    lines(xx,as.data.frame(profil_par_classe[[i]][j,2:n]),type = 'l',col = j,ylim = c(0,1))  
  }
  grid.echo()
  eval(parse(text = paste('profil_de_classe_',i,'_cluster <- grid.grab()',sep = '')))
  dev.off()
}

# PlotOnStaticMap(carte, lat=representation_kmeans[[i]][,'lat'], lon=representation_kmeans[[i]][,'lon'], pch=16, cex=1, col=brewer.pal(i, 'Spectral')[as.numeric(representation_kmeans[[i]]$cluster)])
# dev.off()

# tracer profil de classe avec les membre de la classe
for(i in 2:10){
	for (k in 1:i){
		ifelse(is.null(dev.list()),1,dev.off())
		plot(xx,as.data.frame(profil_par_classe[[i]][k,2:n]),type = 'l',col = 1,ylim = c(0,1),ylab = 'proportion de velib dans la classe')
		for(j in 3:(ncol(representation_kmeans[[i]])-2)){
			if(representation_kmeans[[i]]$cluster[j]==k){
				lines(xx,data_mod_7j_x_station[,j], col = 'lightgray')	
			}
		}
		lines(xx,as.data.frame(profil_par_classe[[i]][k,2:n]), col = 1)	
		grid.echo()
		eval(parse(text = paste('profil_',i,'_cluster_classe_',k,' <- grid.grab()',sep = '')))
		dev.off()
	}
}
