
url2 <- "./Data/data_all_Paris.jjson_2017-01-01-1483248351.gz"

# importation des stations velib
url2_station <- "./Data/input_Paris.json"
stations <- fromJSON(sprintf("[%s]", paste(readLines(url2_station), collapse=",")))[[1]]
stations$lat <- stations$position$lat
stations$lon <- stations$position$lng

# representation des station sur la carte de paris
representation_basique <- unique(stations[,c('lat','lon')])
center <- c(mean(range(representation_basique[,'lat'])),mean(range(representation_basique[,'lon'])))
zoom <- MaxZoom(range(representation_basique[,'lat'])*1,range(representation_basique[,'lon'])*1 )
carte <- GetMap(center=center, zoom=zoom)

test_json <- function(url = url2){
	print(c('fichier : ',url))
	## reformulation pour ne pas avoir creer un vecteur de charactere trop long : 
	list_data <- lapply(readLines(url), function(.x) fromJSON(sprintf("[%s]", .x))) ## recupere une liste de liste contenant 1 data frame
	rm(list_data) ## empile les data frames et supprime la liste
	return('ok')
}
creation_base_station_x_date_mod <- function(url = url2,coord_stations = stations){
	print(c('fichier : ',url))
	## reformulation pour ne pas avoir creer un vecteur de charactere trop long : 
	list_data <- lapply(readLines(url), function(.x) fromJSON(sprintf("[%s]", .x))) ## recupere une liste de liste contenant 1 data frame
	data_frame <- eval(parse(text=paste('rbind(',
																			paste('list_data[[',1:length(list_data),']][[1]]',sep = '',collapse = ', '),
																			')',sep = '')))
	rm(list_data) ## empile les data frames et supprime la liste
	
	# fusion des bases 
	coordstations <- merge(data_frame, coord_stations[,c('number','lat','lon')], by="number", all.x=T)	
	
	# retrait des stations sans coordonn?es de la base 
	lignemanquantecoord <- which(is.na(coordstations[,'lat']) | is.na(coordstations[,'lon']))
	coordstations <- coordstations[-lignemanquantecoord,]
	
	# base de données par date par station :
	
	# ajout des proportions
	coordstations$proportion <- coordstations$available_bikes / pmax((coordstations$available_bike_stands + 
																																			coordstations$available_bikes),1)
	coordstations$proportion <- ifelse(is.na(coordstations$proportion),0,coordstations$proportion)
	# ajout d'une variable de temps raisonable et d'une variable de temps modulo 7 jours
	coordstations$time <- as.POSIXct(coordstations$last_update/1000, origin="1970-01-01")
	coordstations$date_mod_7j <- paste('T', 
																		 format(coordstations$time, "%w"), 
																		 format(coordstations$time, "%H"), 
																		 floor(as.numeric(format(coordstations$time, "%M"))/20)*20, 
																		 sep = '_')
	
	coordstations$jour <- paste('J',format(coordstations$time, "%w"),sep = '_')
	coordstations$identifiant_jour <- paste(coordstations$jour,coordstations$number,sep = '_')
	coordstations$date_mod_j <- paste('T', 
																		format(coordstations$time, "%H"), 
																		floor(as.numeric(format(coordstations$time, "%M"))/20)*20, 
																		sep = '_')
	
	coordstations_melt <- melt(data = coordstations,id.vars = c('number','date_mod_7j'), measure.vars = 'proportion')
	stations_x_data_mod_7j <- dcast(data = coordstations_melt,formula = number~date_mod_7j,fun.aggregate = mean)
	
	eval(parse(text=paste('stations_x_data_mod_7j[,',
												1:ncol(stations_x_data_mod_7j),
												'] <- ifelse(is.na(stations_x_data_mod_7j[,',
												1:ncol(stations_x_data_mod_7j),
												']),0,stations_x_data_mod_7j[,',
												1:ncol(stations_x_data_mod_7j),
												'])',
												sep = '')))
	
	return(stations_x_data_mod_7j)
}

liste_historique <- c(#'./Data/data_all_Paris.jjson_2015-09-01-1441081722.gz', ## probleme de json
											'./Data/data_all_Paris.jjson_2016-04-01-1459484749.gz',
											'./Data/data_all_Paris.jjson_2016-11-01-1477977947.gz',
											#'./Data/data_all_Paris.jjson_2015-03-01-1425187709.gz', ## probleme de json
											'./Data/data_all_Paris.jjson_2015-10-01-1443673554.gz',
											'./Data/data_all_Paris.jjson_2016-05-01-1462076748.gz',
											'./Data/data_all_Paris.jjson_2016-12-01-1480569957.gz',
											#'./Data/data_all_Paris.jjson_2015-04-01-1427862380.gz', ## probleme de json
											'./Data/data_all_Paris.jjson_2015-11-01-1446355872.gz',
											'./Data/data_all_Paris.jjson_2016-06-01-1464755163.gz',
											'./Data/data_all_Paris.jjson_2017-01-01-1483248351.gz',
											'./Data/data_all_Paris.jjson_2015-05-01-1430454376.gz',
											#'./Data/data_all_Paris.jjson_2015-12-01-1448947618.gz', ## probleme de json
											'./Data/data_all_Paris.jjson_2016-07-01-1467347186.gz',
											'./Data/data_all_Paris.jjson_2015-06-01-1433132790.gz',
											'./Data/data_all_Paris.jjson_2016-01-01-1451625969.gz',
											'./Data/data_all_Paris.jjson_2016-08-01-1470025624.gz',
											'./Data/data_all_Paris.jjson_2015-07-01-1435725312.gz',
											'./Data/data_all_Paris.jjson_2016-02-01-1454304333.gz',
											'./Data/data_all_Paris.jjson_2016-09-01-1472703936.gz',
											#'./Data/data_all_Paris.jjson_2015-08-01-1438416396.gz', ## probleme de json
											'./Data/data_all_Paris.jjson_2016-03-01-1456809947.gz',
											'./Data/data_all_Paris.jjson_2016-10-01-1475295963.gz')

# test <- creation_base_station_x_date_mod(url = url2,coord_stations = stations)
# rm(test)
# liste_sation_x_date_mod7j <- lapply(liste_historique,function(.x) test_json(url = .x)) ## verif de la validité du json...

# ce qui suit est long car les fichier jjson sont compressés
liste_sation_x_date_mod7j <- lapply(liste_historique,function(.x) creation_base_station_x_date_mod(url = .x,coord_stations = stations))
# moyenne simple des different sation_x_date_mod7j

# sapply(1:18, function(.x) dim(liste_sation_x_date_mod7j[[.x]]))
# sapply(1:18, function(.x) names(liste_sation_x_date_mod7j[[.x]])[1])
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[2]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[3]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[4]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[5]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[6]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[7]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[8]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[9]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[10]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[11]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[12]]))
# liste_sation_x_date_mod7j[[12]][,506]
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[13]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[14]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[15]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[16]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[17]]))
# which(names(liste_sation_x_date_mod7j[[1]])!=names(liste_sation_x_date_mod7j[[18]]))
# 
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[2]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[3]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[4]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[5]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[6]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[7]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[8]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[9]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[10]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[11]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[12]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[13]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[14]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[15]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[16]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[17]]))
# which(row.names(liste_sation_x_date_mod7j[[1]])!=row.names(liste_sation_x_date_mod7j[[18]]))
# liste_sation_x_date_mod7j[[7]][1219:1228,1:3]
# liste_sation_x_date_mod7j[[1]][1219:1226,1:3]
# 
# liste_sation_x_date_mod7j2 <- liste_sation_x_date_mod7j
# liste_sation_x_date_mod7j <- liste_sation_x_date_mod7j2



# sapply(1:18, function(.x) names(liste_sation_x_date_mod7j[[i]])[2:10])

full_merge <- liste_sation_x_date_mod7j[[1]] %>% 
	base::merge(x= ., y = liste_sation_x_date_mod7j[[2]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_2')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[3]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_3')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[4]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_4')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[5]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_5')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[6]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_6')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[7]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_7')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[8]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_8')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[9]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_9')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[10]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_10')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[11]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_11')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[12]][,-506], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_12')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[13]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_13')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[14]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_14')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[15]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_15')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[16]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_16')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[17]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_17')) %>%
	base::merge(x= ., y = liste_sation_x_date_mod7j[[18]], by.x = 'number',by.y = 'number', all = F, suffixes = c('','_18'))

station_x_date_mod7j <- full_merge[,1:505]

moyenne18 <- function(nom_var){
	ligne1 <- paste('sation_x_date_mod7j$',nom_var,sep = '')
	ligne2 <- paste('full_merge$',nom_var,sep = '')
	
	paste(ligne1,' <- (',ligne2,' + ',
				paste(ligne2,2:18,sep = '_',collapse = ' + '),
				')/18',sep = '')	
}

moyenne18(names(station_x_date_mod7j)[2])
moyenne18(names(station_x_date_mod7j)[3])
moyenne18(names(station_x_date_mod7j)[4])

for(i in 2:ncol(station_x_date_mod7j)){
	eval(parse(text = moyenne18(names(station_x_date_mod7j)[i])))
}



# export du resultat vers un csv (sep = ';') pour eviter le temps (long) de calcul precedent !

write.table(x = station_x_date_mod7j,file = './Sortie/station_x_date_mod7j.csv',sep = ';',dec = ',',row.names = F)
write.table(x = stations[,c('number','lat','lon')],file = './Sortie/stations_gps.csv',sep = ';',dec = ',',row.names = F)
