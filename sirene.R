#librairies
library(jsonlite)
library(curl)
library(cluster)
library(reshape2)
library(plyr)
library(dplyr)
library(raster)
library(maptools)
library(ggmap)
library(RgoogleMaps)
library(googleway)
library(deldir)
library(sp)
devtools::install_github("rstudio/leaflet")
library(leaflet)
library(rgeos)
library(tibble)
library(crosstalk)
library(viridis)

#### gestion des polygones
#### 4 versions : voronoi100, voronoi500, cercle100, cercle500

#récupération des stations et de la latitude et de la longitude 
# et création comme une véritable variable
#(initialement dans un vecteur position)
stations<-fromJSON(sprintf("[%s]", 
                           paste(readLines("http://vlsstats.ifsttar.fr/data/input_Paris.json", 
                                           encoding = "UTF-8"), 
                                 collapse=",")))
stations<-stations[[1]]
stations$lat<-stations$position$lat
stations$lon<-stations$position$lng
stations <- stations[,names(stations) %in% c("number","name","address","lat","lon")]
rownames(stations) <- stations$number

#création du voronoi de base en SpatialPolygonsDataFrame
voronoi <- tile.list(deldir(stations$lon,stations$lat))
voronoi.polys = vector(mode='list', length=length(voronoi))
for (i in seq(along=voronoi.polys)) {
  voronoi.polys[[i]] = Polygons(list(Polygon(rbind(cbind(voronoi[[i]]$x, voronoi[[i]]$y),cbind(voronoi[[i]]$x, voronoi[[i]]$y)[1,]))), ID=stations$number[i])
}
voronoi.polys <- SpatialPolygons(voronoi.polys, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
voronoi <- SpatialPolygonsDataFrame(voronoi.polys, 
                                    data=data.frame(lon=stations$lon, 
                                                    lat=stations$lat,
                                                    number=stations$number,
                                                    name=stations$name,
                                                    address=stations$address,
                                                    row.names=sapply(slot(voronoi.polys, 'polygons'), 
                                                                     function(x) slot(x, 'ID'))))


#création de cercle100 et cercle500
cercle500 <- SpatialPoints(
  stations[,c("lon","lat")], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) %>%
  spTransform(CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +no_defs")) %>%
  gBuffer(width=500, byid=TRUE) %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
cercle100 <- SpatialPoints(
  stations[,c("lon","lat")], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) %>%
  spTransform(CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +no_defs")) %>%
  gBuffer(width=100, byid=TRUE) %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

#création de voronoi100 et voronoi500
voronoi500 <- cercle500 #initialisation
voronoi100 <- cercle100 #initialisation
for (i in 1:length(voronoi@polygons))
{voronoi500@polygons[i] <- (raster::intersect(cercle500[i],voronoi.polys[i]))@polygons
voronoi100@polygons[i] <- (raster::intersect(cercle100[i],voronoi.polys[i]))@polygons
voronoi500@polygons[[i]]@ID <- voronoi@polygons[[i]]@ID
voronoi100@polygons[[i]]@ID <- voronoi@polygons[[i]]@ID
}

#### visualisation (au cas où)
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   setView(lng = mean(stations$lon), lat = mean(stations$lat), zoom = 12) %>%
#   addCircles(data=stations, lat=stations$lat, lng=stations$lon, radius=5,color="navy") %>%
#   addPolygons(data=voronoi500,
#               label=stations$name,
#               color="red",opacity=0.3,fillColor="yellow",fillOpacity=0.05,weight=1,
#               highlightOptions = highlightOptions(color = "black", weight = 4,
#                                                   bringToFront = TRUE)) %>%
#   addScaleBar(options = scaleBarOptions(imperial=F)) %>%
#   addMiniMap(tiles = providers$Esri.WorldTopoMap,
#              toggleDisplay = TRUE)




#### import de SIRENE (pour les depts 75 92 93 94 pour limiter le temps de calcul)
#### fichiers csv contenant les données géolocalisées (depuis http://212.47.238.202/geo_sirene/)

# sirene_75101 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75101.csv")[,c(1:47,85,86)]
# sirene_75102 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75102.csv")[,c(1:47,85,86)]
# sirene_75103 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75103.csv")[,c(1:47,85,86)]
# sirene_75104 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75104.csv")[,c(1:47,85,86)]
# sirene_75105 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75105.csv")[,c(1:47,85,86)]
# sirene_75106 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75106.csv")[,c(1:47,85,86)]
# sirene_75107 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75107.csv")[,c(1:47,85,86)]
# sirene_75108 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75108.csv")[,c(1:47,85,86)]
# sirene_75109 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75109.csv")[,c(1:47,85,86)]
# sirene_75110 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75110.csv")[,c(1:47,85,86)]
# sirene_75111 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75111.csv")[,c(1:47,85,86)]
# sirene_75112 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75112.csv")[,c(1:47,85,86)]
# sirene_75113 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75113.csv")[,c(1:47,85,86)]
# sirene_75114 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75114.csv")[,c(1:47,85,86)]
# sirene_75115 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75115.csv")[,c(1:47,85,86)]
# sirene_75116 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75116.csv")[,c(1:47,85,86)]
# sirene_75117 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75117.csv")[,c(1:47,85,86)]
# sirene_75118 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75118.csv")[,c(1:47,85,86)]
# sirene_75119 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75119.csv")[,c(1:47,85,86)]
# sirene_75120 <- read.csv("C:/Users/tix/Desktop/geo-sirene_75120.csv")[,c(1:47,85,86)]
# sirene_92 <- read.csv("C:/Users/tix/Desktop/geo-sirene_92.csv")[,c(1:47,85,86)]
# sirene_93 <- read.csv("C:/Users/tix/Desktop/geo-sirene_93.csv")[,c(1:47,85,86)]
# sirene_94 <- read.csv("C:/Users/tix/Desktop/geo-sirene_94.csv")[,c(1:47,85,86)]
# sirene <- rbind(sirene_75101, sirene_75102, sirene_75103, sirene_75104, 
#                 sirene_75105, sirene_75106, sirene_75107, sirene_75108,
#                 sirene_75109, sirene_75110, sirene_75111, sirene_75112,
#                 sirene_75113, sirene_75114, sirene_75115, sirene_75116,
#                 sirene_75117, sirene_75118, sirene_75119, sirene_75120,
#                 sirene_92, sirene_93, sirene_94)
# rm(sirene_75101, sirene_75102, sirene_75103, sirene_75104, 
#    sirene_75105, sirene_75106, sirene_75107, sirene_75108,
#    sirene_75109, sirene_75110, sirene_75111, sirene_75112,
#    sirene_75113, sirene_75114, sirene_75115, sirene_75116,
#    sirene_75117, sirene_75118, sirene_75119, sirene_75120,
#    sirene_92, sirene_93, sirene_94)
# sirene$effectif <- as.numeric(levels(sirene$EFETCENT))[sirene$EFETCENT]
# sirene$division <- substring(sirene$APET700,1,2)

#### visualisation (au cas où)
# tmp <- sirene[1:10000,]
# m <- leaflet() %>% 
#   addTiles() %>% 
#   setView(lng = mean(tmp$longitude), lat = mean(tmp$latitude), zoom = 13) %>% 
#   addCircles(data=tmp, lat=tmp$latitude, lng=tmp$longitude, popup=tmp$L1_NORMALISEE, radius=10)
# m

#### calcul du nombre d'établissements et des effectifs par voronoi500 (et décomposition en divisions)

# sirene_voronoi500 <- over(SpatialPoints(sirene[,c("longitude","latitude")],
#                                         proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")),voronoi500)
# sirene_voronoi500 <- cbind(sirene_voronoi500,sirene)
# tmp_stations <- as.data.frame(cbind("number"=stations$number,"sirene_voronoi500"=1:nrow(stations)))
# sirene_voronoi500 <- merge.data.frame(sirene_voronoi500,tmp_stations)
# 
# tmp <- sirene_voronoi500 %>% 
#   group_by(number) %>% 
#   summarise(nombre=n(),effectif=sum(effectif,na.rm=T)) %>%
#   filter(!is.na(number))
# tmp2 <- sirene_voronoi500 %>% 
#   group_by(number, division) %>% 
#   summarise(effectif=sum(effectif,na.rm=T)) %>%
#   mutate(division_eff=gsub(" ","",paste("eff_",division), fixed = T)) %>%
#   dcast(number~division_eff,value.var="effectif") %>%
#   filter(!is.na(number))
# tmp3 <- sirene_voronoi500 %>% 
#   group_by(number, division) %>% 
#   summarise(nombre=n()) %>%
#   mutate(division_nb=gsub(" ","",paste("nb_",division), fixed = T)) %>%
#   dcast(number~division_nb,value.var="nombre") %>%
#   filter(!is.na(number))
# stations_sirene_voronoi500 <- stations %>%
#   merge(tmp,by="number",all=T) %>%
#   merge(tmp2,by="number",all=T) %>%
#   merge(tmp3,by="number",all=T)
# stations_sirene_voronoi500[,6:ncol(stations_sirene_voronoi500)][is.na(stations_sirene_voronoi500[,6:ncol(stations_sirene_voronoi500)])] <- 0
# rm(tmp,tmp2,tmp3,tmp_stations)
# 
# write.csv(stations_sirene_voronoi500,file="C:/Users/tix/Desktop/stations_sirene_voronoi500.csv")

#### calcul du nombre d'établissements et des effectifs par voronoi100 (et décomposition en divisions)

# sirene_voronoi100 <- over(SpatialPoints(sirene[,c("longitude","latitude")],
#                                         proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")),voronoi100)
# sirene_voronoi100 <- cbind(sirene_voronoi100,sirene)
# tmp_stations <- as.data.frame(cbind("number"=stations$number,"sirene_voronoi100"=1:nrow(stations)))
# sirene_voronoi100 <- merge.data.frame(sirene_voronoi100,tmp_stations)
# 
# tmp <- sirene_voronoi100 %>% 
#   group_by(number) %>% 
#   summarise(nombre=n(),effectif=sum(effectif,na.rm=T)) %>%
#   filter(!is.na(number))
# tmp2 <- sirene_voronoi100 %>% 
#   group_by(number, division) %>% 
#   summarise(effectif=sum(effectif,na.rm=T)) %>%
#   mutate(division_eff=gsub(" ","",paste("eff_",division), fixed = T)) %>%
#   dcast(number~division_eff,value.var="effectif") %>%
#   filter(!is.na(number))
# tmp3 <- sirene_voronoi100 %>% 
#   group_by(number, division) %>% 
#   summarise(nombre=n()) %>%
#   mutate(division_nb=gsub(" ","",paste("nb_",division), fixed = T)) %>%
#   dcast(number~division_nb,value.var="nombre") %>%
#   filter(!is.na(number))
# stations_sirene_voronoi100 <- stations %>%
#   merge(tmp,by="number",all=T) %>%
#   merge(tmp2,by="number",all=T) %>%
#   merge(tmp3,by="number",all=T)
# stations_sirene_voronoi100[,6:ncol(stations_sirene_voronoi100)][is.na(stations_sirene_voronoi100[,6:ncol(stations_sirene_voronoi100)])] <- 0
# rm(tmp,tmp2,tmp3,tmp_stations)
# 
# write.csv(stations_sirene_voronoi100,file="C:/Users/tix/Desktop/stations_sirene_voronoi100.csv")

#### calcul du nombre d'établissements et des effectifs par cercle500 (et décomposition en divisions)
#### sauvgarde en csv pour prendre moins de temps

# tmp_stations <- as.data.frame(cbind("number"=stations$number,"sirene_cercle500"=1:nrow(stations)))
# stations_sirene_cercle500 <- stations[0,0]
# 
# for (i in 1:length(voronoi@polygons))
# {
#   sirene_cercle500 <- over(SpatialPoints(sirene[,c("longitude","latitude")],
#                                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")),cercle500[i])
#   sirene_cercle500 <- cbind(sirene_cercle500,sirene)
#   sirene_cercle500$sirene_cercle500[sirene_cercle500$sirene_cercle500==1] <- i
#   sirene_cercle500 <- merge.data.frame(sirene_cercle500,tmp_stations)
#   
#   tmp <- sirene_cercle500 %>% 
#     group_by(number) %>% 
#     summarise(nombre=n(),effectif=sum(effectif,na.rm=T)) %>%
#     filter(!is.na(number))
#   tmp2 <- sirene_cercle500 %>% 
#     group_by(number, division) %>% 
#     summarise(effectif=sum(effectif,na.rm=T)) %>%
#     mutate(division_eff=gsub(" ","",paste("eff_",division), fixed = T)) %>%
#     dcast(number~division_eff,value.var="effectif") %>%
#     filter(!is.na(number))
#   tmp3 <- sirene_cercle500 %>% 
#     group_by(number, division) %>% 
#     summarise(nombre=n()) %>%
#     mutate(division_nb=gsub(" ","",paste("nb_",division), fixed = T)) %>%
#     dcast(number~division_nb,value.var="nombre") %>%
#     filter(!is.na(number))
#   tmp4 <- tmp %>%
#     merge(tmp2,all=T) %>%
#     merge(tmp3,all=T)
#   stations_sirene_cercle500 <- bind_rows(stations_sirene_cercle500,tmp4)
# }
# 
# stations_sirene_cercle500[,][is.na(stations_sirene_cercle500[,])] <- 0
# 
# write.csv(stations_sirene_cercle500,file="C:/Users/tix/Desktop/stations_sirene_cercle500.csv")

#### calcul du nombre d'établissements et des effectifs par cercle100 (et décomposition en divisions)
#### sauvgarde en csv pour prendre moins de temps

# tmp_stations <- as.data.frame(cbind("number"=stations$number,"sirene_cercle100"=1:nrow(stations)))
# stations_sirene_cercle100 <- stations[0,0]
# 
# for (i in 1:length(voronoi@polygons))
# {
#   sirene_cercle100 <- over(SpatialPoints(sirene[,c("longitude","latitude")],
#                                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")),cercle100[i])
#   sirene_cercle100 <- cbind(sirene_cercle100,sirene)
#   sirene_cercle100$sirene_cercle100[sirene_cercle100$sirene_cercle100==1] <- i
#   sirene_cercle100 <- merge.data.frame(sirene_cercle100,tmp_stations)
#   if (nrow(sirene_cercle100)>0){
#     tmp <- sirene_cercle100 %>%
#       group_by(number) %>%
#       summarise(nombre=n(),effectif=sum(effectif,na.rm=T)) %>%
#       filter(!is.na(number))
#     tmp2 <- sirene_cercle100 %>%
#       group_by(number, division) %>%
#       summarise(effectif=sum(effectif,na.rm=T)) %>%
#       mutate(division_eff=gsub(" ","",paste("eff_",division), fixed = T)) %>%
#       dcast(number~division_eff,value.var="effectif") %>%
#       filter(!is.na(number))
#     tmp3 <- sirene_cercle100 %>%
#       group_by(number, division) %>%
#       summarise(nombre=n()) %>%
#       mutate(division_nb=gsub(" ","",paste("nb_",division), fixed = T)) %>%
#       dcast(number~division_nb,value.var="nombre") %>%
#       filter(!is.na(number))
#     tmp4 <- tmp %>%
#       merge(tmp2,all=T) %>%
#       merge(tmp3,all=T)
#     stations_sirene_cercle100 <- bind_rows(stations_sirene_cercle100,tmp4)
#   }
#   print(i)
# }
# 
# stations_sirene_cercle100[,][is.na(stations_sirene_cercle100[,])] <- 0
# 
# write.csv(stations_sirene_cercle100,file="C:/Users/tix/Desktop/stations_sirene_cercle100.csv")


#### import des fichiers IRIS et recensement population

# IRIS <- readShapePoly("C:/Users/tix/Desktop/iris-2013-01-01.shp")
# iris_idf <- IRIS[substring(IRIS@data$DEPCOM,1,2) %in% c("75","92","93","94"),]
# iris_idf <- iris_idf[1:2749,] ###fichier en entree avec repetitions de lignes (je ne sais pas pourquoi)
# 
# recensement <- read.csv("C:/Users/tix/Desktop/base-ic-evol-struct-pop-2013.csv",sep=";")
# recensement <- recensement[recensement$DEP %in% c("75","92","93","94"),]
# colnames(recensement)[1] <- "IRIS"
# recensement <- merge(iris_idf,recensement,by.x="DCOMIRIS",by.y="IRIS")
# 
# iris_area <- as.data.frame(unlist(lapply(recensement@polygons, function(x){x@area})))
# iris_area <- cbind(iris_area,recensement@data[,"DCOMIRIS"])
# colnames(iris_area) <- c("iris_aire","DCOMIRIS")
# 
# tmp <- SpatialPolygonsDataFrame(voronoi500, 
#                                 data=data.frame(lon=stations$lon, 
#                                                 lat=stations$lat,
#                                                 number=stations$number,
#                                                 name=stations$name,
#                                                 address=stations$address,
#                                                 row.names=sapply(slot(voronoi500, 'polygons'), 
#                                                                  function(x) slot(x, 'ID'))))
# iris_voronoi500 <- intersect(recensement,tmp)
# iris_voronoi500_area <- as.data.frame(unlist(lapply(iris_voronoi500@polygons, function(x){x@area})))
# colnames(iris_voronoi500_area)[1] <- "iris_voronoi500_aire"
# iris_voronoi500_area <- cbind(iris_voronoi500_area,iris_voronoi500@data)
# iris_voronoi500_area <- merge(iris_voronoi500_area,iris_area,by.x="DCOMIRIS",by.y="DCOMIRIS")
# iris_voronoi500_area <- cbind(
#   iris_voronoi500_area[-(21:92)],
#   as.data.frame(apply(iris_voronoi500_area[21:92],2,function(x){x*iris_voronoi500_area$iris_voronoi500_aire/iris_voronoi500_area$iris_aire}))
# )
# stations_population_voronoi500 <- iris_voronoi500_area %>% group_by(number, name) %>% summarise_at(27:98,sum)
# 
# write.csv(stations_population_voronoi500,file="C:/Users/tix/Desktop/stations_population_voronoi500.csv")
# 
# 
# tmp <- SpatialPolygonsDataFrame(voronoi100, 
#                                 data=data.frame(lon=stations$lon, 
#                                                 lat=stations$lat,
#                                                 number=stations$number,
#                                                 name=stations$name,
#                                                 address=stations$address,
#                                                 row.names=sapply(slot(voronoi100, 'polygons'), 
#                                                                  function(x) slot(x, 'ID'))))
# iris_voronoi100 <- intersect(recensement,tmp)
# iris_voronoi100_area <- as.data.frame(unlist(lapply(iris_voronoi100@polygons, function(x){x@area})))
# colnames(iris_voronoi100_area)[1] <- "iris_voronoi100_aire"
# iris_voronoi100_area <- cbind(iris_voronoi100_area,iris_voronoi100@data)
# iris_voronoi100_area <- merge(iris_voronoi100_area,iris_area,by.x="DCOMIRIS",by.y="DCOMIRIS")
# iris_voronoi100_area <- cbind(
#   iris_voronoi100_area[-(21:92)],
#   as.data.frame(apply(iris_voronoi100_area[21:92],2,function(x){x*iris_voronoi100_area$iris_voronoi100_aire/iris_voronoi100_area$iris_aire}))
# )
# stations_population_voronoi100 <- iris_voronoi100_area %>% group_by(number, name) %>% summarise_at(27:98,sum)
# 
# write.csv(stations_population_voronoi100,file="C:/Users/tix/Desktop/stations_population_voronoi100.csv")
# 
# 
# stations_population_cercle500 <- stations[0,0]
# for(i in 1:length(voronoi@polygons))
# {
#   tmp <- SpatialPolygonsDataFrame(cercle500[i], 
#                                   data=data.frame(lon=stations$lon[i], 
#                                                   lat=stations$lat[i],
#                                                   number=stations$number[i],
#                                                   name=stations$name[i],
#                                                   address=stations$address[i],
#                                                   row.names=sapply(slot(cercle500[i], 'polygons'), 
#                                                                    function(x) slot(x, 'ID'))))
#   iris_cercle500 <- intersect(recensement,tmp)
#   iris_cercle500_area <- as.data.frame(unlist(lapply(iris_cercle500@polygons, function(x){x@area})))
#   colnames(iris_cercle500_area)[1] <- "iris_cercle500_aire"
#   iris_cercle500_area <- cbind(iris_cercle500_area,iris_cercle500@data)
#   iris_cercle500_area <- merge(iris_cercle500_area,iris_area,by.x="DCOMIRIS",by.y="DCOMIRIS")
#   iris_cercle500_area <- cbind(
#     iris_cercle500_area[-(21:92)],
#     as.data.frame(apply(iris_cercle500_area[21:92],2,function(x){x*iris_cercle500_area$iris_cercle500_aire/iris_cercle500_area$iris_aire}))
#   )
#   tmp4 <- iris_cercle500_area %>% group_by(number, name) %>% summarise_at(27:98,sum)
#   stations_population_cercle500 <- bind_rows(stations_population_cercle500,tmp4)
#   print(i)
# }
# 
# write.csv(stations_population_cercle500,file="C:/Users/tix/Desktop/stations_population_cercle500.csv")
# 
# stations_population_cercle100 <- stations[0,0]
# for(i in 1:length(voronoi@polygons))
# {
#   tmp <- SpatialPolygonsDataFrame(cercle100[i], 
#                                   data=data.frame(lon=stations$lon[i], 
#                                                   lat=stations$lat[i],
#                                                   number=stations$number[i],
#                                                   name=stations$name[i],
#                                                   address=stations$address[i],
#                                                   row.names=sapply(slot(cercle100[i], 'polygons'), 
#                                                                    function(x) slot(x, 'ID'))))
#   iris_cercle100 <- intersect(recensement,tmp)
#   iris_cercle100_area <- as.data.frame(unlist(lapply(iris_cercle100@polygons, function(x){x@area})))
#   colnames(iris_cercle100_area)[1] <- "iris_cercle100_aire"
#   iris_cercle100_area <- cbind(iris_cercle100_area,iris_cercle100@data)
#   iris_cercle100_area <- merge(iris_cercle100_area,iris_area,by.x="DCOMIRIS",by.y="DCOMIRIS")
#   if(nrow(iris_cercle100_area) > 1)
#   {
#     iris_cercle100_area <- cbind(
#       iris_cercle100_area[-(21:92)],
#       as.data.frame(apply(iris_cercle100_area[21:92],2,function(x){x*iris_cercle100_area$iris_cercle100_aire/iris_cercle100_area$iris_aire}))
#     )
#   }
#   if(nrow(iris_cercle100_area) == 1)
#   {
#     iris_cercle100_area <- cbind(
#       iris_cercle100_area[-(21:92)],
#       as.data.frame(t(apply(iris_cercle100_area[21:92],2,function(x){x*iris_cercle100_area$iris_cercle100_aire/iris_cercle100_area$iris_aire})))
#     )
#   }
#   tmp4 <- iris_cercle100_area %>% group_by(number, name) %>% summarise_at(27:98,sum)
#   stations_population_cercle100 <- bind_rows(stations_population_cercle100,tmp4)
#   print(i)
# }
# 
# write.csv(stations_population_cercle100,file="C:/Users/tix/Desktop/stations_population_cercle100.csv")

stations_sirene_voronoi500 <- read.csv("E:/Data/stations_sirene_voronoi500.csv")[,-1]
stations_sirene_voronoi100 <- read.csv("E:/Data/stations_sirene_voronoi100.csv")[,-1]
stations_sirene_cercle500 <- read.csv("E:/Data/stations_sirene_cercle500.csv")[,-1]
stations_sirene_cercle500 <- merge(stations,stations_sirene_cercle500)
stations_sirene_cercle100 <- read.csv("E:/Data/stations_sirene_cercle100.csv")[,-1]
stations_sirene_cercle100 <- merge(stations,stations_sirene_cercle100,all=T)
stations_sirene_cercle100[,][is.na(stations_sirene_cercle100[,])] <- 0
stations_population_voronoi500 <- read.csv("E:/Data/stations_population_voronoi500.csv")[,-1]
stations_population_voronoi500 <- merge(stations,stations_population_voronoi500)
stations_population_voronoi100 <- read.csv("E:/Data/stations_population_voronoi100.csv")[,-1]
stations_population_voronoi100 <- merge(stations,stations_population_voronoi100)
stations_population_cercle500 <- read.csv("E:/Data/stations_population_cercle500.csv")[,-1]
stations_population_cercle500 <- merge(stations,stations_population_cercle500)
stations_population_cercle100 <- read.csv("E:/Data/stations_population_cercle100.csv")[,-1]
stations_population_cercle100 <- merge(stations,stations_population_cercle100)

voronoi500_aires <- unlist(lapply(voronoi500@polygons, function(x){x@area}))
tmp_id <- unlist(lapply(voronoi500@polygons, function(x){x@ID}))
voronoi500_aires <- as.data.frame(cbind(voronoi500_aires,tmp_id))
colnames(voronoi500_aires) <- c("aire","number")
voronoi500_aires$aire <- 
  as.numeric(levels(voronoi500_aires$aire))[voronoi500_aires$aire]  * pi * 0.1 * 0.1 / 3.766366e-06
stations_sirene_voronoi500_densite <- merge(voronoi500_aires,stations_sirene_voronoi500)
stations_sirene_voronoi500_densite <- stations_sirene_voronoi500_densite[,7:ncol(stations_sirene_voronoi500_densite)] / 
  (stations_sirene_voronoi500_densite$aire)
stations_sirene_voronoi500_densite <- cbind(voronoi500_aires, stations_sirene_voronoi500_densite)

stations_population_voronoi500_densite <- merge(voronoi500_aires,stations_population_voronoi500)
stations_population_voronoi500_densite <- stations_population_voronoi500_densite[,7:ncol(stations_population_voronoi500_densite)] / 
  (stations_population_voronoi500_densite$aire)
stations_population_voronoi500_densite <- cbind(voronoi500_aires, stations_population_voronoi500_densite)

write.csv(stations_population_voronoi500_densite,file="E:/Data/stations_population_voronoi500_densite.csv")
write.csv(stations_population_voronoi500_densite,file="E:/Data/stations_sirene_voronoi500_densite.csv")

