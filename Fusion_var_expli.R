var_expli1<-read.csv("Sortie/Jeuvarexpli.csv", sep=",")
var_expli1$X<-NULL
var_expli1$name<-NULL
var_expli1$address<-NULL
var_expli1$banking<-NULL
var_expli1$status<-NULL
var_expli1$contract_name<-NULL
var_expli1$available_bike_stands<-NULL
var_expli1$available_bikes<-NULL
var_expli1$last_update<-NULL
var_expli2<-read.csv("Sortie/stations_population_voronoi500.csv", sep=",")
var_expli2$X<-NULL
var_expli2$name<-NULL
var_expli3<-read.csv("Sortie/stations_sirene_voronoi500.csv", sep=",")
var_expli3$X<-NULL
var_expli3$name<-NULL
var_expli3$address<-NULL

var_expli<-merge(x=var_expli1, y=var_expli2, by="number")
var_expli<-merge(x=var_expli, y=var_expli3, by="number")

write.csv(var_expli, file = "Sortie/Jeuvarexplifinal.csv")
