#cr?ation du jeu de variable explicative final
#en fusionnant les diff?rentes sources

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
var_expli1$lat<-NULL
var_expli1$lon<-NULL
var_expli2<-read.csv("Sortie/stations_population_voronoi500_densite.csv", sep=",")
var_expli2$X<-NULL
var_expli2$name<-NULL
var_expli3<-read.csv("Sortie/stations_sirene_voronoi500_densite.csv", sep=",")
var_expli3$X<-NULL
var_expli3$name<-NULL
var_expli3$aire<-NULL


var_expli<-merge(x=var_expli1, y=var_expli2, by="number")
var_expli<-merge(x=var_expli, y=var_expli3, by="number")
write.csv(var_expli, file = "Sortie/Jeuvarexplifinal.csv")


# Cr?ation d'un jeu de variables explicatives apr?s ACP
library(FactoMineR)
library(corrplot)

#visualisation de la correlation
Mcor<-cor(var_expli[,2:61])
corrplot.mixed(Mcor)
corrplot(Mcor, type="upper")

#r?alisation de l'ACP
var_expli$bonus<-as.numeric(var_expli$bonus)
res.pca = PCA(var_expli[,-1], scale.unit=TRUE,ncp=(ncol(var_expli)-1) ,graph=T, axes=c(1,2))

res.pca$eig$`cumulative percentage of variance`
var_expli_ACP<-as.data.frame(cbind(var_expli[,1],res.pca$ind$coord))
colnames(var_expli_ACP)[1]<-"number"

limcos=0.2
layout(matrix(1:1,nrow=1,byrow=F))
plot.PCA(res.pca, axes=c(1, 2), choix="var", lim.cos2.var=limcos)
plot.PCA(res.pca, axes=c(3, 4), choix="var", lim.cos2.var=limcos)
plot.PCA(res.pca, axes=c(5, 6), choix="var", lim.cos2.var=limcos)

colnames(var_expli_ACP)[2:11]<-c("densité pop", "densité entreprises", "centralité", "densité pop par âge", "commerces", "Restau et cité U", "commerces", "effectif salarié", "RATP", "eff 0 et eff 03")
write.csv(var_expli_ACP, file = "Sortie/JeuvarexplifinalACP.csv")
