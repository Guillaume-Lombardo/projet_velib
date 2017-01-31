library(jsonlite)
require(curl)
library(cluster)
library(RgoogleMaps)

#r?cup?ration des donn?es json pr?alablement sauvegard?es pour acc?lerer grandement le chargement
url="http://vlsstats.ifsttar.fr/rawdata/RawData/data_all_Paris.jjson"
url2="C:/FB/Data scientist/Deuxieme session/TP velib/data_all_Paris.jjson"
data<-fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=",")))

#r?cup?ration des stations
url="http://vlsstats.ifsttar.fr/data/input_Paris.json"
url2="C:/FB/Data scientist/Deuxieme session/TP velib/input_Paris.json"
stations<-fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=",")))
stations<-stations[[1]]
stations$lat<-stations$position$lat
stations$lon<-stations$position$lng

coordstations <- merge(as.data.frame(data[1]), stations[,c(1,13,14)], by="number", all.x=T)
coordstations <- coordstations[,c(1,9,10)]
#on n'a pas les coordonn?es de certaines stations donc on va les enlever pour l'analyse (surtout l'analyse g?ographique)
lignemanquantecoord<-which(is.na(coordstations[,2]))
#coordstations<-coordstations[-lignemanquantecoord,]
coordstations[lignemanquantecoord,c(2)]<-mean(coordstations[,2], na.rm = T)
coordstations[lignemanquantecoord,c(3)]<-mean(coordstations[,3], na.rm = T)



#construction d'une matrice
#en ligne les stations
#en colonne, une variable ? diff?rentes date, ici la proportion de v?lo disponible
ndate <- length(data)
nstations <- length(data[[1]]$number)
matvelib<-matrix(nrow = nstations, ncol = ndate)
listestations<-c(1:nstations)
listestations<-listestations[-lignemanquantecoord]
for (i in listestations){
  for (j in 1:ndate){
      matvelib[i,j]<-data[[j]][i,8]/data[[j]][i,4]
  }
}
matvelib[which(is.na(matvelib))]<-0

#CAH
CAH <- agnes(matvelib, diss=F, metric="euclidian", stand=T, method="ward")
plot(CAH)

#k-means
reskmeans<-kmeans(matvelib, centers = 4, nstart=1)
summary(reskmeans)
reskmeans$cluster

#repr?sentation graphique sur la carte de Paris
center<-c(mean(range(coordstations[,2])),mean(range(coordstations[,3])))
zoom<-MaxZoom(range(coordstations[,2])*1.1,range(coordstations[,3])*1.1 )
carte<-GetMap(center=center, zoom=zoom)
PlotOnStaticMap(carte, lat=coordstations[,2], lon=coordstations[,3], pch=16, cex=1, col=as.numeric(reskmeans$cluster))




#cr?ation d'une liste de dataframe de stations.
#Pour chaque station, on va avoir le nom de la station, la date de l'update et la proportion de v?los disponibles
N <- nrow(data[[1]])
listetempstation <- vector('list', N)

for (i in 1:N)
{
  temp <- as.data.frame(data[[1]][i,6])
  colnames(temp)<-c("update")
  temp$prop <- data[[1]][i,8]/data[[1]][i,4]
  listetempstation[[i]] <- temp
}

for (j in 2:length(data))
{
  for (i in 1:N)
  {
    temp <- as.data.frame(data[[j]][i,6])
    colnames(temp)<-c("update")
    temp$prop <- as.numeric(data[[j]][i,8]/data[[j]][i,4])
    listetempstation[[i]]=rbind.data.frame(listetempstation[[i]],temp)
  }
}


#pour chaque dataframe de stations, on convertit la date en un format plus lisible 
#et on ajoute jour, heures et minutes
for (j in 1:length(listetempstation))
{
  listetempstation[[j]]$time <- as.POSIXct(listetempstation[[j]]$update/1000, origin="1970-01-01")
  listetempstation[[j]]$jour <- format(listetempstation[[j]]$time, "%w")
  listetempstation[[j]]$heure <- as.numeric(format(listetempstation[[j]]$time, "%H"))
  listetempstation[[j]]$minute <-  as.numeric(format(listetempstation[[j]]$time, "%M"))
  listetempstation[[j]]$minute <- floor(listetempstation[[j]]$minute/30)*30
  }


#cr?ation d'un dataframe histostation
#les colonnes : les stations
#les lignes :  la proportion moyenne suivant les jours et heures
#revhistostation est la transpos?e
histostation<-aggregate(listetempstation[[1]]$prop, by=list(listetempstation[[1]]$jour, listetempstation[[1]]$heure, listetempstation[[1]]$minute), FUN=mean)
for (j in 2:length(listetempstation))
{
  temp <- aggregate(listetempstation[[j]]$prop, by=list(listetempstation[[j]]$jour, listetempstation[[j]]$heure, listetempstation[[j]]$minute), FUN=mean)
  histostation <- merge(histostation,temp, by=c(1,2,3), all.x=T)
}
histostation<-histostation[order(histostation[,1],histostation[,2],histostation[,3]),]
histostation <- histostation[,-c(1:3)]
histostation <- as.matrix(histostation)
histostation[which(is.na(histostation))]<-0
histostation<-as.data.frame(histostation)

revhistostation <- as.data.frame(t(as.matrix(histostation)))


#summary(histostation)

plot(histostation[,100], type ="l")
lines(histostation[,6])



#CAH
CAH <- agnes(histostation, diss=F, metric="euclidian", stand=T, method="ward")
plot(CAH)

#k-means

nkmeans <- 6
reskmeans2<-kmeans(as.matrix(revhistostation), centers = nkmeans, nstart=1)
#summary(reskmeans)
#reskmeans2$cluster

i<-1
moyenneparstation <-as.data.frame(rowMeans(histostation[,which(reskmeans2$cluster==i)]))
for (i in 2:nkmeans)
{
  moyenneparstation <- cbind.data.frame(moyenneparstation,as.data.frame(rowMeans(histostation[,which(reskmeans2$cluster==i)])))
}
colnames(moyenneparstation)<-1:nkmeans

plot(moyenneparstation[,1], type ="l", ylim=c(0,1), col=1)
for (i in 2:nkmeans)
{
  lines(moyenneparstation[,i], col=i)
}

#repr?sentation graphique sur la carte de Paris
center<-c(mean(range(coordstations[,2])),mean(range(coordstations[,3])))
zoom<-11
carte<-GetMap(center=center, zoom=zoom)
PlotOnStaticMap(carte, lat=coordstations[,2], lon=coordstations[,3], pch=16, cex=1, col=as.numeric(reskmeans2$cluster))





















########################################################
# On refait la m?me chose mais en prenant
# une observation de 1-k de l'?chantillon et on compare
# les pr?visions sur l'?chantillon de taille k observ?
#########################################################

#on a nobs=lenght(data) observations
#on va s?parer ces nobs observations en 10
K<-10
nobs <- length(data)
nobs10 <- ceiling(nobs/K)
partitionobs <- 1:nobs
partition <- sample(nobs)
listepartition <- vector('list', K)
for (i in 1:(K-1))
{
  listepartition[[i]]<-partition[((i-1)*nobs10+1):(i*nobs10)]
}
listepartition[[K]]<-partition[((K-1)*nobs10+1):nobs]


# pour chaque i, on va comparer le mod?le sur les obs avec l'?lement non pris en compte
ipartition<-1
partitionobs <-  partitionobs[-listepartition[[ipartition]]]
partitionprev <- listepartition[[ipartition]]

#cr?ation d'une liste de dataframe de stations.
#Pour chaque station, on va avoir le nom de la station, la date de l'update et la proportion de v?los disponibles
N <- nrow(data[[1]])
listetempstationobs <- vector('list', N)
listetempstationprev <- vector('list', N)

for (i in 1:N)
{
  temp <- as.data.frame(data[[partitionobs[1]]][i,6])
  colnames(temp)<-c("update")
  temp$prop <- data[[partitionobs[1]]][i,8]/data[[partitionobs[1]]][i,4]
  listetempstationobs[[i]] <- temp
  
  temp <- as.data.frame(data[[partitionprev[1]]][i,6])
  colnames(temp)<-c("update")
  temp$prop <- data[[partitionprev[1]]][i,8]/data[[partitionprev[1]]][i,4]
  listetempstationprev[[i]] <- temp  
}

for (j in partitionobs[-1])
{
  for (i in 1:N)
  {
    temp <- as.data.frame(data[[j]][i,6])
    colnames(temp)<-c("update")
    temp$prop <- as.numeric(data[[j]][i,8]/data[[j]][i,4])
    listetempstationobs[[i]]=rbind.data.frame(listetempstationobs[[i]],temp)
  }
}

for (j in partitionprev[-1])
{
  for (i in 1:N)
  {
    temp <- as.data.frame(data[[j]][i,6])
    colnames(temp)<-c("update")
    temp$prop <- as.numeric(data[[j]][i,8]/data[[j]][i,4])
    listetempstationprev[[i]]=rbind.data.frame(listetempstationprev[[i]],temp)
  }
}

#pour chaque dataframe de stations, on convertit la date en un format plus lisible 
#et on ajoute jour, heures et minutes
for (j in 1:length(listetempstationobs))
{
  listetempstationobs[[j]]$time <- as.POSIXct(listetempstationobs[[j]]$update/1000, origin="1970-01-01")
  listetempstationobs[[j]]$jour <- format(listetempstationobs[[j]]$time, "%w")
  listetempstationobs[[j]]$heure <- as.numeric(format(listetempstationobs[[j]]$time, "%H"))
  listetempstationobs[[j]]$minute <-  as.numeric(format(listetempstationobs[[j]]$time, "%M"))
  listetempstationobs[[j]]$minute <- floor(listetempstationobs[[j]]$minute/30)*30
}
for (j in 1:length(listetempstationprev))
{
  listetempstationprev[[j]]$time <- as.POSIXct(listetempstationprev[[j]]$update/1000, origin="1970-01-01")
  listetempstationprev[[j]]$jour <- format(listetempstationprev[[j]]$time, "%w")
  listetempstationprev[[j]]$heure <- as.numeric(format(listetempstationprev[[j]]$time, "%H"))
  listetempstationprev[[j]]$minute <-  as.numeric(format(listetempstationprev[[j]]$time, "%M"))
  listetempstationprev[[j]]$minute <- floor(listetempstationprev[[j]]$minute/30)*30
}

#cr?ation d'un dataframe histostation
#les colonnes : les stations
#les lignes :  la proportion moyenne suivant les jours et heures
#revhistostation est la transpos?e
histostationobs<-aggregate(listetempstationobs[[1]]$prop, by=list(listetempstationobs[[1]]$jour, listetempstationobs[[1]]$heure, listetempstationobs[[1]]$minute), FUN=mean)
for (j in 2:length(listetempstationobs))
{
  temp <- aggregate(listetempstationobs[[j]]$prop, by=list(listetempstationobs[[j]]$jour, listetempstationobs[[j]]$heure, listetempstationobs[[j]]$minute), FUN=mean)
  histostationobs <- merge(histostationobs,temp, by=c(1,2,3), all.x=T)
}
histostationobs<-histostationobs[order(histostationobs[,1],histostationobs[,2],histostationobs[,3]),]



histostationprev<-aggregate(listetempstationprev[[1]]$prop, by=list(listetempstationprev[[1]]$jour, listetempstationprev[[1]]$heure, listetempstationprev[[1]]$minute), FUN=mean)
for (j in 2:length(listetempstationprev))
{
  temp <- aggregate(listetempstationprev[[j]]$prop, by=list(listetempstationprev[[j]]$jour, listetempstationprev[[j]]$heure, listetempstationprev[[j]]$minute), FUN=mean)
  histostationprev <- merge(histostationprev,temp, by=c(1,2,3), all.x=T)
}
histostationprevtot<-merge(histostationobs[,1:3],histostationprev, by=c(1,2,3), all.x=T)
lignepresenteprv <- !is.na(histostationprevtot[,4])

diff <- histostationobs[lignepresenteprv,-(1:3)]-histostationprevtot[lignepresenteprv,-(1:3)]
diff <- diff*diff
sum(diff, na.rm=T)



#la diff?rence en prenant comme pr?diction la moyenne des stations de m?me type (kmeans2)

reskmeans2$cluster
moyenneparstation
#boucle sur toutes les stations
#NON VERIFIE !!!!!!!!!!!!!!!!
diffm <- 0
for (i in 1:nrow(data[1]))
{
  diffm <- moyenneparstation[lignepresenteprv,reskmeans2$cluster[i]]-histostationprevtot[lignepresenteprv,3+i]
  diffm <- diffm*diffm
  sum(diffm, na.rm=T)
}
