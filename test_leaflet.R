url="http://vlsstats.ifsttar.fr/rawdata/RawData/data_all_Paris.jjson"
data<-fromJSON(sprintf("[%s]", paste(readLines(url), collapse=",")))
data[[10]]

url="http://vlsstats.ifsttar.fr/data/input_Paris.json"
stations<-fromJSON(sprintf("[%s]", paste(readLines(url), collapse=",")))
stations<-stations[[1]]
stations$lat<-stations$position$lat
stations$lon<-stations$position$lng

fulldt <-as.data.frame(data[1])
fulldt<-merge(fulldt, stations[,c(1,2,13,14)],by="number")

fulldt$prop<-data[[1]][8]/data[[1]][4]
#for (i in 2:length(data))
for (i in 2:20)
{
  temp<-as.data.frame(data[i])
  temp$prop<-data[[i]][8]/data[[i]][4]
  fulldt <- merge(fulldt, temp[,c(5,9)], by="number")
}


#CAH
t2[[2]]

t2<-fulldt[,-(1:11)]
t3<-as.data.frame(lapply(t2, FUN=cbind))
t4<-as.matrix(t3)

library(cluster)

#k-means

reskmeans<-kmeans(t4, centers = 3, nstart=1)
summary(reskmeans)
reskmeans$cluster



library(leaflet)
pal <- colorFactor(c("navy", "red", "yellow"), domain = 1:3)
m <- leaflet() %>% addTiles()
m %>% setView(lng = mean(fulldt[,11]), lat = mean(fulldt[,10]), zoom = zoom)
m %>% addCircles(data=fulldt, lat=fulldt[,10], lng=fulldt[,11], color=pal(as.numeric(reskmeans$cluster)), popup=stations$name, radius=75)
m %>% addLegend("bottomleft", pal=pal, values=as.numeric(reskmeans$cluster), title="Classes",opacity=0.5)
