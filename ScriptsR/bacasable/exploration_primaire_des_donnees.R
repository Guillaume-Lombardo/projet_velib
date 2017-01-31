library(jsonlite)

# taille des objets en memoire : 
# sapply(ls(),function(x) format(object.size(get(x)),'auto'))

url="http://vlsstats.ifsttar.fr/rawdata/RawData/data_all_Paris.jjson"
url2="D:/projet_velib/Data/data_all_Paris.jjson_2017-01-01-1483248351"
data<-fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=",")))


url_station="http://vlsstats.ifsttar.fr/data/input_Paris.json"
url2_station="D:/projet_velib/Data/input_Paris.json"
stations<-fromJSON(sprintf("[%s]", paste(readLines(url2), collapse=",")))
stations<-stations[[1]]
stations$lat<-stations$position$lat
stations$lon<-stations$position$lng