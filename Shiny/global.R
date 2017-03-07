library(shiny)
library(rAmCharts)
library(leaflet)
library(glmnet)




#data (1 ligne par station) doit contenir les variables à afficher et la variable number
#data doit contenir les variables à afficher et la variable number
#stations doit contenir number, lat, lon, name
#polygones doit être un SpatialPolygons
#var_polygone est la variable représentée (couleurs polygones), doit être dans data
#var_point est pour colorer les points
#lbl_var_polygone : libellé de la variable pour la légende
#lbl_var_point : libellé de la variable pour les points
afficher_carte <- function(data, stations, polygones, var_polygone, var_point=NULL, lbl_var_polygone, lbl_var_point)
{
  zzz <- data
  rownames(zzz) <- zzz$number
  nrow(zzz)
  eval(parse(text = paste('palll1 <- leaflet::colorNumeric("viridis",domain=zzz$',var_polygone,',reverse=T)',sep = '')))
  
  spdf <- SpatialPolygonsDataFrame(polygones, zzz)
  
  parse_leaflet <- 'carte <- leaflet(spdf) %>%'
  parse_tiles <- 'addProviderTiles(providers$Esri.WorldTopoMap) %>%'
  parse_view <- 'setView(lng = mean(stations$lon), lat = mean(stations$lat), zoom = 12) %>%'
  parse_circles <- 'addCircles(data=stations, lat=stations$lat, lng=stations$lon, radius=5,color="navy") %>%'
  parse_scale <- 'addScaleBar(options = scaleBarOptions(imperial=F)) %>%'
  parse_minipap <- 'addMiniMap(tiles = providers$Esri.WorldTopoMap,toggleDisplay = TRUE) %>%'
  parse_polygon <- paste('addPolygons(data=spdf,label=stations$name,',
                         ' layerId=stations$number,color="red",opacity=0.3,fillColor=palll1(zzz$',
                         var_polygone,
                         '),fillOpacity=0.4,weight=1,',
                         'highlightOptions = highlightOptions(color = "black", weight = 4,bringToFront = TRUE)) %>%',sep = '')
  parse_legend <- paste('addLegend(position ="topright",pal=palll1,values=zzz$',var_polygone,',title="',lbl_var_polygone,'")',sep = '')
  
  if (missing(var_point)) 
  {
    parse_tout <- paste(parse_leaflet,parse_tiles,parse_view,parse_circles,parse_scale,parse_minipap,parse_polygon, parse_legend)
    carte <- eval(parse(text = parse_tout))
    return(carte)
  }
  if (!missing(var_point)) 
  {
    eval(parse(text = paste('palll2 <- leaflet::colorNumeric("Accent",domain=zzz$',var_point,',reverse=T)',sep = '')))
    parse_circles2 <- paste('addCircles(data=stations, lat=stations$lat, lng=stations$lon, radius=5,color=palll2(zzz$',var_point,')) %>%',sep = '')
    parse_legend2 <- paste(' %>% addLegend(position ="topright",pal=palll2,values=zzz$',var_point,',title="',lbl_var_point,'")',sep = '')
    parse_tout2 <- paste(parse_leaflet,parse_tiles,parse_view,parse_circles2,parse_scale,parse_minipap,parse_polygon, parse_legend,parse_legend2)
    carte <- eval(parse(text = parse_tout2))
    return(carte)
  } 
}

####exemple
# afficher_carte(data=read.csv("../Sortie/stations_population_voronoi500_densite.csv"),
#                polygones=readRDS("voronoi500.rds"),
#                stations=read.csv(file="../Sortie/stations_sirene_voronoi500.csv")[,2:6],
#                var_polygone="P13_POP",
#                lbl_var_polygone="Densité</br>(hab/km²)")


representation_kmeans <- readRDS(file = '../Sortie/representation_kmeans.RDS')
stations_colonnes <- readRDS(file = '../Sortie/stations_colonnes.RDS')
eval(parse(text = paste("profil_colonnes_",1:10," <- readRDS(",
                        "file = '../Sortie/profil_colonnes_",1:10,".RDS')", sep = '')))