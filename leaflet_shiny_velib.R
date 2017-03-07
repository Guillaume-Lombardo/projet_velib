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
library(leaflet)
library(rgeos)
library(tibble)
library(viridis)
library(highcharter)

#### gestion des polygones
#### 4 versions : voronoi100, voronoi500, cercle100, cercle500
#### à lancer par ailleurs

yyy <- unlist(lapply(voronoi500@polygons, function(x){x@area}))
xxx <- unlist(lapply(voronoi500@polygons, function(x){x@ID}))
xxx <- as.data.frame(cbind(xxx,yyy))
colnames(xxx) <- c("number","aire")

zzz <- merge(xxx,stations_population_voronoi500,by="number")
rownames(zzz) <- zzz$number
zzz$aire <- as.numeric(levels(zzz$aire))[zzz$aire] * pi * 0.1 * 0.1 / 3.766366e-06
zzz$densite <- zzz$P13_POP / zzz$aire
zzzz <- SpatialPolygonsDataFrame(voronoi500, zzz)

palll <- leaflet::colorNumeric("viridis",domain=zzz$densite,reverse=T)

ui <- fluidPage(
  titlePanel("Vélib"),
  fluidRow(
    column(1,selectInput("NbClasses", "Nombre de classes (k) : ", c(2:10))),
    column(6,leafletOutput("map",height=800)),
    column(3,highchartOutput("grph",height=200)),
    column(1,wellPanel(
      textOutput("coords")
    )),
    column(1,wellPanel(
      textOutput("summary")
    ))
  )
)

server <- function(input, output, session) {
  
  sd <- SharedData$new(zzz)
  
  output$map <- renderLeaflet({
    leaflet(sd) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = mean(stations$lon), lat = mean(stations$lat), zoom = 12) %>%
      addCircles(data=stations, lat=stations$lat, lng=stations$lon, radius=5,color="navy") %>%
      addCircleMarkers(opacity = 0, fillOpacity = 0) %>%
      addPolygons(data=zzzz,
                  label=stations$name, layerId=stations$number,
                  color="red",opacity=0.3,fillColor=~palll(densite),
                  fillOpacity=0.4,weight=1,
                  highlightOptions = highlightOptions(color = "black", weight = 4,
                                                      bringToFront = TRUE)) %>%
      addScaleBar(options = scaleBarOptions(imperial=F)) %>%
      addMiniMap(tiles = providers$Esri.WorldTopoMap,
                 toggleDisplay = TRUE) %>%
      addLegend(position = 'topright',pal=palll,values=zzz$densite,title="Densité<br/>(hab/km²)")
  }) 
  
  
  observeEvent(input$map_shape_click, {
    
    #create object for clicked polygon
    click <- input$map_shape_click
    print(click)
    dgr <- arrange(coordstations[coordstations$number == click$id,],time)

    output$coords <- renderText({ 
      paste("ID : ", click$id, ", latitude : ", click$lat, ", longitude : ", click$lng,
            ", obs : ", nrow(dgr))
    })
    # output$grph <- renderHighchart({
    #   hchart(dgr, type = "line",
    #          hcaes(x = last_update, y = proportion, group=number))
    # })
  })
  
  output$summary <- renderText({
    df <- sd$data(withSelection = TRUE) %>%
      filter(selected_ | is.na(selected_)) %>%
      mutate(selected_ = NULL)
    paste("Nb select : ", nrow(df))
  })
  
  # df <- sd$data(withSelection = TRUE) %>%
  #   filter(selected_ | is.na(selected_)) %>%
  #   mutate(selected_ = NULL)
  # if nrow(df)<5
  # {
  #   rgrse <- as.numeric(levels(df[,1]))[df[,1]]
  #   dgr <- arrange(coordstations[coordstations$number %in% rgrse,],time)
  #   output$grph <- renderHighchart({hchart(dgr, type = "line",hcaes(x = last_update, y = proportion, group=number))})
  # }
}

shinyApp(ui, server)
