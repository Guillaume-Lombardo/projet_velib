
shared_ <- SharedData$new(zzz)

output$B_plot <- renderPlotly({
  hm
})

output$event <- renderPrint({
  d <- event_data("plotly_click")
  if (is.null(d)) "Cliquez!" else paste(listvar[d$x], ", ", listvar[d$y])
})

observe({
  d <- event_data("plotly_click")
  if (!is.null(d)) {
    updateSelectInput(session, "B_var1_test", selected = liste_variables_exp[liste_variables_exp$Variable==listvar[d$x],3])
    updateSelectInput(session, "B_var2_test", selected = liste_variables_exp[liste_variables_exp$Variable==listvar[1+length(listvar)-d$y],3])
  }
})

output$B_boxplot1 <- renderAmCharts({
  B_var1_char <- as.character(input$B_var1_test)
  var_popppp1 <- liste_variables_exp[liste_variables_exp$lbl2==B_var1_char,1]
  eval(parse(text = paste('amBoxplot(',
                          var_popppp1,
                          '~cluster_',
                          input$B_nb_cluster,
                          ', data=zzz, col=brewer.pal(',
                          input$B_nb_cluster,
                          ', "Accent"), main="',
                          var_popppp1,
                          ' sur ',
                          input$B_nb_cluster,
                          ' classes")',
                          sep = '')))
}) 

output$B_boxplot2 <- renderAmCharts({
  B_var2_char <- as.character(input$B_var2_test)
  var_popppp2 <- liste_variables_exp[liste_variables_exp$lbl2==B_var2_char,1]
  eval(parse(text = paste('amBoxplot(',
                          var_popppp2,
                          '~cluster_',
                          input$B_nb_cluster,
                          ', data=zzz, col=brewer.pal(',
                          input$B_nb_cluster,
                          ', "Accent"), main="',
                          var_popppp2,
                          ' sur ',
                          input$B_nb_cluster,
                          ' classes")',
                          sep = '')))
}) 


output$B_gr1 <- renderPlotly({
  B_var1_char <- as.character(input$B_var1_test)
  var_popppp1 <- liste_variables_exp[liste_variables_exp$lbl2==B_var1_char,1]
  B_var2_char <- as.character(input$B_var2_test)
  var_popppp2 <- liste_variables_exp[liste_variables_exp$lbl2==B_var2_char,1]
  eval(parse(text = paste('plot_ly(shared_,x = ~',
                          var_popppp1,
                          ',y = ~',
                          var_popppp2,
                          ',color = ~factor(cluster_',
                          input$B_nb_cluster,
                          '), colors = brewer.pal(',
                          input$B_nb_cluster,
                          ', "Accent"))',
                          sep = '')))
})

output$B_map1 <- renderLeaflet({
  B_var1_char <- as.character(input$B_var1_test)
  var_popppp <- liste_variables_exp[liste_variables_exp$lbl2==B_var1_char,1]
  #var_popppp <- liste_variables_exp[liste_variables_exp$lbl==B_var2_char,1]
  
  eval(parse(text = paste('palll1 <- leaflet::colorNumeric("viridis",domain=zzz$',var_popppp,',reverse=T)',sep = '')))
  parse1 <- 'leaflet(shared_) %>%'
  parse2 <- 'addProviderTiles(providers$Esri.WorldTopoMap) %>%'
  parse3 <- 'setView(lng = mean(stations1199$lon), lat = mean(stations1199$lat), zoom = 12) %>%'
  parse4 <- 'addMarkers(layerId=stations1199$number, group="picto", icon = makeIcon(iconUrl = "./picto_velib.png", iconWidth = 10, iconHeight = 10, iconAnchorX = 5, iconAnchorY = 5), options = markerOptions(opacity=0.2)) %>%'
  parse5 <- 'addScaleBar(options = scaleBarOptions(imperial=F)) %>%'
  parse6 <- paste('addPolygons(data=spdf,label=stations1199$name,layerId=stations1199$number,color="red",opacity=0.3,fillColor=palll1(zzz$',
                  var_popppp,
                  '),fillOpacity=0.4,weight=1, highlightOptions = highlightOptions(color = "black", weight = 4,bringToFront = TRUE)) %>%')
  parse7 <- paste('addLegend(position ="topright",pal=palll1,values=zzz$',
                  var_popppp,
                  ') %>%')
  parse8 <- 'addLayersControl(overlayGroups = c("picto"))'
  eval(parse(text = paste(parse1,parse2,parse3,parse4,parse5,parse6,parse7,parse8)))
  
})


output$B_map2 <- renderLeaflet({
  B_var2_char <- as.character(input$B_var2_test)
  var_popppp <- liste_variables_exp[liste_variables_exp$lbl2==B_var2_char,1]
  eval(parse(text = paste('palll1 <- leaflet::colorNumeric("viridis",domain=zzz$',var_popppp,',reverse=T)',sep = '')))
  parse1 <- 'leaflet(shared_) %>%'
  parse2 <- 'addProviderTiles(providers$Esri.WorldTopoMap) %>%'
  parse3 <- 'setView(lng = mean(stations1199$lon), lat = mean(stations1199$lat), zoom = 12) %>%'
  parse4 <- 'addMarkers(layerId=stations1199$number, group="picto", icon = makeIcon(iconUrl = "./picto_velib.png", iconWidth = 10, iconHeight = 10, iconAnchorX = 5, iconAnchorY = 5), options = markerOptions(opacity=0.2)) %>%'
  parse5 <- 'addScaleBar(options = scaleBarOptions(imperial=F)) %>%'
  parse6 <- paste('addPolygons(data=spdf,label=stations1199$name,layerId=stations1199$number,color="red",opacity=0.3,fillColor=palll1(zzz$',
                  var_popppp,
                  '),fillOpacity=0.4,weight=1, highlightOptions = highlightOptions(color = "black", weight = 4,bringToFront = TRUE)) %>%')
  parse7 <- paste('addLegend(position ="topright",pal=palll1,values=zzz$',
                  var_popppp,
                  ') %>%')
  parse8 <- 'addLayersControl(overlayGroups = c("picto"))'
  eval(parse(text = paste(parse1,parse2,parse3,parse4,parse5,parse6,parse7,parse8)))
  
})

data_of_click <- reactiveValues(clickedMarker=NULL)
observeEvent(input$B_map1_marker_click,{
  data_of_click$clickedMarker <- input$B_map1_marker_click
})

observeEvent(input$B_map1_marker_click,{
  clk <- input$B_map1_marker_click
  proxy <- leafletProxy("B_map1")
  station_centre <- clk$id
  liste_stations_proches <- delaunay[delaunay$number2 == station_centre,1]
  stations_proches <- stations1199[stations1199$number %in% c(liste_stations_proches,station_centre),]
  stations_proches2 <- stations1199[stations1199$number %in% liste_stations_proches,]
  lng1 <- 1.5*min(stations_proches$lon)-0.5*max(stations_proches$lon)
  lat1 <- 1.5*min(stations_proches$lat)-0.5*max(stations_proches$lat) 
  lng2 <- 1.5*max(stations_proches$lon)-0.5*min(stations_proches$lon)
  lat2 <- 1.5*max(stations_proches$lat)-0.5*min(stations_proches$lat)
  proxy %>% clearGroup("delaunay") %>% fitBounds(lng1, lat1, lng2, lat2) %>% 
    addLabelOnlyMarkers(lng=stations1199[stations1199$number == station_centre,]$lon,
                        lat=stations1199[stations1199$number == station_centre,]$lat,
                        label=stations1199[stations1199$number == station_centre,]$name, group="delaunay",
                        labelOptions = labelOptions(noHide = T,
                                                    direction = "bottom",
                                                    offset = c(0, 10),
                                                    style = list("color" = "red",
                                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                 "font-size" = "14px","border-color" = "rgba(0,0,0,0.5)")
                        )
    ) %>%
    addLabelOnlyMarkers(lng=stations_proches2$lon,
                        lat=stations_proches2$lat,
                        label=stations_proches2$name, group="delaunay",
                        labelOptions = labelOptions(noHide = T, 
                                                    direction = "bottom", 
                                                    offset = c(0, 10),
                                                    style = list("color" = "black", 
                                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)", 
                                                                 "font-size" = "12px","border-color" = "rgba(0,0,0,0.5)")
                        )
    )
})

output$B_gr2 <- renderAmCharts({
  if (is.null(data_of_click$clickedMarker$id) == F)
  {
    station_centre <- data_of_click$clickedMarker$id
    liste_stations_proches <- paste('X', delaunay[delaunay$number2 == station_centre,1], sep = '')
    xxx <- stations_colonnes[,c(paste('X',station_centre, sep = ''),liste_stations_proches,"time")]
    pal5 <- brewer.pal((length(liste_stations_proches)+1),"Set1")
    amTimeSeries(xxx, 'time', names(xxx)[1:(length(liste_stations_proches)+1)], linetype =0, export = T, 
                 main = paste('Station ', station_centre, ' et voisines', sep = ''),linewidth = c(3, 1),color=pal5, precision=2)
    
  }
})