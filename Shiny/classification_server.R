tracer_profil_membres <- function(representation, station, nb_classes=2, classe=1, nb_membre=5){
  # couleur <- brewer.pal(10, 'Paired')
  liste_serie_temp <- sample(which(representation[[nb_classes]]$cluster==classe),nb_membre)
  liste_stations_classe <- paste('X',
                                 representation[[nb_classes]]$number[liste_serie_temp],
                                 sep = '')
  liste_profil_classe <- paste('P',classe,sep = '')
  
  eval(parse(text = paste("data_series <- merge(x = profil_colonnes_",nb_classes,
                          "[,c('time',liste_profil_classe)],",
                          "y = station[,c('time',liste_stations_classe)],",
                          "by.x = 'time',by.y = 'time')",
                          sep = '')))
  
  amTimeSeries(data_series, 'time', names(data_series)[which(names(data_series)!='time')], 
               linetype = 0, export = T,
               main = paste('description de la classe ',classe, ' par ',nb_membre,' groupes'))
}

output$Amod1 <- renderText({
  paste("carte ; {profil de classe ; 1 classe + quelques series} ; variances")
  # print(head(profil_colonnes_6))
})

output$Aprofil_classe <- renderAmCharts({
  input$AmiseAjour
  isolate({
    eval(parse(text = paste("graphe <- amTimeSeries(profil_colonnes_",input$Anb_cluster,", 'time', ",
                            "names(profil_colonnes_",input$Anb_cluster,")[1:",input$Anb_cluster,"],",
                            " linetype =0, export = T,",
                            "main = paste('profil à ',",input$Anb_cluster,",' classes'))",sep = '')))
  })
})

output$Aui_select_classe_detail <- renderUI({
  selectInput(inputId = 'Anumero_classe_detail', label = 'Numero de la classe à détailler', 
              selected = 1,
              choices = 1:input$Anb_cluster)
  print('uirender')
  print(as.numeric(input$Anb_cluster))
})

output$Adetail_classe <- renderAmCharts({
  input$AmiseAjour
  isolate({
    tracer_profil_membres(representation = representation_kmeans,
                          station = stations_colonnes,
                          nb_classes = as.numeric(input$Anb_cluster),
                          classe = min(as.numeric(input$Aclasse_detail),as.numeric(input$Anb_cluster)),
                          nb_membre = as.numeric(input$Anb_courbe))
  })
})

output$A_map2 <- renderLeaflet({
  data_carte <- representation_kmeans[[as.numeric(input$Anb_cluster)]][,c('number','cluster')]
  data_carte$couleur_poly <- data_carte$cluster #brewer.pal(10, 'Paired')[data_carte$cluster]
  voronoi_custom <- voronoi500[which(sapply(1:length(voronoi500), 
  																									 function(.x) voronoi500@polygons[[.x]]@ID) %in% data_carte$number)]
  afficher_carte(data=data_carte,
                 polygones=voronoi_custom,
                 stations=read.csv(file="../Sortie/stations1199.csv"),
                 var_polygone='couleur_poly',
                 lbl_var_polygone='cluster')

})


# library(rAmCharts)
# test <- stations_colonnes[,c('X903', 'X905','time')]
# amTimeSeries(test, 'time', c('X903', 'X905'))  
# 
# data('data_stock_2')
# amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'))

# output$Cafficheimportance <- renderUI({
#   input$Cgo
#   isolate({
#     if (input$Cselecmod == "Lasso") {
#       amChartsOutput("CImpvarPlot")
#     }
#     #fin isolate
#   })
# })