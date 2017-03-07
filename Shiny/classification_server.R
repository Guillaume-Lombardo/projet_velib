tracer_profil_membres <- function(representation, station, nb_classes=2, classe=1, nb_membre=5){
  # couleur <- brewer.pal(10, 'Paired')
  print('tracer_profil_membres')
  print(nb_classes)
  print(classe)
  print(nb_membre)
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
  
  amTimeSeries(data_series, 'time', names(data_series)[which(names(data_series)!='time')], linetype = 0, export = T)
}

output$Amod1 <- renderText({
  paste("carte ; {profil de classe ; 1 classe + quelques series} ; variances")
  # print(head(profil_colonnes_6))
})

output$Aprofil_classe <- renderAmCharts({
  input$AmiseAjour
  isolate({
    eval(parse(text = paste("graphe <- amTimeSeries(profil_colonnes_",input$Anb_cluster,", 'time', ",
                            "names(profil_colonnes_",input$Anb_cluster,")[1:",input$Anb_cluster,"], linetype =0, export = T)",sep = '')))
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