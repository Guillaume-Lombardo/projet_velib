representation_kmeans <- reactive(readRDS(file = '../Sortie/representation_kmeans.RDS'))
stations_colonnes <- reactive(readRDS(file = '../Sortie/stations_colonnes.RDS'))
eval(parse(text = paste("profil_colonnes_",1:10," <- reactive(readRDS(",
                        "file = '../Sortie/profil_colonnes_",1:10,".RDS'))", sep = '')))

tracer_profil_membres <- function(representation, station, nb_classes=2, classe=1, nb_membre=5){
  # couleur <- brewer.pal(10, 'Paired')
  liste_serie_temp <- sample(which(representation[[nb_classes]]$cluster==classe),nb_membre)
  liste_stations_classe <- paste('X',
                                 representation[[nb_classes]]$number[liste_serie_temp],
                                 sep = '')
  liste_profil_classe <- paste('P',classe,sep = '')
  
  eval(parse(text = paste("data_series <- merge(x = profil_colonnes_",nb_classes,
                          "()[,c('time',liste_profil_classe)],",
                          "y = station[,c('time',liste_stations_classe)],",
                          "by.x = 'time',by.y = 'time')",
                          sep = '')))
  
  amTimeSeries(data_series, 'time', names(data_series)[which(names(data_series)!='time')], linetype = 0, export = T)
}

output$Amod1 <- renderText({
  paste("carte ; {profil de classe ; 1 classe + quelques series} ; variances")
  # print(head(profil_colonnes_6()))
})

output$Aprofil_classe <- renderAmCharts({
  input$AmiseAjour
  isolate({
    eval(parse(text = paste("graphe <- amTimeSeries(profil_colonnes_",input$Anb_cluster,"(), 'time', ",
          "names(profil_colonnes_",input$Anb_cluster,"())[1:",input$Anb_cluster,"], linetype =0, export = T)",sep = '')))
  })
})

output$Adetail_classe <- renderAmCharts({
  input$AmiseAjour
  isolate({
    tracer_profil_membres(representation = representation_kmeans(),
                          station = stations_colonnes(),
                          nb_classes = as.numeric(input$Anb_cluster),
                          classe = as.numeric(input$Aclasse_detail),
                          nb_membre = as.numeric(input$Anb_courbe))
  })
})


# library(rAmCharts)
# test <- stations_colonnes[,c('X903', 'X905','time')]
# amTimeSeries(test, 'time', c('X903', 'X905'))  
# 
# data('data_stock_2')
# amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'))
