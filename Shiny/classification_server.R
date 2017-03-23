tracer_profil_membres <- function(representation, station, nb_classes=2, classe=1, nb_membre=5){
  liste_serie_temp <- sample(which(representation[[nb_classes]]$cluster==classe),nb_membre)
  liste_stations_classe <- paste('X',
                                 representation[[nb_classes]]$number[liste_serie_temp],
                                 sep = '')
  liste_profil_classe <- paste0('P',classe)
  liste_profil_classe_25 <- paste0('P',classe,'_p25')
  liste_profil_classe_75 <- paste0('P',classe,'_p75')
  
  if (input$AclusterACP){
    eval(parse(text = paste0("data_series <- merge(x = profil_2colonnes_",nb_classes,
                             "[,c('time',liste_profil_classe)],",
                             "y = p25_2colonnes_",nb_classes,
                             "[,c('time',liste_profil_classe_25)],",
                             "by.x = 'time',by.y = 'time')")))
    eval(parse(text = paste0("data_series <- merge(x = data_series,",
                             "y = p75_2colonnes_",nb_classes,
                             "[,c('time',liste_profil_classe_75)],",
                             "by.x = 'time',by.y = 'time')")))
  } else {
    eval(parse(text = paste0("data_series <- merge(x = profil_colonnes_",nb_classes,
                             "[,c('time',liste_profil_classe)],",
                             "y = p25_colonnes_",nb_classes,
                             "[,c('time',liste_profil_classe_25)],",
                             "by.x = 'time',by.y = 'time')")))
    eval(parse(text = paste0("data_series <- merge(x = data_series,",
                             "y = p75_colonnes_",nb_classes,
                             "[,c('time',liste_profil_classe_75)],",
                             "by.x = 'time',by.y = 'time')")))
  }
  
  eval(parse(text = paste0("data_series <- merge(x = data_series,",
                           "y = station[,c('time',liste_stations_classe)],",
                           "by.x = 'time',by.y = 'time')")))
  
  # lw <- ifelse((substr(names(data_series)[names(data_series)!='time'],1,1)=='P') & 
  #                substr(names(data_series)[names(data_series)!='time'],1,3) not %in% c('P25','P75')),3,1)
  
  
  liste_P <-list(c(liste_profil_classe_25,liste_profil_classe,liste_profil_classe_75))
  liste_nom_series <- c(liste_P, as.list(names(data_series)[which(names(data_series)!='time' & substr(names(data_series),1,1)!='P')]))
  
  lw <- sapply(liste_nom_series,length)
  liste_couleur <- c(sapply(1:nb_classes,leaflet::colorNumeric('viridis',1:nb_classes,reverse=T, na.color = '#FFFFFF'))[classe],
  									 brewer.pal(9,'Set1')[1:(length(lw)-1)])
  
  amTimeSeries(data_series, 'time', liste_nom_series, 
               linetype = 0, export = T,
               main = paste('description de la classe ',classe, ' par ',nb_membre,' groupes'),
               linewidth = lw, color = liste_couleur)
}

# output$Amod1 <- renderText({
#   paste("carte ; {profil de classe ; 1 classe + quelques series} ; variances")
#   # print(head(profil_colonnes_6))
# })

output$Aprofil_classe <- renderAmCharts({
  input$AmiseAjour
  isolate({
    if (input$AclusterACP){
      eval(parse(text = paste0("graphe <- amTimeSeries(profil_2colonnes_",input$Anb_cluster,", 'time', ",
                              "names(profil_2colonnes_",input$Anb_cluster,")[1:",input$Anb_cluster,"],",
                              " linetype =0, export = T,",
                              "main = paste('profil à ',",input$Anb_cluster,",' classes'))")))
    } else {
      eval(parse(text = paste0("graphe <- amTimeSeries(profil_colonnes_",input$Anb_cluster,", 'time', ",
                              "names(profil_colonnes_",input$Anb_cluster,")[1:",input$Anb_cluster,"],",
                              " linetype =0, export = T,",
                              "main = paste('profil à ',",input$Anb_cluster,",' classes'),",
      												"color = sapply(1:",input$Anb_cluster,
      												",leaflet::colorNumeric('viridis',1:",input$Anb_cluster,",reverse=T, na.color = '#FFFFFF')))")))
    }
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
    if (input$AclusterACP){
      tracer_profil_membres(representation = representation_kmeans2,
                            station = stations_colonnes,
                            nb_classes = as.numeric(input$Anb_cluster),
                            classe = min(as.numeric(input$Aclasse_detail),as.numeric(input$Anb_cluster)),
                            nb_membre = as.numeric(input$Anb_courbe))
    } else {
      tracer_profil_membres(representation = representation_kmeans,
                            station = stations_colonnes,
                            nb_classes = as.numeric(input$Anb_cluster),
                            classe = min(as.numeric(input$Aclasse_detail),as.numeric(input$Anb_cluster)),
                            nb_membre = as.numeric(input$Anb_courbe))
    }
  })
})

output$A_map2 <- renderLeaflet({
  input$AmiseAjour
  isolate({
    if (input$AclusterACP){
      data_carte <- representation_kmeans2[[as.numeric(input$Anb_cluster)]][,c('number','cluster')]
    } else {
      data_carte <- representation_kmeans[[as.numeric(input$Anb_cluster)]][,c('number','cluster')]
    }
    
    data_carte$couleur_poly <- data_carte$cluster #brewer.pal(10, 'Paired')[data_carte$cluster]
    voronoi_custom <- voronoi500[which(sapply(1:length(voronoi500), 
                                              function(.x) voronoi500@polygons[[.x]]@ID) %in% data_carte$number)]
    afficher_carte(data=data_carte,
                   polygones=voronoi_custom,
                   stations=read.csv(file="../Sortie/stations1199.csv"),
                   var_polygone='couleur_poly',
                   lbl_var_polygone='cluster') 
  })
})

output$Atable_taille <- renderTable({
  input$AmiseAjour
  isolate({
    # browser()
    if (input$AclusterACP){
      res <- data.frame(cluster = 1:input$Anb_cluster, Taille = cluster2[[as.numeric(input$Anb_cluster)]]$size)
    } else {
      res <- data.frame(cluster = 1:input$Anb_cluster, Taille = cluster[[as.numeric(input$Anb_cluster)]]$size)
    }
    res
  }) 
  
},
rownames=F,
colnames=T
)

output$Agraphe_variance <- renderPlot({
    if (input$AclusterACP){
      serie <- sapply(1:10, function(.x) cluster2[[.x]]$betweenss / 
                        cluster2[[.x]]$totss)
      print(plot(1:10,serie, type='l', 
                 xlab = 'nombre de classe', ylim=c(0,1),
                 main = 'variance intra'))
    } else {
      serie <- sapply(1:10, function(.x) cluster[[.x]]$betweenss / 
                        cluster[[.x]]$totss)
      print(plot(1:10,serie, type='l', 
                 xlab = 'nombre de classe', ylim=c(0,1),
                 main = 'variance intra'))
    }
})