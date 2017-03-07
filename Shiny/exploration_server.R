# B_donnees <- reactive({as.data.frame(read.csv("Sortie/Jeuvarexplifinal.csv",sep=',')[,input$B_var1])})


output$B_map <- renderLeaflet({
  afficher_carte(data=stations_population_voronoi500_densite,
                             stations= stations, polygones=voronoi500, var_polygone="P13_POP",
                             lbl_var_polygone="Densité</br>(hab/km²)")
})