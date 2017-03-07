# B_donnees <- reactive({as.data.frame(read.csv("Sortie/Jeuvarexplifinal.csv",sep=',')[,input$B_var1])})

output$B_var1_uio <- renderUI({
  selectInput("B_var1","Choix de variable 1 :",read.csv("../Sortie/liste_variables_exp.csv",sep=';')[,2])
})

output$B_var2_uio <- renderUI({
  selectInput("B_var2","Choix de variable 2 :",read.csv("../Sortie/liste_variables_exp.csv",sep=';')[,2])
})



output$B_map <- renderLeaflet({
  afficher_carte(data=read.csv("../Sortie/stations_population_voronoi500_densite.csv"), 
                 polygones=readRDS("voronoi500.rds"),
                 stations=read.csv(file="../Sortie/stations_sirene_voronoi500.csv")[,2:6],
                 var_polygone="P13_POP",
                 lbl_var_polygone="Densité</br>(hab/km²)")
})

