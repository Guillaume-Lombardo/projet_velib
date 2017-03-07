

output$B_var1_uio <- renderUI({
  selectInput("B_var1","Choix de variable 1 :",read.csv("../Sortie/liste_variables_exp.csv",sep=';')[,2], selected=read.csv("../Sortie/liste_variables_exp.csv",sep=';')[1,2])
})

output$B_var2_uio <- renderUI({
  selectInput("B_var2","Choix de variable 2 :",read.csv("../Sortie/liste_variables_exp.csv",sep=';')[,2])
})



output$B_map1 <- renderLeaflet({
  print("début B_map1")
  print(as.character(input$B_var1))
  B_var1_char <- as.character(input$B_var1)
  input$B_go
  isolate({
    var_popppp <- read.csv("../Sortie/liste_variables_exp.csv",sep=';')[read.csv("../Sortie/liste_variables_exp.csv",sep=';')$lbl==B_var1_char,1]
    print(as.character(var_popppp))
  afficher_carte(data=read.csv("../Sortie/stations_population_voronoi500_densite.csv"), 
                 polygones=readRDS("voronoi500.rds"),
                 stations=read.csv(file="../Sortie/stations_sirene_voronoi500.csv")[,2:6],
                 var_polygone=var_popppp,
                 lbl_var_polygone=B_var1_char)
  })
})

output$B_map2 <- renderLeaflet({
  input$B_go
  isolate({
    afficher_carte(data=read.csv("../Sortie/stations_population_voronoi500_densite.csv"), 
                   polygones=readRDS("voronoi500.rds"),
                   stations=read.csv(file="../Sortie/stations_sirene_voronoi500.csv")[,2:6],
                   var_polygone=read.csv("../Sortie/liste_variables_exp.csv",sep=';')[read.csv("../Sortie/liste_variables_exp.csv",sep=';')$lbl==input$B_var2,1],
                   lbl_var_polygone=input$B_var2)
  })
})

