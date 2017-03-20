

output$B_var1_uio <- renderUI({
  selectInput("B_var1","Choix de variable 1 :",liste_variables_exp[,2], selected=liste_variables_exp[1,2])
})

output$B_var2_uio <- renderUI({
  selectInput("B_var2","Choix de variable 2 :",liste_variables_exp[,2], selected=liste_variables_exp[1,2])
})



output$B_map1 <- renderLeaflet({
  B_var1_char <- as.character(input$B_var1)
  input$B_go
  isolate({
    var_popppp <- liste_variables_exp[liste_variables_exp$lbl==B_var1_char,1]
    if (length(var_popppp) == 0) {var_popppp <- "P13_POP"}
    afficher_carte(data=read.csv("../Sortie/stations_population_voronoi500_densite.csv"), 
                 polygones=voronoi500,
                 stations=read.csv(file="../Sortie/stations1199.csv"),
                 var_polygone=var_popppp,
                 lbl_var_polygone=B_var1_char)
  })
})

output$B_map2 <- renderLeaflet({
  B_var2_char <- as.character(input$B_var2)
  input$B_go
  isolate({
    var_popppp2 <- liste_variables_exp[liste_variables_exp$lbl==B_var2_char,1]
    if (length(var_popppp2) == 0) {var_popppp2 <- "P13_POP"}
    afficher_carte(data=read.csv("../Sortie/stations_population_voronoi500_densite.csv"), 
                   polygones=voronoi500,
                   stations=read.csv(file="../Sortie/stations1199.csv"),
                   var_polygone=var_popppp2,
                   lbl_var_polygone=B_var2_char)
  })
})

# output$B_plot <- renderAmCharts({
#   input$B_go
#   isolate({})
# })

