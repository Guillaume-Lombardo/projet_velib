
Cmodele <- reactive({
  input$Cgo
  isolate({
  scale<-1
  if(!input$Cscale){
    scale<-0
  }
  nommodele <- paste0(input$Ckmeans, input$Cselecmod,scale,".RDS")
  #modele<-readRDS(file = nommodele)
  #pur l'instant, on a un seul modèle en stock donc à modifier
  modele<-readRDS(file = "../Modeles/Lasso61.RDS")
  })
})

output$mod1 <- renderText({
   paste("Un bien beau modèle ", input$Cselecmod, " sur ", input$Ckmeans, " classes ")
  })

#le graphique qui va afficher le barplot de l'importance des variables dans le modèle
output$CImpvarPlot <- renderAmCharts({
  input$Cgo
  isolate({


    if (input$Cselecmod == "Lasso") {
      #Lasso
      #représentation des coefficients les plus importants
      #modele<-readRDS(file = "../Modeles/Lasso61.RDS")
      modele<-Cmodele()
      out2<-coef(modele)
      out22<-abs(out2[[1]])
      for (i in 2:input$Ckmeans)
      {
        out22<-out22+abs(out2[[i]])

      }
      out22<-as.data.frame(as.matrix(out22))
      out22$X2<-row.names(out22)
      out22<-out22[order(out22[,1], decreasing=T),]
      amBarplot(x="X2", y="1", data=out22[1:10,], export = T, 
                main= "Importance des variables", 
                xlab = "Variables explicatives", 
                ylab="somme des valeaurs absolues des beta")
    }
    else
    {
      x= 1:5
      amHist(x, export = T)
    }
   #fin isolate
    
  })
})

#On affiche le graphique des importances si c'est possible pour le modèle retenu
output$Cafficheimportance <- renderUI({
  input$Cgo
  isolate({
    if (input$Cselecmod == "Lasso") {
      amChartsOutput("CImpvarPlot")
    }
    #fin isolate
  })
})











# output$CdistPlot2 <- renderAmCharts({
#   input$Cgo
#   isolate({
# 
#     x= 1:5
# 
#     amHist(x, export = T)
# 
# 
#     #fin isolate
#   })
# })