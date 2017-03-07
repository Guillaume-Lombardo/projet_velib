
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

output$Cmod1 <- renderText({
  paste0("Modèle ", input$Cselecmod, " sur ", input$Ckmeans, " classes")
  #  tags$h1("Heading")

  })

#le graphique qui va afficher le barplot de l'importance des variables dans le modèle
output$CImpvarPlot <- renderAmCharts({
  input$Cgo
  isolate({


    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")){
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
      nvar<-min(input$Cnbvar, nrow(out22))
      amBarplot(x="X2", y="1", data=out22[1:nvar,], export = T, 
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


#table de confusion en nombre
output$Ctableconfusion <- renderTable({
  input$Cgo
  isolate({
      if(input$Cscale){
        X<-readRDS(file = "../Modeles/Xscale.RDS")
      }
      else{
        X<-readRDS(file = "../Modeles/Xnonscale.RDS")
      }
      modele<-Cmodele()
      Yprev<-predict(modele, X, type="class",s=modele$lambda.1se)
      
      confusion<-as.data.frame.matrix(table(Y,Yprev))
      # sommel<-apply(confusion, 1, sum)
      # confusion2<-round(100*apply(confusion, MARGIN=2, sommel, FUN="/"),digits=0)
      confusion
      #fin isolate
  }) 

},
rownames=T,
colnames=T
)

#table de confusion en nombre en %
output$Ctableconfusionp <- renderTable({
  input$Cgo
  isolate({
    if(input$Cscale){
      X<-readRDS(file = "../Modeles/Xscale.RDS")
    }
    else{
      X<-readRDS(file = "../Modeles/Xnonscale.RDS")
    }
    modele<-Cmodele()
    Yprev<-predict(modele, X, type="class",s=modele$lambda.1se)
    
    confusion<-as.data.frame.matrix(table(Y,Yprev))
    sommel<-apply(confusion, 1, sum)
    confusion<-round(100*apply(confusion, MARGIN=2, sommel, FUN="/"),digits=0)
    confusion
    #fin isolate
  }) 
  
},
rownames=T,
colnames=T, 
digits=0
)

output$Cpourcentagebienclasse <- renderText({
  input$Cgo
  isolate({
    if(input$Cscale){
      X<-readRDS(file = "../Modeles/Xscale.RDS")
    }
    else{
      X<-readRDS(file = "../Modeles/Xnonscale.RDS")
    }
    Y <- readRDS(file = "../Modeles/Y.RDS")
    modele<-Cmodele()
    Yprev<-predict(modele, X, type="class",s=modele$lambda.1se)
    
    confusion<-as.data.frame.matrix(table(Y,Yprev))
    #bienclasse <- 100* sum(diag(confusion))/sum(confusion)
    bienclasse<- round(100*sum(diag(as.matrix(confusion)))/sum(confusion),digits=2)
  }) 
  paste("Le pourcentage de bien classés", bienclasse, "%")
})



#le graphique qui va afficher une mesure de deviance de CV
#en fonction de lambda pour lasso, ridge et elasticnet
output$Cdevlambda <- renderPlot({
  input$Cgo
  isolate({
    
    
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")) {
      #Lasso
      #représentation des coefficients les plus importants
      #modele<-readRDS(file = "../Modeles/Lasso61.RDS")
      modele<-Cmodele()
      plot(modele)
    }
    #fin isolate
  })
})

#affiche une mesure de deviance de CV en fonction de lambda pour lasso, ridge et elasticnet
output$Caffichedev <- renderUI({
  input$Cgo
  isolate({
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")) {
      plotOutput("Cdevlambda")
    }
    #fin isolate
  })
})



