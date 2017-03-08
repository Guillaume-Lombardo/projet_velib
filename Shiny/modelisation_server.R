
Cmodele <- reactive({
  input$Cgo
  isolate({
  scale<-1
  if(!input$Cscale){
    scale<-0
  }
  nommodele <- paste0("../Modeles/",input$Cselecmod, input$Ckmeans ,scale,".RDS")
  if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")&&(input$Ckmeans %in% 5:6)){
  modele<-readRDS(file = nommodele)
  }
  else
  {
  #pour l'instant, on n'a pas tous les modèles générés
  modele<-readRDS(file = "../Modeles/Lasso61.RDS")
  }
  })
})

output$Cmod1 <- renderText({
  input$Cgo
  isolate({
    paste0("Modèle ", input$Cselecmod, " sur ", input$Ckmeans, " classes")
    #  tags$h1("Heading")
  })
})

#le graphique qui va afficher le barplot de l'importance des variables dans le modèle
output$CImpvarPlot <- renderAmCharts({
  input$Cgo
  nvar<-input$Cnbvar
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
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")) {
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
      url<-paste0("../Modeles/Y",input$Ckmeans,".RDS")
      Y <- readRDS(file = url)
      modele<-Cmodele()
      Yprev<-predict(modele, X, type="class",s=modele$lambda.1se)
      
      confusion<-as.data.frame.matrix(table(Y,Yprev))
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
    url<-paste0("../Modeles/Y",input$Ckmeans,".RDS")
    Y <- readRDS(file = url)
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
    url<-paste0("../Modeles/Y",input$Ckmeans,".RDS")
    Y <- readRDS(file = url)
    modele<-Cmodele()
    Yprev<-predict(modele, X, type="class",s=modele$lambda.1se)
    
    confusion<-as.data.frame.matrix(table(Y,Yprev))
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
      plot(modele, main="")
      title("Mesure de deviance de Cross-Validation", line = +3)
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


#affiche la carte des stations mal classées
output$C_map <- renderLeaflet({
  input$Cgo
  isolate({
    url<-paste0("../Sortie/clustering_",input$Ckmeans,"_classes_mod7j.csv")
    Y<-read.csv(url, sep=";") 
    if(input$Cscale){
      X<-readRDS(file = "../Modeles/Xscale.RDS")
    }
    else{
      X<-readRDS(file = "../Modeles/Xnonscale.RDS")
    }  
    modele<-Cmodele()
    Yprev<-predict(modele, X, type="class")
    Yprev<-as.data.frame(Yprev)
    colnames(Yprev)[1]<-"Yprev"
    merge(Y, Yprev)

    voronoi_custom <- voronoi500[which(sapply(1:length(voronoi500), 
                                              function(.x) voronoi500@polygons[[.x]]@ID) %in% Y$number)]
     afficher_carte(data=Y,
                   polygones=voronoi_custom,
                   stations=read.csv(file="../Sortie/stations_sirene_voronoi500.csv")[,2:6],
                   var_polygone="cluster",
                   var_point="cluster",
                   lbl_var_polygone="cluster initial",
                   lbl_var_point="cluster prévu")
  })
})



