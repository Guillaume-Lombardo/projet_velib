
Cmodele <- reactive({
  input$Cgo
  isolate({
    if (input$CACPvarexpli == F){
      if (input$CACPcluster == F){
        url <- paste0("../Modeles/ClusternoACPvenoACP/")
      }else
      {
        url <- paste0("../Modeles/ClusterACPvenoACP/")
      }
    } else
    {
      if (input$CACPcluster == F){
        url <- paste0("../Modeles/ClusternoACPveACP/")
      }else
      {
        url <- paste0("../Modeles/ClusterACPveACP/")
      }    
    }
    scale<-1
    if(!input$Cscale){
      scale<-0
    }
    nommodele <- paste0(url,input$Cselecmod, input$Ckmeans ,scale,".RDS")
    # if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet", "RandomForest")){
    modele<-readRDS(file = nommodele)
    # }
    # else
    # {
    #   #pour l'instant, on n'a pas tous les modèles générés
    #   modele<-readRDS(file = "../Modeles/RandomForest61.RDS")
    # }
  })
})

Yprev <- reactive({
  input$Cgo
  isolate({
    if (input$CACPvarexpli == F){
      if (input$CACPcluster == F){
        url <- paste0("../Modeles/ClusternoACPvenoACP/")
      }else
      {
        url <- paste0("../Modeles/ClusterACPvenoACP/")
      }
    } else
    {
      if (input$CACPcluster == F){
        url <- paste0("../Modeles/ClusternoACPveACP/")
      }else
      {
        url <- paste0("../Modeles/ClusterACPveACP/")
      }    
    }
    if(input$Cscale){
      X<-readRDS(file = paste0(url,"Xscale.RDS"))
    }
    else{
      X<-readRDS(file = paste0(url,"Xnonscale.RDS"))
    }
    modele<-Cmodele()
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")){
      Yprev<-predict(modele, X, type="class", s="lambda.1se")
    }
    else if(input$Cselecmod == "RandomForest"){
      Yprev<-modele$predicted
    }
    else if(input$Cselecmod %in% c("SVMRadial", "SVMLinear")){
      Yprev<-modele$fitted
    }
    else{
      Yprev<-predict(modele, X)
    }
  })
})

confusion <- reactive({
  input$Cgo
  isolate({ 
    scale<-1
    if(!input$Cscale){
      scale<-0
    }
    if (input$CACPvarexpli == F){
      if (input$CACPcluster == F){
        url <- paste0("../Confusion/ClusternoACPvenoACP/confusion", input$Cselecmod, input$Ckmeans, scale, ".RDS")
      }else
      {
        url <- paste0("../Confusion/ClusterACPvenoACP/confusion", input$Cselecmod, input$Ckmeans, scale, ".RDS")
      }
    } else
    {
      if (input$CACPcluster == F){
        url <- paste0("../Confusion/ClusternoACPveACP/confusion", input$Cselecmod, input$Ckmeans, scale, ".RDS")
      }else
      {
        url <- paste0("../Confusion/ClusterACPveACP/confusion", input$Cselecmod, input$Ckmeans, scale, ".RDS")
      }    
    }
    confusion <- readRDS(file = url)
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
      out2<-coef(modele, s="lambda.1se")
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
                ylab="somme des valeurs absolues des beta")
    }
    else if(input$Cselecmod == "RandomForest"){
      
      out<-as.data.frame(Cmodele()$importance[order(Cmodele()$importance[,input$Ckmeans+1],decreasing = T),input$Ckmeans+1])
      out<-cbind.data.frame(row.names(out), out) 
      colnames(out)[]<-c("names","importance")
      amBarplot(x="names", y="importance", data=out[1:nvar,], export = T, 
                main= "Importance des variables", 
                xlab = "Variables explicatives", 
                ylab="Importance")     
      
    }
    else {
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
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet", "RandomForest")) {
      amChartsOutput("CImpvarPlot")
    }
    #fin isolate
  })
})


#table de confusion en nombre
output$Ctableconfusion <- renderTable({
  input$Cgo
  isolate({
    # url<-paste0("../Modeles/ClusternoACPvenoACP/Y",input$Ckmeans,".RDS")
    # Y <- readRDS(file = url)
    # confusion<-as.data.frame.matrix(table(Y,Yprev()))
    confusion()
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
    # url<-paste0("../Modeles/ClusternoACPvenoACP/Y",input$Ckmeans,".RDS")
    # Y <- readRDS(file = url)
    # confusion<-as.data.frame.matrix(table(Y,Yprev()))
    confusion<-confusion()
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
    # url<-paste0("../Modeles/ClusternoACPvenoACP/Y",input$Ckmeans,".RDS")
    # Y <- readRDS(file = url)
    # confusion<-as.data.frame.matrix(table(Y,Yprev()))
    confusion<-confusion()
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
    #récupération des clusters initiaux
    url<-paste0("../Sortie/clustering_",input$Ckmeans,"_classes_mod7j.csv")
    Y<-read.csv(url, sep=";") 
    #récupération du modèle
    modele<-Cmodele()
    #calcul du Y prévisionnel
    Yprev<-Yprev()
    
    #mise en forme de la matrice pour l'affichage de la carte
    Yprev<-as.data.frame(as.numeric(Yprev))
    colnames(Yprev)[1]<-"Yprev"
    Y<-cbind.data.frame(Y, Yprev)
    #on ne retient que les stations mal prévues
    Y<-Y[Y$cluster!=Y$Yprev,]
    
    
    voronoi_custom <- voronoi500[which(sapply(1:length(voronoi500), 
                                              function(.x) voronoi500@polygons[[.x]]@ID) %in% Y$number)]
    afficher_carte(data=Y,
                   polygones=voronoi_custom,
                   stations=read.csv(file="../Sortie/stations1199.csv"),
                   var_polygone="cluster",
                   var_point="Yprev",
                   lbl_var_polygone="cluster initial",
                   lbl_var_point="cluster prévu")
  })
})


#le graphique qui va afficher le barplot de l'importance des variables dans le modèle
output$CCoeffPlot <- renderAmCharts({
  input$Cgo
  nvar<-input$Cnbvar
  icluster<-input$Ccoeff
  isolate({
    
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet")){
      #Lasso
      #représentation des coefficients les plus importants
      #modele<-readRDS(file = "../Modeles/Lasso61.RDS")
      modele<-Cmodele()
      out2<-coef(modele, s="lambda.1se")
      out22<-abs(out2[[icluster]])
      out22<-as.data.frame(as.matrix(out22))
      out22$X2<-row.names(out22)
      out22<-out22[order(out22[,1], decreasing=T),]
      nvar<-min(input$Cnbvar, nrow(out22))
      amBarplot(x="X2", y="1", data=out22[1:nvar,], export = T, 
                main= "Importance des variables", 
                xlab = "Variables explicatives", 
                ylab="somme des valeaurs absolues des beta")
    }
    else if(input$Cselecmod == "RandomForest"){
      
      out<-as.data.frame(Cmodele()$importance[order(Cmodele()$importance[,icluster],decreasing = T),icluster])
      out<-cbind.data.frame(row.names(out), out) 
      colnames(out)[]<-c("names","importance")
      amBarplot(x="names", y="importance", data=out[1:nvar,], export = T, 
                main= "Importance des variables", 
                xlab = "Variables explicatives", 
                ylab="Importance des variablesr")     
      
    }
    else {
      x= 1:5
      amHist(x, export = T)
    }
    #fin isolate
    
  })
})

output$C_numinputcoeff <- renderUI({
  if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet", "RandomForest")) {
    numericInput(inputId="Ccoeff", label="Choix du cluster", value=1, min = 1, max=input$Ckmeans)
  }
})

#On affiche le graphique des importances si c'est possible pour le modèle retenu
output$Caffichecoeff <- renderUI({
  input$Cgo
  isolate({
    if (input$Cselecmod %in% c("Lasso", "Ridge", "Elasticnet", "RandomForest")) {
      amChartsOutput("CCoeffPlot")
    }
    #fin isolate
  })
})


