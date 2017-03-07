
Cmodele <- reactive({
  input$Cgo
  isolate({
  scale<-1
  if(!input$Cscale){
    scale<-0
  }
  nommodele <- paste0(input$Ckmeans, input$Cselecmod,scale,".RDS")
  #modele<-readRDS(file = nommodele)
  modele<-readRDS(file = "Lasso61.RDS")
  })  
})

output$mod1 <- renderText({
  
  
  paste("Un bien beau modèle ", input$Cselecmod, " sur ", input$Ckmeans, " classes    ")
  
})

output$CdistPlot1 <- renderAmCharts({
  input$Cgo
  isolate({
    x= 10:20
    amHist(x, export = T)
   #fin isolate
  })
})

output$Cafficheimportance <- renderUI({
  input$Cgo
  isolate({
    if (input$Cselecmod == "Lasso") {
      amChartsOutput("CdistPlot1")
    }
    #fin isolate
  })
})
  



output$CdistPlot2 <- renderAmCharts({
  input$Cgo
  isolate({

    x= 1:5

    amHist(x, export = T)


    #fin isolate
  })
})