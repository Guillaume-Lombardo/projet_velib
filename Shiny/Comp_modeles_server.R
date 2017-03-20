


#table de comparaison des modèles
output$Dtablecompmodele <- renderTable({
  if (input$DACPvarexpli == F){
    if (input$DACPcluster == F){
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
  url<-paste0(url,"Biensclasses.RDS")
  table <- readRDS(file = url)
  table
})