


#table de comparaison des modèles
output$Dtablecompmodele <- renderDataTable({
  if (input$DACPvarexpli == F){
    if (input$DACPcluster == F){
      url <- paste0("../Confusion/ClusternoACPvenoACP/")
    }else
    {
      url <- paste0("../Confusion/ClusterACPvenoACP/")
    }
  } else
  {
    if (input$CACPcluster == F){
      url <- paste0("../Confusion/ClusternoACPveACP/")
    }else
    {
      url <- paste0("../Confusion/ClusterACPveACP/")
    }    
  }
  url<-paste0(url,"Biensclasses.RDS")
  table <- readRDS(file = url)
  datatable(table, options = list(pageLength = 12))
})