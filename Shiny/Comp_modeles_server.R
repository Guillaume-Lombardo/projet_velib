


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
  datatable(table, options = list(pageLength = 12))%>%
    formatStyle(
      'clusters 2',
      color = styleInterval(c(max(table[,1])-0.0001), c('black', 'red'))
    )%>%
  formatStyle(
    'clusters 3',
    color = styleInterval(c(max(table[,2])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 4',
    color = styleInterval(c(max(table[,3])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 5',
    color = styleInterval(c(max(table[,4])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 6',
    color = styleInterval(c(max(table[,5])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 7',
    color = styleInterval(c(max(table[,6])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 8',
    color = styleInterval(c(max(table[,7])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 9',
    color = styleInterval(c(max(table[,8])-0.0001), c('black', 'red'))
  )%>%
  formatStyle(
    'clusters 10',
    color = styleInterval(c(max(table[,9])-0.0001), c('black', 'red'))
  )
})