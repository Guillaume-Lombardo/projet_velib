


#table de comparaison des modèles
output$Dtablecompmodele <- renderTable({
  url<-paste0("../Confusion/Biensclasses.RDS")
  table <- readRDS(file = url)
  table
})