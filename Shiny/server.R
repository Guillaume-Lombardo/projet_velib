shinyServer(function(input, output,session) {
  source("classification_server.R", local = TRUE, encoding = "UTF-8")
  source("exploration_server.R", local = TRUE, encoding = "UTF-8")
  source("modelisation_server.R", local = TRUE, encoding = "UTF-8")
  
  #table de comparaison des modèles
  output$tablecompmodele <- renderTable({
      url<-paste0("../Modeles/Biensclasses.RDS")
      table <- readRDS(file = url)
      table
  })

})