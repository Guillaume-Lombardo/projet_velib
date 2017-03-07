shinyServer(function(input, output) {
  source("classification_server.R", local = TRUE, encoding = "UTF-8")
  source("exploration_server.R", local = TRUE, encoding = "UTF-8")
  source("modelisation_server.R", local = TRUE, encoding = "UTF-8")
})