shinyServer(function(input, output) {
  source("classification_server.R", local = TRUE)
  source("exploration_server.R", local = TRUE)
  source("modelisation_server.R", local = TRUE)
})