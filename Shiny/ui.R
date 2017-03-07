shinyUI(
  fluidPage(
    
    navbarPage(
      title = "Classification et modélisation - Velib",
      tabPanel(title = "Classification",
               "onglet A",
               source("classification_ui.R", local = TRUE)$value
      ),
      tabPanel(title = "Exploration variables explicatives",
               "onglet B",
               source("exploration_ui.R", local = TRUE)$value
      ),
      tabPanel(title = "Modélisation",
               "onglet C",
               source("modelisation_ui.R", local = TRUE)$value
      )
    )
  )
)
