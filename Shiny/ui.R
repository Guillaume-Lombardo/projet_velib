shinyUI(
  fluidPage(
    
    navbarPage(
      title = "Classification et modélisation - Velib",
      tabPanel(title = "Classification",
               source("classification_ui.R", local = TRUE, encoding = "UTF-8")$value
      ),
      tabPanel(title = "Exploration variables explicatives",
               source("exploration_ui.R", local = TRUE, encoding = "UTF-8")$value
      ),
      tabPanel(title = "Modélisation",
               source("modelisation_ui.R", local = TRUE, encoding = "UTF-8")$value
      ),
      tabPanel(title = "Comparaison des modèles",
               fluidRow(
                 tableOutput("tablecompmodele")
               )
      )
    )
  )
)
