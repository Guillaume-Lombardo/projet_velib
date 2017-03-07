library(shiny)

shinyUI(fluidPage(
  
  navbarPage(
    title = "Classification et modélisation - Velib",
    tabPanel(title = "Classification",
             "Classification"),
    tabPanel(title = "Exploration variables explicatives",
             "Exploration variables explicatives"),
    tabPanel(title = "Modélisation",
             "Modélisation",
             
             source("modelisation_ui.R", local = TRUE)$value
    )
  )
)
)
