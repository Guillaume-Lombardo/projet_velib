
# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
           selectInput(inputId = "Cselecmod", label = "Choix du modèle", selected = 1,
                       choices = c("Lasso" = 1, "Ridge" = 2, "Elasticnet" = 3,
                                   "Random Forest" = 4, "SVM"=5))
  )),
  # deuxieme colonne avec les sortiesw
  column(width = 8, 
         
         textOutput("mod1")
         
  )
  )

