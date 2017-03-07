
# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
           
           #selection du nombre de classes
           selectInput(inputId = "Ckmeans", label = "Nombre de classes", selected = 6,
                       choices = 2:10), 
           
           #selection du modèle
           selectInput(inputId = "Cselecmod", label = "Choix du modèle", selected = "Lasso",
                       choices = c("Lasso", "Ridge", "Elasticnet",
                                   "Random Forest", "SVM")),
           
           # selectInput(inputId = "Cselecmod", label = "Choix du modèle", selected = 1,
           #             choices = c("Lasso" = 1, "Ridge" = 2, "Elasticnet" = 3,
           #                         "Random Forest" = 4, "SVM"=5)),
           
           # #Proposition de centrer-réduire les variables
           checkboxInput(inputId = "Cscale", label = "Centrer-réduire ?"),

           # #action boutton pour lancer le calcul
           actionButton(inputId="Cgo", "Go !")
  )),
  # deuxieme colonne avec les sortiesw
  column(width = 8, 
         
         textOutput("mod1"),
         
          uiOutput( "Cafficheimportance")
         
         # ,
         # amChartsOutput("CdistPlot2")
         
  )
  )

