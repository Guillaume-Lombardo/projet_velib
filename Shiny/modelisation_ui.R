
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
           checkboxInput(inputId = "Cscale", label = "Centrer-réduire ?", value=T),
           
           #nombre de variables à afficher
           numericInput(inputId="Cnbvar", label="Nombre de variables explicatives à afficher", value=10, min = 1),
           
           # #action boutton pour lancer le calcul
           actionButton(inputId="Cgo", "Go !")
           
           
         )),
  # deuxieme colonne avec les sortiesw
  column(width = 8, 
         
         tags$div( tags$h1(textOutput("Cmod1")) , align="center"), 
         hr(),
         
         #affiche les variables importantes quand c'est possible
         uiOutput( "Cafficheimportance"),
         
         br(),
         
         div(tags$h2("Tables de confusion et proportion de biens classés"),
             br(),
             
             fluidRow(
               
               column(width = 5,
                      fluidRow(
                        #tableconfusion en nombre
                        tags$h5("Tables de confusion en nombre"),
                        br(),
                        tableOutput("Ctableconfusion") 
                      )
               ), 
               column(width = 5,
                      fluidRow(
                        #tableconfusion en %
                        tags$h5("Tables de confusion en pourcentage"),
                        br(),
                        tableOutput("Ctableconfusionp")
                      )
               )
             ),
             align="center"),
         br(), 
         br(),
         
         #proportion de biens classés
         div(textOutput("Cpourcentagebienclasse"), align="center"), 
         
         hr(),
         #affiche une mesure de deviance de CV 
         #en fonction de lambda pour lasso, ridge et elasticnet
         uiOutput( "Caffichedev")
         
  )
)

