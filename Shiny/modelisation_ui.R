
# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
           
           #selection du nombre de classes
           numericInput(inputId="Ckmeans", label="Nombre de classes", value=6, min = 2, max=10),
           
           #selection du modèle
           selectInput(inputId = "Cselecmod", label = "Choix du modèle", selected = "Lasso",
                       choices = c("Lasso", "Ridge", "Elasticnet",
                                   "Random Forest"= "RandomForest", "SVMRadial", "SVMLinear")),
           
           # selectInput(inputId = "Cselecmod", label = "Choix du modèle", selected = 1,
           #             choices = c("Lasso" = 1, "Ridge" = 2, "Elasticnet" = 3,
           #                         "Random Forest" = 4, "SVM"=5)),
           
           # #Proposition de centrer-réduire les variables
           checkboxInput(inputId = "Cscale", label = "Centrer-réduire ?", value=T),
           
           # #action boutton pour lancer le calcul
           actionButton(inputId="Cgo", "Go !"),
           hr(),
           
           #nombre de variables à afficher
           numericInput(inputId="Cnbvar", label="Nombre de variables explicatives à afficher", value=10, min = 1)
           
           
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
         
         #affiche la carte des stations mal classées
         leafletOutput("C_map",height=600),
         hr(),
         
         #affiche une mesure de deviance de CV 
         #en fonction de lambda pour lasso, ridge et elasticnet
         uiOutput( "Caffichedev"),
         hr(),
         
         #affiche les coefficients pour un cluster donné
         # par ordre décroissant pour lasso, ridge et elasticnet
         #nombre de variables à afficher
         uiOutput( "C_numinputcoeff"),
         #numericInput(inputId="Ccoeff", label="Choix du cluster", value=1, min = 1, max=10),
         hr(),
         uiOutput( "Caffichecoeff"),
         hr()
         
  )
)

