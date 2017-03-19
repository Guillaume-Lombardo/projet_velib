
# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
 
           #Proposition de faire une ACP pour construire les clusters
           checkboxInput(inputId = "DACPcluster", label = "ACP pour construire les clusters ?", value=F),
           
           #Proposition de faire une ACP sur les variables explicatives
           checkboxInput(inputId = "DACPvarexpli", label = "ACP sur les variables explicatives ?", value=F)

           
           # #action boutton pour lancer le calcul
           # actionButton(inputId="Dgo", "Go !"),
           # hr(),
           

         )),
  # deuxieme colonne avec les sortiesw
  column(width = 8, 
  
         tableOutput("Dtablecompmodele")
               

  )
)

