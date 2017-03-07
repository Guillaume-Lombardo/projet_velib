
# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
           numericInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
  )),
  # deuxieme colonne avec les sortiesw
  column(width = 8, 
         
         textOutput("mod1")
         
  )
  )

