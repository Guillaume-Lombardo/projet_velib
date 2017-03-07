# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
           
           #selection du nombre de classes
           selectInput(inputId = 'Anb_cluster', label = 'Nombre de classes', selected = 6,
                       choices = 2:10), 
           
           #selection du modèle
           sliderInput(inputId = 'Anb_courbe', 
                       label = 'nombre de variable à afficher par classe', 
                       selected = 5,
                       choices = 1:30
           ),
           activeButton('AmiseAjour','activation !')
         ),
         # deuxieme colonne avec les sortiesw
         column(width = 8, 
                
                textOutput("mod1")
                
         )
  )
)
