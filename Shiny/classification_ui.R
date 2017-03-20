# ouvrir une ligne horizontale
fluidRow(
  # premiere colonne pour les entrees utilisateurs
  column(width = 2, 
         wellPanel(
           
           
           # Faire une ACP sur les profils temporel avant clustering ou pas !
           checkboxInput(inputId = "AclusterACP", label = "ACP sur les prfils temporels", value=F),
           # selection du nombre de classes
           selectInput(inputId = 'Anb_cluster', label = 'Nombre de classes', selected = 6,
                       choices = 2:10), 
           
           # selection du modèle
           sliderInput(inputId = 'Anb_courbe', 
                       label = 'nombre de variable à afficher par classe', 
                       value = 5,
                       min = 1,
                       max = 10
           ),
           selectInput(inputId = 'Aclasse_detail', label = 'classe pour laquelle on veut des details',
                       selected = 1,
                       choices = 1:10),
           # uiOutput('Aui_select_classe_detail'),
           actionButton('AmiseAjour','activation !')
         )
  ),
  # deuxieme colonne avec les sortiesw
  column(width = 8, 
         # textOutput("Amod1"),
         amChartsOutput('Aprofil_classe'),
         amChartsOutput('Adetail_classe'),
         leafletOutput('A_map2')
  )
)

