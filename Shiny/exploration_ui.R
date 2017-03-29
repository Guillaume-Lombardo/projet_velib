

fluidPage(
  fluidRow(
    column(1,selectInput("B_nb_cluster", "Nombre de clusters :",2:10, width='100%',selectize = FALSE,selected=4)),
    column(11,
           div(tags$h3("Sélectionnez vos 2 variables dans les liste déroulantes ou en cliquant dans la matrice de corrélation"),align="center"),
           br(),
           # fluidRow(
           #   column(4,offset=4,selectInput("B_var1_test", "Choix de variable 1 :",list(
           #     Population = liste_variables_exp[1:72,3],
           #     Entreprises = liste_variables_exp[73:247,3],
           #     Géographique = liste_variables_exp[248:260,3],
           #     Proximité = liste_variables_exp[261:307,3],
           #     Divers = liste_variables_exp[308:309,3]
           #   ),selectize = FALSE,selected=liste_variables_exp[1,3], width='100%')),
           #   column(4,selectInput("B_var2_test", "Choix de variable 2 :",list(
           #     Population = liste_variables_exp[1:72,3],
           #     Entreprises = liste_variables_exp[73:247,3],
           #     Géographique = liste_variables_exp[248:260,3],
           #     Proximité = liste_variables_exp[261:307,3],
           #     Divers = liste_variables_exp[308:309,3]
           #   ),selectize = FALSE,selected=liste_variables_exp[252,3], width='100%'))
           #   
           # ),
           # fluidRow(
           #   column(4,plotlyOutput("B_plot",height=600)),
           #   column(4,amChartsOutput("B_boxplot1")),
           #   column(4,amChartsOutput("B_boxplot2"))
           # ),
             fluidRow(
               column(4,plotlyOutput("B_plot",height=500)),
               column(4,selectInput("B_var1_test", "Choix de variable 1 :",list(
                 Population = liste_variables_exp[1:72,3],
                 Entreprises = liste_variables_exp[73:247,3],
                 Géographique = liste_variables_exp[248:260,3],
                 Proximité = liste_variables_exp[261:307,3],
                 Divers = liste_variables_exp[308:309,3]
               ),selectize = FALSE,selected=liste_variables_exp[1,3], width='100%'),
               amChartsOutput("B_boxplot1",height=450)),
               column(4,selectInput("B_var2_test", "Choix de variable 2 :",list(
                 Population = liste_variables_exp[1:72,3],
                 Entreprises = liste_variables_exp[73:247,3],
                 Géographique = liste_variables_exp[248:260,3],
                 Proximité = liste_variables_exp[261:307,3],
                 Divers = liste_variables_exp[308:309,3]
               ),selectize = FALSE,selected=liste_variables_exp[252,3], width='100%'),
               amChartsOutput("B_boxplot2",height=450))
             ),
           fluidRow(
             column(4,plotlyOutput("B_gr1",height=600)),
             column(4,leafletOutput("B_map1",height=600)),
             column(4,leafletOutput("B_map2",height=600))
           ),
           div(tags$h3("Cliquez sur 1 station de la 1ère carte pour voir la série chronologique de la station et voisines"),align="center"),
           br(),
           amChartsOutput("B_gr2",height=600)
    )
  )
)
