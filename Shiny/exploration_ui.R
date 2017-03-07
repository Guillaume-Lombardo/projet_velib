
fluidRow(
  column(2,selectInput("B_var1","Choix de variable 1 :",liste_variables_exp[,2]),
         checkboxInput("B_var1_norm","Centrer-réduire Var1?"),
         checkboxInput("B_bivarie", "Bivarié?", value = FALSE),
         conditionalPanel(
            condition = "input.B_bivarie == true",
            selectInput("B_var2", "Choix de variable 2 :", c(2:10)),
            checkboxInput("B_var2_norm","Centrer-réduire Var2?")),
         actionButton("B_go","Mettre à jour")
         ),
  column(6,leafletOutput("B_map",height=800))
)
