
#aaa <- read.csv("Sortie/liste_variables_exp.csv",sep=';')

fluidRow(
  column(2,
         uiOutput("B_var1_uio"),
         checkboxInput("B_var1_norm","Centrer-réduire Var1?"),
         checkboxInput("B_bivarie", "Bivarié?", value = FALSE),
         conditionalPanel(
            condition = "input.B_bivarie == true",
            uiOutput("B_var2_uio"),
            checkboxInput("B_var2_norm","Centrer-réduire Var2?")),
         actionButton("B_go","Mettre à jour")
         ),
  column(6,leafletOutput("B_map",height=800))
)
