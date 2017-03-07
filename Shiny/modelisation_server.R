output$mod1 <- renderText({
  
  
  paste("Un bien beau modèle ", input$Cselecmod, " sur ", input$Ckmeans, " classes")
})


# output$distPlot <- renderAmCharts({
#   input$go
#   isolate({
#     # generate bins based on input$bins from ui.R
#     #x    <- faithful[, 2]
#     choixcol <- names(faithful)==(input$colonne)
#     x<-faithful[,choixcol]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     
#     # draw the histogram with the specified number of bins
#     #amHist(x, breaks = bins, col = input$colorselect, border = 'white', main=input$titre, xlab=input$colonne, export=T)
#     amHist(x, control_hist = list(breaks = bins), col = input$colorselect , border = "white", main=input$titre, xlab=input$colonne)
#     #amHist(x)
#     
#     #fin isolate
#   })   
# })