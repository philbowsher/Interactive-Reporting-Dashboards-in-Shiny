palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

ui <- htmlTemplate(
  
  filename = "index.html",
  plot1 = plotOutput('plot1'),
  xcol = selectInput('xcol', 'X Variable', names(iris)),
  ycol = selectInput('ycol', 'Y Variable', names(iris), selected=names(iris)[[2]]),
  clus = numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)

  )

server <- shinyServer(function(input, output, session) {
  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
})


shinyApp(ui = ui, server = server)
