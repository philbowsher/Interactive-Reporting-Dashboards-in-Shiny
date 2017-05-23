require(shiny)
require(forecast)

ui <- htmlTemplate(
  "index.html",
  periods = sliderInput("periods", "Forecasting periods", 1, 20, 10, 1),
  plot1 = plotOutput("plot1")
)


server <- shinyServer(function(input, output, session){
  
  output$plot1 <- renderPlot({
    fit <- auto.arima(AirPassengers)
    pred <- forecast(fit, level=c(80, 95, 99), input$periods)
    plot(pred, shadecols = "oldstyle")
  })
  
})

shinyApp(ui = ui, server = server)
