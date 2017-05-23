library(shiny)
library(dygraphs)
library(quantmod)
library(shinydashboard)

sidebar <- dashboardSidebar( sidebarMenu( menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), menuItem("Widgets", tabName = "widgets", icon = icon("th")), menuItem("Charting Time-Series - Stocks", tabName = "myTabForScatterPlot", icon = icon("bar-chart-o")), menuItem("Stock Data Explorer", tabName = "myTabForDataTable", icon = icon("fa fa-table")), menuItem("Stock Comparison", tabName = "myTabForGvisMap", icon = icon("fa fa-map-marker")), menuItem("Top Stocks", tabName = "myTabForRecruitRanking", icon = icon("fa fa-list-ol")), menuItem("External Info", tabName = "myTabForExternalInfo", icon = icon("fa fa-external-link"))
                                     )
                        )

body <- dashboardBody(
        tabItems(
          # First tab content
          tabItem(tabName = "dashboard",
  
        fluidRow(
        
        box(textInput("symb", label = h3("Input a Valid Stock Ticker"), value = "GE")),
        
        box(title='Closing price', width = 12, height = NULL, dygraphOutput("plot"))
        )
                  
      )
      ,
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
    )
     )
  )

ui <- dashboardPage(dashboardHeader(title = 'Stock Watch'), sidebar,body)
              
server <- shinyServer(function(input, output) {
  
  dataInput <- reactive({
    
    prices <- getSymbols(input$symb, src = "google", auto.assign = FALSE)
    
  }) 
  
  ### uncomment this section to see a static OHLC chart via quantmod
  ##   output$plot <- renderPlot({
  
  ##       prices <- dataInput()
  
  ## chartSeries(prices)
  
  ##       })
  ## })
  
  
  ### uncomment this to see an interactive plot via dygraphs
  output$plot <- renderDygraph({
    
    prices <- dataInput()
    
    dygraph(Cl(prices)) %>%
      dyRangeSelector()
  })
})

shinyApp(ui = ui, server = server)