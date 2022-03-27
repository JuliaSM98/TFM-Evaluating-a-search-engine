library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 12,
          htmlOutput("tab1"),
          textOutput("view"),
          actionButton("submit", "Submit", class = "btn-primary"),
          
          )
    )
  )
)

server <- function(input, output) {
  
  getPage<-function() {
    return(includeHTML("www/iframe_test.html"))
  }
  output$tab1<-renderUI({getPage()})
  count <- reactive({ input$count }) 
  clicks <- reactive({ input$click }) 
  output$view <- renderText( paste0( "Number of loads: ",count()," and ", "Number of clicks: ",clicks()))
  
  observeEvent(input$submit,{
    output$tab1<-renderUI({getPage()})
    count <- reactive({ input$count }) 
    clicks <- reactive({ input$click }) 
    output$view <- renderText( paste0( "Number of loads: ",count(), " and ", "Number of clicks: ",clicks()))
  })

}

runApp(list(ui = ui, server = server), launch.browser = TRUE)

