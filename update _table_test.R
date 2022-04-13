library(shiny)
library(DT)

RV <- reactiveValues(data = mtcars)

app <- shinyApp(
  
  
  ui <- fluidPage(
    DT::dataTableOutput("mytable"),
    actionButton("do", "Click Me")
  ),
  
  
  server = function(input, output,session) {
    
    #Load the mtcars table into a dataTable
    output$mytable = DT::renderDataTable({
      RV$data
    })
    
    #A test action button
    observeEvent(input$do, {
      RV$data$cyl <- RV$data$cyl * 10 
    })   
  }
  
)

runApp(app)