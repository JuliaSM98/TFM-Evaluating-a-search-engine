library(shiny)
library(DT)
library(dplyr)
ui <- fluidPage(
  titlePanel("Delete rows with DT"),
  sidebarLayout(
    sidebarPanel(
      actionButton("deleteRows", "Delete Rows")
    ),
    mainPanel(
      dataTableOutput("tableus")
    )
  )
)

df <- data.frame(x = 1:10, y = letters[1:10])

server <- function(input, output) {
  values <- reactiveValues(dfWorking = df)
  
  observeEvent(input$deleteRows,{
    
    if (!is.null(input$tableus_rows_selected)) {
      
      values$dfWorking <- values$dfWorking[-as.numeric(input$tableus_rows_selected),]
    }
  })
  
  output$tableus <- renderDataTable({
    values$dfWorking
  })
  
}

shinyApp(ui, server)