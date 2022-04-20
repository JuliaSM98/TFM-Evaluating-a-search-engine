library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("reactivePoll and reactiveFileReader"),
  fluidRow(
    column(12,
           p("This app has a log file which is appended to",
             "every second.")
    )
  ),
  fluidRow(
    column(12, wellPanel(
      "This side uses a reactiveFileReader, which is monitoring",
      "the log file for changes every 0.5 seconds.",
      verbatimTextOutput("fileReaderText")
    ))
  )
))

server <- shinyServer(function(input, output, session) {
  
  
  reader <- reactiveFileReader(intervalMillis = 1000, session, filePath = 
                                 "CSV_inputs/users.csv", readFunc = read.csv)
 
  output$fileReaderText <- renderText({
    
  })
  
})
  


runApp(list(ui = ui, server = server), launch.browser = TRUE)
