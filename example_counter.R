library(lubridate)
library(shiny)
library(countdown)
library(shinydashboard)



ui <- fluidPage(
  #hr(),
  # actionButton('start','Start'),
  # actionButton('stop','Stop'),
  actionButton('reset','Reset'),
  #numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1),
  textOutput('timeleft')

)

server <- function(input, output, session) {

  # Initialize the timer, 10 seconds, active
  timer <- reactiveVal(5)
  active <- reactiveVal(TRUE)

  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })

  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown completed!"
          ))
        }
      }
    })
  })

  # observers for actionbuttons
  # observeEvent(input$start, {active(TRUE)})
  # observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(5)})

}

# shinyApp(ui, server)
# 
# task_descprition <- read.csv('CSV_inputs/tasks_descrip.csv')
# users <- read.csv('CSV_inputs/users.csv')
# 
# task_number <- c()
# y = c(1:length(task_descprition$TASK))
# for (val in y) {
#   task_number<- append(task_number, toString((task_descprition$TASK)[val]))
# }
# task_des <- c()
# y = c(1:length(task_descprition$DESCRIPTION))
# for (val in y) {
#   task_des<- append(task_des, toString((task_descprition$DESCRIPTION)[val]))
# }
# 
# data <- matrix(0, nrow = 1, ncol = 1)
# 
# 
# 
# 
# 
# #### MAKE TASK TITLE AND DESCRIPTION CHANGE ACCORDING TO WHCH ONE TURN IT IS !!!
# 
#   ui <- basicPage(
#     
#     h1("Evaluating a search engine"),
#     textOutput('task'),
#     textOutput('task_description'),
#     textInput("txt", "Enter the answer below:"),
#     #verbatimTextOutput("default"),
#     verbatimTextOutput("placeholder", placeholder = TRUE),
#     sliderInput("n", "Number of observations", 2, 1000, 500),
#     plotOutput("plot")
#     
#     # imageOutput('count')
#   )
#   
#   server <- function(input, output) {
#     
#     autoInvalidate <- reactiveTimer(4000) 
#     # observe({
#     # 
#     #   
#     # })
#     
#     output$plot <- renderPlot({
#       autoInvalidate()
#       hist(rnorm(input$n)) # rnorm canvia cada vegada 
#     })
#     output$task <-renderText({
#       autoInvalidate()
#       data <- isolate(sweep(x = data, MARGIN = 1,
#                     STATS = 1, FUN = "+"))
#       #paste(task_number[i+1])
#       # tasks[i]
#       # ':'
#     })
#     output$task_description <-renderText({
#       autoInvalidate()
#       print(autoInvalidate())
#       paste("Description")
#    
#       # tasks[i]
#       # ':'
#     })
#     #output$default <- renderText({ input$txt })
#     output$placeholder <- renderText({ 
#       autoInvalidate()
#       input$txt })
#     # output$count <- renderImage({
#     #   autoInvalidate()
#     #   countdown(minutes = 10, seconds = 0)
#     # })
#   }
#   
  shinyApp(ui, server)

