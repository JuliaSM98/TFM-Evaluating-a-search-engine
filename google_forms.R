# library(shiny)
# library(shinyforms)
# 
# # Define the first form: basic information
# basicInfoForm <- list(
#   id = "basicinfo",
#   questions = list(
#     list(id = "name", type = "text", title = "Name", mandatory = TRUE,
#          hint = "Your name exactly as it is shown on your passport"),
#     list(id = "age", type = "numeric", title = "Age", mandatory = FALSE),
#     list(id = "favourite_pkg", type = "text", title = "Favourite R package"),
#     list(id = "terms", type = "checkbox", title = "I agree to the terms")
#   ),
#   storage = list(
#     type = STORAGE_TYPES$FLATFILE,
#     path = "responses"
#   ),
#   name = "Personal info",
#   password = "shinyforms",
#   reset = TRUE,
#   multiple = FALSE,
#   validations = list(
#     list(condition = "nchar(input$name) >= 3",
#          message = "Name must be at least 3 characters"),
#     list(condition = "input$terms == TRUE",
#          message = "You must agree to the terms")
#   )
# )
# 
# # Define the second form: soccer
# soccerFormInfo <- list(
#   id = "soccerform",
#   questions = list(
#     list(id = "team", type = "text", title = "Favourite soccer team"),
#     list(id = "player", type = "text", title = "Favourite player")
#   ),
#   storage = list(
#     type = STORAGE_TYPES$FLATFILE,
#     path = "responses"
#   ),
#   multiple = FALSE
# )
# 
# ui <- fluidPage(
#   h1("shinyforms example"),
#   tabsetPanel(
#     tabPanel(
#       "Basic info",
#       formUI(basicInfoForm)
#     ),
#     tabPanel(
#       "Soccer",
#       formUI(soccerFormInfo)
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   formServer(basicInfoForm)
#   formServer(soccerFormInfo)
# }
# 
# shinyApp(ui = ui, server = server)


library(shiny)
library(shinysurveys)

df <- data.frame(question = "What is your favorite food?",
                 option = "Your Answer",
                 input_type = "text",
                 input_id = "favorite_food",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

ui <- fluidPage(
  surveyOutput(df = df,
               survey_title = "Hello, World!",
               survey_description = "Welcome! This is a demo survey showing off the {shinysurveys} package.")
)

server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Congrats, you completed your first shinysurvey!",
      "You can customize what actions happen when a user finishes a survey using input$submit."
    ))
  })
}

shinyApp(ui, server)