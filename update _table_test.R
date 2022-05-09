library(shiny)
library(shinyforms)
library(googlesheets4)
library(googledrive)

# Define the first form: basic information
basicInfoForm <- list(
  id = "basicinfo",
  questions = list(
    list(id = "nationality", type = "text", title = "Nationality", mandatory = TRUE),
    list(id = "course", type = "text", title = "Current Course", mandatory = TRUE),
    list(id = "experience", type = "text", title = "What kind of experience do you have with this kind of search engines?", mandatory = TRUE),
    list(id = "age", type = "numeric", title = "Age", mandatory = TRUE),
    list(id ="gender", type = "text", title = "Gender", mandatory = TRUE)
  ),
  storage = list(
    type = STORAGE_TYPES$FLATFILE,
    path = "responses"
  ),
  reset = TRUE,
  multiple = FALSE,
  validations = list(
  )
)

ui <- fluidPage(
      "Basic info",
      formUI(basicInfoForm)
  
)

server <- function(input, output, session) {
  formServer(basicInfoForm)
  
  
}

shinyApp(ui = ui, server = server)

