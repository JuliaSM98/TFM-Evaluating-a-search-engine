library(shinysurveys)
library(shiny)
library(tibble)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(sodium)
library(lubridate)
library(stringr)
library(base)
library(glue)
library(RSQLite)
library(DBI)
library(tidyverse)
library(markdown)
library(tibble)


appCSS <- ".mandatory_star { color: red; }
           } "

header <- dashboardHeader( title = "Welcome", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), shinyjs::inlineCSS(appCSS), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

df <- data.frame(
                question = "What is your favorite food?",
                 option = "Your Answer",
                 input_type = 'id',"text",
                 input_id = "favorite_food",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

slider_question <- data.frame(
  question = "On a scale from 1-10, how much do you love sushi?",
  option = NA,
  input_type = "slider",
  input_id = "sushi_scale",
  dependence = NA,
  dependence_value = NA,
  required = TRUE
)
extendInputType(input_type = "slider", {
  shiny::sliderInput(
    inputId = surveyID(),
    label = surveyLabel(),
    min = 1,
    max = 10,
    value = 5
  ) 
})

server <- function(input, output) {
  
  output$body <- renderUI({
    # dashboardPage(
    # dashboardHeader(title = "Welcome"),
    # dashboardSidebar(),
    # dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 12,
          surveyOutput(df=readRDS('questions.rds'))
          #surveyOutput(read.csv("questions.csv"))
          #source("CSV_inputs/questionnaire.R", local=TRUE)$value
      ),
      box(width = 12,
          # https://shiny.rstudio.com/articles/html-ui.html
          # puedo hacer lo mismo en html por el tema de que lo puedo cambiar en el usuario admin
          #source("CSV_inputs/questionnaire.R", local=TRUE)$value,
          div(actionButton("start", "Start", class = "btn-primary"), style="float:left"),
          div(actionButton("back", "Back", class = "btn-primary"), style="float:right")
      ))
  })
  #)


  #renderSurvey()


  observeEvent(input$submit, {
    print(input$age)
    #getSurveyData()
  })
}

shinyApp(ui, server)




