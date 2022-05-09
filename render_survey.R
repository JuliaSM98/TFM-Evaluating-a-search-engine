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
library(googlesheets4)
library(googledrive)
library(xlsx)
library(shinysurveys)


appCSS <- ".mandatory_star { color: red; }
           } "

header <- dashboardHeader( title = "Welcome", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), shinyjs::inlineCSS(appCSS), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

questions.url <- "https://docs.google.com/spreadsheets/d/1hYgo1f90zLOEQthe_k_6qpj4p7TnikZ14LCj2Cf-OcE/edit#gid=0"
questions <- read_sheet(questions.url, sheet=1)



server <- function(input, output) {
  
  output$body <- renderUI({
    # dashboardPage(
    # dashboardHeader(title = "Welcome"),
    # dashboardSidebar(),
    # dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 12,
          #surveyOutput(df=readRDS('questions.rds'))
          surveyOutput(df=questions)
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
  

  # data <- c(data, Age = input$age)
  # data <- c(data, Gender = input$gender)
  # data <- c(data, Nationality = input$nationality)
  # data <- c(data, Course = input$course)
  # data <- c(data, Experience = input$experience)
  

  observeEvent(input$submit, {
    df <- getSurveyData()
    # print(df)
    a<- df[,"question_id"]
    b<- df[,"response"]
    # print(a[[1]])
    # print(b[[1]])
    #data <- c(data, a=b)
    #print(data)
    
    for (i in 1:5){
      idd <- (df[i,2])
      content <- toString(df[i,4])
      print(idd)
      print(content)
      data <- c(data, df$idd = content)
    }
    print(data)
  })
}

shinyApp(ui, server)




