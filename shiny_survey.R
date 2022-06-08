library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(lubridate)
library(stringr)
library(base)
library(glue)
library(RSQLite)
library(DBI)
library(tidyverse)
library(markdown)
library(xlsx)
library(shinysurveys)
library(data.table)
library(bcrypt)

data <- read_csv("CSV_inputs/questions.csv", col_types = cols(x = col_character()), col_names = T)

if(dim(data)[1]>0){
    ui <- fluidPage(
      surveyOutput(df=data,
      )
    )
    
    server <- function(input, output, session) {
      renderSurvey()
      
      submitted <- FALSE
      saveRDS(submitted, file = "submitted.rds")
      
      observeEvent(input$submit, {
        df <- getSurveyData()
        save(df, file = "df.RData")
        submitted <- TRUE
        saveRDS(submitted, file = "submitted.rds")
        showModal(modalDialog(
          title = "Questionnaire submitted",
          "Click Start button if you have completed all the questions!",
          easyClose = FALSE,
          footer = NULL
          
        ))
        
      })
    }
    shinyApp(ui, server)
}

  


