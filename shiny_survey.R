library(shiny)
library(base)
library(tidyverse)
library(shinysurveys)

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
          "Click the Start button. You have completed all the questions!",
          easyClose = FALSE,
          footer = NULL
          
        ))
        
      })
    }
    shinyApp(ui, server)
}
