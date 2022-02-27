library(shiny)
library(shinyjs)

fieldsMandatory <- c("name") 
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

labelMandatory <- function(label) {#usado
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }" #usado


  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS), #usado
    titlePanel("Evaluating a search engine"),
    
    div(
      id = "form",
      
      textInput("name", labelMandatory("Name"), ""),
      actionButton("submit", "Submit", class = "btn-primary")
    )
  )
  
  server = function(input, output) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    }) 
    
    fieldsAll <- c("name")
    responsesDir <- file.path("response/")
    epochTime <- function() {
      as.integer(Sys.time())
    }
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = humanTime())
      data <- t(data)
      data
    })
    saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
    })
    
  }
  shinyApp(ui = ui, server = server)
