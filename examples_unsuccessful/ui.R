library(shiny)
library(shinyjs)



shinyApp(
  
fieldsMandatory <- c("name"),
ui = fluidPage(
  titlePanel("Evaluating a search engine"),
  div(
    id = "form",
    
    textInput("name", "Name", ""),
    actionButton("submit", "Submit", class = "btn-primary")
  )
),

server <- function(input, output, session) {
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  
  
}
)








