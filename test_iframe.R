library(shiny)

members <- data.frame(name=c("Name 1", "Name 2"), nr=c('BCRA1','FITM2'))

ui <- fluidPage(titlePanel("Getting Iframe"), 
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(6, selectInput("Member", label=h5("Choose a option"),choices=c('BCRA1','FITM2'))
                      ))),
                  mainPanel(fluidRow(
                    htmlOutput("frame")
                  )
                  )
                ))

server <- function(input, output) {
  # observe({ 
  #   query <- members[which(members$nr==input$Member),2]
  #   test <<- paste0("http://news.scibite.com/scibites/news.html?q=GENE$",query)
  # })
  output$frame <- renderUI({
    # input$Member
    my_test <- tags$iframe(height=600, width=535, url_link="http://news.scibite.com/scibites/news.html?q=GENE" )
    my_test
  })
}

shinyApp(ui, server)