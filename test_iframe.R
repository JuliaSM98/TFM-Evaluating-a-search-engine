library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 12,
          HTML('<input type="text" id="count" name="count" style="display: none;"> '), 
          HTML('
          <style>
            iframe{
                width: 100%;
                height: 600px;
                border: 2px solid #ccc;
            }
          </style>
          </head>
          <body>
              <script> 
                var countStr = "count";
                var changeStr = "change";
                var countInput = document.getElementById(countStr);
                countInput.value = -1;
              </script>
              <iframe src="https://elpais.com/" onLoad="var event = new Event(changeStr);countInput.value++;countInput.dispatchEvent(event)"></iframe>

          </body>'),
          textOutput("view"),
          actionButton("submit", "Submit", class = "btn-primary"),
          
          )
    )
  )
)

server <- function(input, output) {
  
    count <- reactive({ input$count }) 
    output$view <- renderText( paste0( "User ID is: ",count()) ) 

}

runApp(list(ui = ui, server = server), launch.browser = TRUE)

