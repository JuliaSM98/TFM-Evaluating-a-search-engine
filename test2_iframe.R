# ui.R
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  #HTML('<input type="text" id="i" name="i" style="display: none;"> '), 
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 12,
          HTML('
          <script src="https://code.jquery.com/jquery-1.12.4.min.js"></script>
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
              document.getElementById("mydiv").onclick = function() {
              var number = Math.random();
              Shiny.onInputChange("mydata", number);
              };   
                      </script>
              <iframe src="https://elpais.com/" onLoad="Shiny.onInputChange("mydata", ++i)"></iframe>
              
          </body>'),
          
          # a shiny element to display unformatted text
          verbatimTextOutput("results"),
          
          

          
          
          # javascript code to send data to shiny server
  #         tags$script('
  #         var number = 0;
  #         window.onload = function() {
  #         var iframe = document.createElement("iframe");
  #         iframe.style.display = "none";
  #         iframe.src = "https://elpais.com/";
  #         document.body.appendChild(iframe);
  #         Shiny.onInputChange("mydata", ++number);
  #         };
  #       
  # ')
      )
    )
  )
)


# server.R

server <- function(input, output) {
  
  output$results = renderPrint({
    input$mydata
  })
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)