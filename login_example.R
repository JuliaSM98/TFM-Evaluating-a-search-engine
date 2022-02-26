library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(lubridate)
# library(countdown)

task_descprition <- read.csv('CSV_tests/tasks_descrip.csv')
users <- read.csv('CSV_tests/users.csv')

uu <- c()
x = c(1:length(users$USER_NAME))
for (val in x) {
  uu<- append(uu, toString((users$USER_NAME)[val]))
}
pp<-c()
xx = c(1:length(users$Password))
for (val in xx) {
  pp<- append(pp, toString((users$Password)[val]))
}

task_number <- c()
y = c(1:length(task_descprition$TASK))
for (val in y) {
  task_number<- append(task_number, toString((task_descprition$TASK)[val]))
}
task_des <- c()
y = c(1:length(task_descprition$DESCRIPTION))
for (val in y) {
  task_des<- append(task_des, toString((task_descprition$DESCRIPTION)[val]))
}


i=1


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("For search engine 1 the Password is: mypass1"),
                     br(),
                     tags$code("For search engine 2 the Password is: mypass2")
                   ))
)

credentials = data.frame(
  username_id = uu,
  passod   = sapply(pp,password_store),
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Welcome", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  url1 <- a("El País", href="https://elpais.com/")
  url2 <- a("El Mundo", href="https://www.elmundo.es/")
  
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a("Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE){ 
      sidebarMenu(
        menuItem("Task", tabName = "engine", icon = icon("th"))
      )
    }
  })
  
  
  
#### MODIFICAR A PARTIR D'AQUÍ PER VEURE COM POSAR QUE LES TASQUES CANVIIN   
  
  output$body <- renderUI({
    timer <- reactiveVal(10)
    if (USER$login == TRUE ) {
      
      tabItems(
        if (input$passwd == "mypass1"){
        # First tab
        tabItem(tabName ="engine", class = "active",
                fluidRow(
                  h1(task_number[i]),
                  h2(task_des[i]),
                  box(width = 12, 
                      uiOutput("tab1"),
                      textAreaInput("txt", "Enter the answer below:"),
                      actionButton("submit", "Submit", class = "btn-primary"),
                      verbatimTextOutput("placeholder", placeholder = TRUE
                      )
                )))
          }
        else{
          tabItem(tabName = "engine", class ="active",
                  fluidRow(
                    h1(task_number[i]),
                    h2(task_des[i]),
                    box(width = 12, 
                        uiOutput("tab2"),
                        textAreaInput("txt", "Enter the answer below:"),
                        actionButton("submit", "Submit", class = "btn-primary"),
                        verbatimTextOutput("placeholder", placeholder = TRUE
                        )
                    )))
        }
      )
      
    }
    else {
      loginpage
    }
  })
  
  output$tab1 <- renderUI({
    tagList("URL link to the search engine:", url1)
  })
  output$tab2 <- renderUI({
    tagList("URL link to the search engine:", url2)
  })
  
  output$placeholder <- renderText({ 
    #autoInvalidate()
    input$submit
    input$txt
    })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)