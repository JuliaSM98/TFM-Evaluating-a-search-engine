library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(sodium)
library(lubridate)
library(stringr)

fieldsMandatory <- c("txt") 
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }"

task_description <- read.csv('CSV_inputs/tasks_descrip.csv')
users <- read.csv('CSV_inputs/users.csv')

user_name <- c()
x = c(1:length(users$USER_NAME))
for (val in x) {
  user_name<- append(user_name, toString((users$USER_NAME)[val]))
}

pass<-c()
for (val in x) {
  pass<- append(pass, toString((users$Password)[val]))
}


task_number <- c()
y = c(1:length(task_description$TASK))
for (val in y) {
  task_number<- append(task_number, toString((task_description$TASK)[val]))
}
task_des <- c()
for (val in y) {
  task_des<- append(task_des, toString((task_description$DESCRIPTION)[val]))
}

min <- c()
sec <- c()
for (val in y) {
  t <- (str_split((task_description$TIME_MIN_SEC)[val],":"))
  min <- append(min, as.integer(sapply(t,"[[",1)))
  sec <- append(sec, as.integer(sapply(t,"[[",2)))
}


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
                     # br(),
                     # br(),
                     # tags$code("For search engine 1 the Password is: mypass1"),
                     # br(),
                     # tags$code("For search engine 2 the Password is: mypass2")
                   ))
)

credentials = data.frame(
  username_id = user_name,
  passod   = sapply(pass,password_store),
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Welcome", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), shinyjs::inlineCSS(appCSS), uiOutput("body"))
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
  
  observe({
    if (!is.null(input$submit)) {
      if(input$submit>0){
        updateTextInput(session, "txt", value = "")
      }
    }
  })
  
  ################## Make all csv be saved in one #############################
  #############################################################################
  
  ## block if answer is in blank!
  # observe({
  #   mandatoryFilled <-
  #     vapply(fieldsMandatory,
  #            function(x) {
  #              !is.null(input[[x]]) && input[[x]] != ""
  #            },
  #            logical(1))
  #   mandatoryFilled <- all(mandatoryFilled)
  #   shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  # }) 
  
  
  fieldsAll <- c("userName","txt")
  responsesDir <- file.path("response/")
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- c(data, task= task_number[input$submit])
    data <- t(data)
    data
  })
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        input$userName,
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  #############################################################################
  if(isolate(USER$login) == TRUE){
    k <- isolate(input$submit)
  }
  else {
    k<-0
  }
  i <- match(task_number[k +1],task_description$TASK)
  minutes <- reactiveVal(min[i])
  seconds <- reactiveVal(sec[i])
  active <- reactiveVal(FALSE)
  observe({
    invalidateLater(1, session)
    isolate({
      if (USER$login == TRUE & (minutes()>=0 & seconds()>=0)){
        active(TRUE)
        if(active() & !(minutes()==0 & seconds()==0))
        {
          if (seconds() == 0 ){
            seconds(60)
            minutes(minutes() - 1)
          }
          seconds(seconds()-1)
          if(minutes() ==0 & seconds() ==0){
            active(FALSE)
            j <- match(task_number[input$submit +1],task_description$TASK)
            showModal(modalDialog(
              title = "Important message",
              task_description$COUNTDOWN_MESSAGE[j]
            ))
            shinyjs::toggleState(id = "txt", condition = FALSE)
          }
        }
        }
    })
  })
  
  observeEvent(input$submit, {
    i <- match(task_number[input$submit +1],task_description$TASK)
    if(!is.na(i)){
      shinyjs::toggleState(id = "txt", condition = TRUE)
      minutes(min[i])
      seconds(sec[i])
    }
    else{
      shinyjs::toggleState(id = "txt", condition = FALSE)
      minutes(0)
      seconds(0)
      shinyjs::toggleState(id = "submit", condition = FALSE)
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      i<-match(input$userName,users$USER_NAME)
      tabItems(
        if (users$Engine[i] == 1){
        # First tab
        tabItem(tabName ="engine", class = "active",
                fluidRow(
                  h1("Evaluating a Search Engine"),
                  box(width = 12, 
                      textOutput("task"),
                      tags$head(tags$style("#task{color: black;
                                 font-size: 26px;
                                 font-style: bold;
                                 }"
                      )),
                      textOutput("descrip"),
                      tags$head(tags$style("#descrip{color: black;
                                 font-size: 20px;``
                                 }"
                      )),
                      uiOutput("tab1"),
                      textOutput('timeleft'),
                      tags$head(tags$style("#timeleft{color: red;
                                 font-size: 20px;``
                                 }"
                      )),
                      textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "250px"),
                      actionButton("submit", "Submit", class = "btn-primary"),
                )))
          }
        else{
          tabItem(tabName = "engine", class ="active",
                  fluidRow(
                    h1("Evaluating a Search Engine"),
                    box(width = 12, 
                        textOutput("task"),
                        tags$head(tags$style("#task{color: black;
                                 font-size: 26px;
                                 font-style: bold;
                                 }"
                        )),
                        textOutput("descrip"),
                        tags$head(tags$style("#descrip{color: black;
                                 font-size: 20px;``
                                 }"
                        )),
                        uiOutput("tab2"),
                        textOutput('timeleft'),
                        tags$head(tags$style("#timeleft{color: red;
                                 font-size: 20px;``
                                 }"
                        )),
                        textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "250px"),
                        actionButton("submit", "Submit", class = "btn-primary"),
                    )))
        }
      )
      
    }
    else {
      loginpage
    }
  })
  
  output$task <- renderText({
    if (input$submit+1 > length(task_number)){
      "You have finished!"
    }
    else{
      task_number[input$submit +1]
    }
  })
  
  output$descrip <- renderText({
    if (input$submit+1 > length(task_number)){
      "Congratulations!"
    }
    else{
      task_des[input$submit +1]
    }
   
  })
  
  output$tab1 <- renderUI({
    tagList("URL link to the search engine:", url1)
  })
  output$tab2 <- renderUI({
    tagList("URL link to the search engine:", url2)
  })
  observeEvent(input$submit, {
    if (input$submit > length(task_number)){
    }
    else{
      saveData(formData())
    }
  })
  output$timeleft <- renderText({
    paste("Time left:", minutes(), "M", seconds(), "S")
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)