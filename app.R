# revise if i need all the libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(sodium)
library(lubridate)
library(stringr)
library(base)


# Mandatory to write somethings in the text boxes! This way the red asterisk will appear 
fieldsMandatory <- c("txt") 

# We define Date and Time, too know at which date and at which time the subject pressed submit
Date <- function() format(Sys.time(), "%Y-%m-%d")
Time <-function() format(Sys.time(), "%H:%M:%OS")
#humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# definition of the red star
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
appCSS <- ".mandatory_star { color: red; }
           } "

# read configuration files
task_description <- read.csv('CSV_inputs/tasks_descrip.csv')
users <- read.csv('CSV_inputs/users.csv')

##################### Information to extract from csv's ########################
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
################################################################################

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 50%; max-width: 100%; margin: 0 auto; padding: 20px;",
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


# Server function 
server <- function(input, output, session) {
  
  # To be changed 
  url1 = "https://elpais.com/"
  # url2 = "https://www.elmundo.es/"
  
  # when login is false 
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
  
  # Once the login becomes true 
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE){ 
      sidebarMenu(
        menuItem("Task", tabName = "engine", icon = icon("th"))
      )
    }
  })
  
  # If someone writes anwer and clicks submit, clear txt
  observe({
    if (!is.null(input$submit)) {
      if(input$submit>0){
        updateTextInput(session, "txt", value = "")
      }
    }
  })

  ####### Define the time for each task and also the countdown message ########
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
    invalidateLater(1000, session)
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
  
  
  # observeEvent(input$submit, {
  #   if (input$submit > length(task_number)){
  #   }
  #   else{
  #     saveData(formData())
  #   }
  # })
  
  Timecomplete <- function(j){
    m<-minutes()-min[j]
    s<-seconds()-sec[j]
    if (s<0){
      s<-60+s
      if (m>0) m<-m-1
      else m<-0
    }
    return(paste(m,s,sep=":"))
  }
  
  # When input$submit, we change the min/sec from next task and save data
  
  
  # define the form in which we want to save data
  fieldsAll <- c("userName","txt")
  responsesDir <- file.path("response/")
  # formData <- reactive({
  #   data <- sapply(fieldsAll, function(x) input[[x]])
  #   data <- c(data, datestamp = Date())
  #   data <- c(data, timestamp = Time())
  #   data <- c(data, TimeToComplete = "hola")
  #   data <- c(data, task = task_number[input$submit])
  #   i<-match(input$userName,users$USER_NAME)
  #   data <- c(data, engine = users$Engine[i])
  #   data <- t(data)
  #   data
  # })
  
  observeEvent(input$submit, {
    i <- match(task_number[input$submit +1],task_description$TASK)
    j <- match(task_number[input$submit],task_description$TASK)
    
    m<-min[j]-minutes()
    s<-sec[j]-seconds()
    if (s<0 | m<0){
      if (s<0){
        s<-60+s
        m<-m-1
      }
      if (m<0) m<-0
    }
    
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, datestamp = Date())
      data <- c(data, timestamp = Time())
      data <- c(data, TimeToComplete = paste(m,s,sep=":"))
      data <- c(data, SecondsToComplete = m*60+s)
      data <- c(data, pages = count())
      data <- c(data, task = task_number[input$submit])
      i<-match(input$userName,users$USER_NAME)
      data <- c(data, engine = users$Engine[i])
      data <- t(data)
      data
    })
    
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
    
    if (input$submit > length(task_number)){
    }
    else{
      saveData(formData())
      #saveData(formData(TimeToComplete("cola")))
    }
  })
  #############################################################################
  # Define function to save the data
  saveData <- function(data) {
    fileName1 <- sprintf("app_results.tsv")
    file = file.path(responsesDir, fileName1)
    
    if (!file.exists(paste(responsesDir,fileName1,sep=""))){
      write.table(x = data, file = file.path(responsesDir, fileName1), append = TRUE,
                  row.names = FALSE, col.names = TRUE, qmethod = "double")
    }
    else{
      write.table(x = data, file = file.path(responsesDir, fileName1), append = TRUE,
                  row.names = FALSE, col.names = FALSE, qmethod = "double")
    }
  }
  

  
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
                      textOutput('timeleft'),
                      tags$head(tags$style("#timeleft{color: red;
                                 font-size: 20px;``
                                 }"
                      )),
                      #htmlOutput("tab1"),
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
                      textOutput("view1"),
                      textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "100px"),
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
                        textOutput('timeleft'),
                        tags$head(tags$style("#timeleft{color: red;
                                 font-size: 20px;``
                                 }"
                        )),
                        #htmlOutput("tab2"),
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
                                <iframe src="https://www.elmundo.es/" onLoad="var event = new Event(changeStr);countInput.value++;countInput.dispatchEvent(event)"></iframe>
                  
                            </body>'),
                        textOutput("view2"),
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
      paste(task_number[input$submit +1],"of",length(task_number))
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
  
  # output$tab1 <- renderUI({
  #   if (input$submit>0){
  #     tags$iframe(src=url1, width="100%", height = 300)
  #   }
  #   else{
  #     tags$iframe(src=url1, width="100%", height = 300)
  #   }
  # })
  
  count <- reactive({ input$count }) 
  output$view1 <- renderText( paste0( "Number of clicks: ",count()) )
  output$view2 <- renderText( paste0( "Number of clicks: ",count()) )
  
  # output$tab2 <- renderUI({
  #   if (input$submit>0){
  #     tags$iframe(src=url2, width="100%", height = 300)
  #   }
  #   else{
  #     tags$iframe(src=url2, width="100%", height = 300)
  #   }
  # })
  
  output$timeleft <- renderText({
    paste("Time left:", minutes(), "M", seconds(), "S")
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)