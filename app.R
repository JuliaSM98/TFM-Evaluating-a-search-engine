# revise if i need all the libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(sodium)
library(lubridate)
library(stringr)
library(base)
library(glue)
#library(shinyauthr)
library(RSQLite)
library(DBI)
library(tidyverse)

appCSS <- ".mandatory_star { color: red; }
           } "

header <- dashboardHeader( title = "Welcome", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), shinyjs::inlineCSS(appCSS), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")


# Server function 
server <- function(input, output, session) {
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
  credentials = data.frame(
    username_id = user_name,
    passod   = sapply(pass,password_store),
    stringsAsFactors = F
  )
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
  splash = FALSE
  PAGE <- reactiveValues(splash = splash)
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE & PAGE$splash == FALSE){ 
      sidebarMenu(
        menuItem("Description", tabName = "splash", icon = icon("th"))
        # if (input$userName == "admin"){
        #   menuItem("Config", tabName = "config", icon = icon("th"))
        # }
      )
    }
    else if (USER$login == TRUE & PAGE$splash == TRUE){
      sidebarMenu(
        menuItem("Task", tabName = "engine", icon = icon("th")),
        if (input$userName == "admin"){
          menuItem("Config", tabName = "config", icon = icon("th"))
        }
      )
    }
  })
  
  # If someone writes an answer and clicks submit, clear txt
  observe({
    if (!is.null(input$submit)) {
      if(input$submit>0){
        updateTextInput(session, "txt", value = "")
      }
    }
  })

  ####### Define the time for each task and also the countdown message ########
  if(isolate(USER$login) == TRUE & isolate(PAGE$splash)==TRUE){
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
      if (USER$login == TRUE & (minutes()>=0 & seconds()>=0) & PAGE$splash == TRUE){
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
      data <- c(data, Datestamp = Date())
      data <- c(data, Timestamp = Time())
      data <- c(data, TimeToComplete = paste(m,s,sep=":"))
      data <- c(data, SecondsToComplete = m*60+s)
      data <- c(data, Pages = count())
      data <- c(data, Clicks = clicks())
      data <- c(data, Task = task_number[input$submit])
      i<-match(input$userName,users$USER_NAME)
      data <- c(data, Engine = users$Engine[i])
      data <- c(data, Age = input$age)
      data <- c(data, Gender = input$gender)
      data <- c(data, Nationality = input$nationality)
      data <- c(data, Course = input$course)
      data <- c(data, Experience = input$experience)
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
  
# go back and forth questionnaire
  questions = reactiveVal(FALSE)
  observeEvent(input$questions, {
    questions(TRUE)
  })
  observeEvent(input$back, {
    questions(FALSE)
  })
  
  # Load Data
  RV <- reactiveValues(data = users, tasks = task_description )
  user_data <- reactive({ RV$data})
  t_descrip <- reactive({RV$tasks})
  
  #A test action button
  observeEvent(input$new_row, {
    print(input$tabs)
    if (input$userName == "admin") {
      if (input$tabs =="Users"){
        name <- input$uname
        pass <- input$passs
        engine <- as.numeric(input$eng)
        RV$data <- RV$data %>% add_row(USER_NAME = name, Password = pass, Engine = engine)
        userDir <- "CSV_inputs/"
        write.table(x = RV$data, file = file.path(userDir, "users.csv"), append = FALSE,
                    row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")}
      else if (input$tabs == "Task Consfiguration"){
        
      }
      else if (input$tabs == "Questionaire"){
        
      }
      
    }
  }) 
  
  observeEvent(input$delete_row, {
    if (input$userName == "admin"){
      if (!is.null(input$usertable_rows_selected)) {
        RV$data <- RV$data[-as.numeric(input$usertable_rows_selected),]
        row.names(RV$data) <- NULL
        userDir <- "CSV_inputs/"
        write.table(x = RV$data, file = file.path(userDir, "users.csv"), append = FALSE,
                    row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
      }}
    
  }) 
  
  
  output$usertable <- renderDataTable(user_data(), options = list(scrollX = TRUE))
  output$tasktable <- renderDataTable(t_descrip(), options = list(scrollX = TRUE))
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      i<-match(input$userName,users$USER_NAME)
      num_engine <- paste("tab",users$Engine[i],sep="")
      if (PAGE$splash == FALSE ){
        tabItem(tabName ="splash", class = "active",
                fluidRow(
                  if (questions() == FALSE){
                    box(width = 12,
                        includeMarkdown("markdown.Rmd"),
                        actionButton("questions", "Go to questionnaire", class = "btn-primary"),)}
                  else{
                    box(width = 12,
                          # https://shiny.rstudio.com/articles/html-ui.html
                          # puedo hacer lo mismo en html por el tema de que lo puedo cambiar en el usuario admin
                          source("CSV_inputs/questionnaire.R", local=TRUE)$value,
                          div(actionButton("start", "Start", class = "btn-primary"), style="float:left"),
                          div(actionButton("back", "Back", class = "btn-primary"), style="float:right")
                      )}
                  ))}
      else{
        if (input$userName == "admin"){
          tabItems(
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
                          htmlOutput(num_engine),
                          textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "100px"),
                          actionButton("submit", "Submit", class = "btn-primary"),
                      ))),
            tabItem(tabName ="config",
                    tabsetPanel(type = "tabs", id = "tabs", footer = list(actionButton("new_row", "Add new row"),
                                                                          actionButton("delete_row","Delete a row")),
                                tabPanel("Users", 
                                         box(
                                           width = NULL,
                                           status = "primary",
                                           DT::dataTableOutput("usertable"),
                                           textInput("uname", "User Name"),
                                           textInput('passs','Password'),
                                           selectInput("eng", ("Engine"),
                                                       choices = list("1" = 1, "2" = 2), selected = 2),
                                           # actionButton("new_row", "Add new row"),
                                           # actionButton("delete_row","Delete a row"),
                                         )
                                         ),
                                tabPanel("Task Consfiguration", 
                                         box(
                                           width = NULL,
                                           status = "primary",
                                           DT::dataTableOutput("tasktable"),
                                           textInput("TASK", "Task number"),
                                           textInput("DESCRIPTION", 'Task description'),
                                           textInput('TIME_MIN_SEC','Time in minutes and seconds'),
                                           textInput('COUNTDOWN_MESSAGE','Countdown Message'),
                                           # actionButton("new_row", "Add new row"),
                                           # actionButton("delete_row","Delete a row"),
                                         )
                                         
                                         
                                         ),
                                tabPanel("Questionaire",
                                         box(
                                           width = NULL,
                                           status = "primary",
                                           # actionButton("new_row", "Add new row"),
                                           # actionButton("delete_row","Delete a row"),
                                         )
                                         )
                    )
                    )
          )
          
        }
        
        else {
        tabItems(
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
                        htmlOutput(num_engine),
                        textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "100px"),
                        actionButton("submit", "Submit", class = "btn-primary"),
                  )))
         )
          }
        }
      
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
  
  observeEvent(input$start,{
    PAGE$splash <- TRUE 
  })
  
  
  
  getPage1<-function() {
    return(includeHTML("www/iframe1.html"))
  }
  output$tab1<-renderUI({getPage1()})
  count <- reactive({ input$count }) 
  
  observeEvent(input$submit,{
    output$tab1<-renderUI({getPage1()})
    count <- reactive({ input$count }) 
    clicks <- reactive({ input$click }) 
  })
  
  
  getPage2<-function() {
    return(includeHTML("www/iframe2.html"))
  }
  output$tab2<-renderUI({getPage2()})
  count <- reactive({ input$count })
  clicks <- reactive({ input$click }) 
  
  observeEvent(input$submit,{
    output$tab2<-renderUI({getPage2()})
    count <- reactive({ input$count }) 
    clicks <- reactive({ input$click }) 
  })
  
  output$timeleft <- renderText({
    paste("Time left:", minutes(), "M", seconds(), "S")
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)