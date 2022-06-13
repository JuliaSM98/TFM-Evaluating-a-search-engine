# Row reording callbacks definition
callback_tasks <- c(
  "table.on('row-reorder', function(e, details, edit){",
  "  var oldRows = [], newRows = [];",
  "  for(let i=0; i < details.length; ++i){",
  "    oldRows.push(details[i].oldData);",
  "    newRows.push(details[i].newData);",
  "  }",
  "  Shiny.setInputValue('rowreorder', {old: oldRows, new: newRows});",
  "});"
)


callback_questions <- c(
  "table.on('row-reorder', function(e, details, edit){",
  "  var oldRows = [], newRows = [];",
  "  for(let i=0; i < details.length; ++i){",
  "    oldRows.push(details[i].oldData);",
  "    newRows.push(details[i].newData);",
  "  }",
  "  Shiny.setInputValue('rowreorder_quest', {old: oldRows, new: newRows});",
  "});"
)

## SERVER FUNCTION
function(input, output, session) {
  # Mandatory field that have to be completed
  fieldsMandatory <- c("txt") 
  # Date and Time functions
  Date <- function() format(Sys.time(), "%Y-%m-%d")
  Time <-function() format(Sys.time(), "%H:%M:%OS")
  # Definition of red star mandatory symbol
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  
  # Reading configuration files
  text_config <- read.csv('CSV_inputs/text.csv', row.names = 1,stringsAsFactors=FALSE)
  text_config_survey <- read.csv('CSV_inputs/text_survey.csv', row.names = 1,stringsAsFactors=FALSE)
  questions_drive <- read_csv("CSV_inputs/questions.csv", col_types = cols(x = col_character()), col_names = T)
  app_responses <- as.data.frame(fread("response/app_results.tsv", fill=TRUE))
  task_description <- read.csv('CSV_inputs/tasksdescrip.csv')
  users <- read.csv('CSV_inputs/users.csv')
  task_descriptionn <- reactiveFileReader(1000, session, "CSV_inputs/tasksdescrip.csv", read.csv)
  
  user_name <- c()
  x = c(1:length(users$USER_NAME))
  for (val in x) {
    user_name<- append(user_name, toString((users$USER_NAME)[val]))
  }
  
  pass<-c()
  for (val in x) {
    pass<- append(pass, toString((users$Password)[val]))
  }
  
  admin_permit <- c()
  for (val in x) {
    admin_permit<- append(admin_permit, toString((users$Admin)[val]))
  }
  
  min <- c(task_description$TIME_MIN)
  sec <- c(task_description$TIME_SEC)
  
  # USERS CREDENTIALS
  credentials = data.frame(
    username_id = user_name,
    passod = sapply(pass,hashpw),
    admin = admin_permit,
    stringsAsFactors = F
  )
  
  # MAIN LOGIN SCREEN
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
  
  
  # CHECK PASSWORD AND USERNAME MATCH
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
            if (checkpw(Password,pasmatch)){
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

  # LOGOUT button
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a("Logout",
              href="javascript:window.location.reload(true)"),
            class = "dropdown",
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  # Remove survey data when logging out
  observe({
    if(USER$login == FALSE & file.exists("df.RData")){
      file.remove("df.RData")
    }
  })
  
  # SIDEBAR MENU WHEN LOGIN = TRUE
  splash = FALSE
  PAGE <- reactiveValues(splash = splash)
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE & PAGE$splash == FALSE){ 
      sidebarMenu( 
        id = "des",
        menuItem("Description", tabName = "splash", icon = icon("th")),
        if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
          sidebarMenu(
            menuItem("Configuration", tabName = "config", icon = icon("th")),
            menuItem("Results", tabName = "results", icon = icon("th")),
            menuItem("HTML", tabName = "init_markdown", icon = icon("th")),
            menuItem("Engine links", tabName = "en_link", icon = icon("th")),
            if (questions()){
              sidebarMenu(
              actionButton("rerender_survey", "Update survey"))
            }
          )
        }
      )
    }
    else if (USER$login == TRUE & PAGE$splash == TRUE){
      selected_in <- function(){
        i<-match(input$userName,users$USER_NAME)
        return(users$Engine[i])
      }
      sidebarMenu(
        menuItem("Task", tabName = "engine", icon = icon("th")),
        if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
          sidebarMenu(
            menuItem("Results", tabName = "results", icon = icon("th")),
            sidebarMenu(
              selectInput("type_engine","Choose which engine you want to show:", choices = c("Engine 1" = 1, "Engine 2" = 2),selected = selected_in())
              )
          )
        }
      )
    }
  })
  
  # Clear text when submitting answer
  observeEvent(input$submit_answer,{
    if(input$submit_answer>0){
      updateTextInput(session, "txt", value = "")
    }
  })
  
  # Definition of the time for each task and also the countdown message
  minutes <- reactiveVal(min[1])
  seconds <- reactiveVal(sec[1])
  active <- reactiveVal(FALSE)
  observe({
    invalidateLater(1000, session)
    if(dim(task_descriptionn())[1] ==0){
      minutes(0)
      seconds(0)
      toggleState(id = "txt", condition = FALSE)
      shinyjs::toggleState(id = "submit_answer", condition = FALSE)
    }
    else{
    isolate({
      if (USER$login == TRUE & (minutes()>=0 & seconds()>=0 ) & PAGE$splash == TRUE){
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
            
            task_count <- c()
            z = c(1:length(task_descriptionn()$TASK))
            for(val in z){
              task_count <- append(task_count, toString((task_descriptionn()$COUNTDOWN_MESSAGE)[val]))
            }
            showModal(modalDialog(
              title = "Press submit to continue:",
              task_count[input$submit_answer +1]
            ))
            shinyjs::toggleState(id = "txt", condition = FALSE)
          }
        }
      }
    })}
  })
  
  # WHEN input$submit_answer: CHANGE MIN&SEC AND SAVE DATA 
  fieldsAll <- c("userName","txt")
  observeEvent(input$submit_answer, {
    task_description <- read.csv('CSV_inputs/tasksdescrip.csv')
    task_number <- c()
    y = c(1:length(task_description$TASK))
    for (val in y) {
      task_number<- append(task_number, toString((task_description$TASK)[val]))
    }
    min <- c(task_description$TIME_MIN)
    sec <- c(task_description$TIME_SEC)
    i <- match(task_number[input$submit_answer +1],task_description$TASK)
    j <- input$submit_answer
    m<-min[j]-minutes()
    s<-sec[j]-seconds()
    if (s<0 | m<0){
      if (s<0){
        s<-60+s
        m<-m-1
      }
      if (m<0) m<-0
    }
    
    load("df.RData")
    row <- dim(df)[1]
    # Format in which we save the data
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, Datestamp = Date())
      data <- c(data, Timestamp = Time())
      data <- c(data, TimeToCompleteMin = m)
      data <- c(data, TimeToCompleteSec = s)
      data <- c(data, TotalSecondsToComplete = m*60+s)
      data <- c(data, Pages = count())
      data <- c(data, Load_Times = toString(array_of_times()))
      data <- c(data, Load_Seconds = toString(array_of_sec()))
      data <- c(data, Task = task_number[input$submit_answer])
      i<-match(input$userName,users$USER_NAME)
      engine_fun <-function(){
        if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
          return(as.numeric(input$type_engine))
        }
        else{
          return(users$Engine[i])
        }
      } 
      data <- c(data, Engine = engine_fun())
      for (i in 1:row){
        content <- toString(df[i,4])
        question_id <- toString(df[i,2])
        data <- c(data, question_id = question_id)
        data <- c(data, question_answer = content)
        
      }
      data <- t(data)
      data
    })
    
    
    if(!is.na(i)){
      shinyjs::toggleState(id = "txt", condition = TRUE)
      minutes(min[i])
      seconds(sec[i])
    }
    else { 
      shinyjs::toggleState(id = "txt", condition = FALSE)
      minutes(0)
      seconds(0)
      shinyjs::toggleState(id = "submit_answer", condition = FALSE)
    }
    if (input$submit_answer > length(task_number)){
    }
    else{
      saveData(formData())
    }
  })
  

  # Definition of the function that saves the data
  saveData <- function(data) {
    fileName1 <- sprintf("app_results.tsv")
    responsesDir <- file.path("response/")
    file = file.path(responsesDir, fileName1)
    reading <- as.data.frame(fread("response/app_results.tsv", header = FALSE, fill=TRUE))
    if (dim(reading)[2] == 0){
      RV$answers <- data
      write.table(x = data, file = file.path(responsesDir, fileName1), append = TRUE, 
                  row.names = FALSE, col.names = TRUE, qmethod = "double")
      
    }
    else{
      write.table(x = data, file = file.path(responsesDir, fileName1), append = TRUE,
                  row.names = FALSE, col.names = FALSE, qmethod = "double") 
      RV$answers <- as.data.frame(fread("response/app_results.tsv", header = TRUE, fill=TRUE))
    }
  }
  
  # Go back and forth between HTML and questionnaire
  questions = reactiveVal(FALSE)
  observeEvent(input$questions, {
    questions(TRUE)
    updateTextInput(session, "html_frame", value = includeHTML("www/iframe2_link.html"))
    updateTextInput(session, "html_frame_1", value = includeHTML("www/iframe1_link.html"))
  })
  
  observeEvent(input$back, {
    questions(FALSE)
  })
  
  # Disable back button when the survey is submitted
  disableActionButton <- function(id,session) {
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
  }
  
  # MAKE DATA REACTIVE
  RV <- reactiveValues(data = users, tasks = task_description, survey = questions_drive, answers = app_responses, text = text_config, text_survey = text_config_survey )
  user_data <- reactive({RV$data})
  t_descrip <- reactive({RV$tasks})
  questionss <-reactive({RV$survey})
  responses <- reactive({RV$answers})
  textconfig <- reactive({RV$text})
  textconfig_survey <- reactive({RV$text_survey})
  
  # CHANGE SOME CONFIGURABLE TEXT
  observeEvent(input$change_survey_title, {
    RV$text_survey['survey_title',] <- input$survey_page_title
    write.table(x = RV$text_survey, file = file.path("CSV_inputs/", "text_survey.csv"), append = FALSE,
                row.names = TRUE, col.names = TRUE, sep = ",", qmethod = "double")
    
   
    
  })

  observeEvent(input$change_task_title, {
    RV$text['task_page_title',] <- input$task_page_title
    write.table(x = RV$text, file = file.path("CSV_inputs/", "text.csv"), append = FALSE,
                row.names = TRUE, col.names = TRUE, sep = ",", qmethod = "double")
    
    
  })
  
  observeEvent(input$change_finished_tasks_title, {
    RV$text['finished_tasks_title',] <- input$finished_tasks_title
    write.table(x = RV$text, file = file.path("CSV_inputs/", "text.csv"), append = FALSE,
                row.names = TRUE, col.names = TRUE, sep = ",", qmethod = "double")
  })
  
  observeEvent(input$change_finished_tasks_descrip, {
    RV$text['finished_tasks_descrip',] <- input$finished_tasks_descrip
    write.table(x =  RV$text, file = file.path("CSV_inputs/", "text.csv"), append = FALSE,
                row.names = TRUE, col.names = TRUE, sep = ",", qmethod = "double")
  })
  
  
  # INSERT NEW ROW
  observeEvent(input$new_row, {
    if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
      userDir <- "CSV_inputs/"
      if (input$tabs =="Users"){
        name <- input$uname
        pass <- input$passs
        engine <- as.numeric(input$eng)
        permit <- as.logical(input$permit)
        RV$data <- RV$data %>% add_row(USER_NAME = name, Password = pass, Engine = engine, Admin=permit)
        write.table(x = RV$data, file = file.path(userDir, "users.csv"), append = FALSE,
                    row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
      }
      else if (input$tabs == "Task Configuration"){
        if(dim(task_descriptionn())[1]>0){
        RV$tasks <- RV$tasks %>% add_row(TASK = paste("Task"," ",dim(RV$tasks)[1]+1), DESCRIPTION= input$DESCRIPTION,
                                         TIME_MIN = input$TIME_MIN, TIME_SEC = input$TIME_SEC,
                                         COUNTDOWN_MESSAGE = input$COUNTDOWN_MESSAGE)}
        else {
          RV$tasks <- data.frame(TASK = paste("Task"," ",dim(RV$tasks)[1]+1), DESCRIPTION= input$DESCRIPTION,
                                    TIME_MIN = input$TIME_MIN, TIME_SEC = input$TIME_SEC,
                                    COUNTDOWN_MESSAGE = input$COUNTDOWN_MESSAGE)
        }
        write.table(x = RV$tasks, file = file.path(userDir, 'tasksdescrip.csv'), append = FALSE,
                    row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
      }
      else if (input$tabs == "Questionnaire"){
        
        RV$survey <- RV$survey %>% add_row(question = input$question , option= input$option ,
                                           input_type  = input$input_type, input_id = input$input_id,
                                           dependence  = NA, dependence_value = NA, required = TRUE)
        
        
        write.table(x = RV$survey, file = file.path(userDir, 'questions.csv'), append = FALSE,
                    row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
      }
    }
  }) 
  
  # DELETE ROW
  observeEvent(input$delete_row, {
    userDir <- "CSV_inputs/"
    if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
      if (!is.null(input$usertable_rows_selected)) {
        if (input$tabs =="Users"){
          if(any(RV$data[-as.numeric(input$usertable_rows_selected),][,4])==TRUE ){
              RV$data <- RV$data[-as.numeric(input$usertable_rows_selected),]
              row.names(RV$data) <- NULL
              write.table(x = RV$data, file = file.path(userDir, "users.csv"), append = FALSE,
                          row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
          }
          else {
            showModal(modalDialog(
              title = "Warning!",
               'You cannot delete all users with admin permission'
            ))
          }
        }}
      if(!is.null(input$tasktable_rows_selected)){
        if (input$tabs == "Task Configuration"){
          RV$tasks <- RV$tasks[-as.numeric(input$tasktable_rows_selected),]
          row.names(RV$tasks) <- NULL
          if (dim(task_descriptionn())[1] > length(input$tasktable_rows_selected)){
          RV$tasks$TASK <- paste("Task",c(1:dim(RV$tasks)[1]))
          }
          write.table(x = RV$tasks, file = file.path(userDir, "tasksdescrip.csv"), append = FALSE,
                      row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
        }}
      if(!is.null(input$questiontable_rows_selected )){
        if (input$tabs == "Questionnaire"){
          RV$survey <- RV$survey[-as.numeric(input$questiontable_rows_selected),]
          row.names(RV$survey) <- NULL
          write.table(x = RV$survey, file = file.path(userDir, "questions.csv"), append = FALSE,
                      row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
        }}
    }
  }) 
  observeEvent(input$delete_row_response, {
    if(!is.null(input$responsetable_rows_selected )){
        RV$answers <- RV$answers[-as.numeric(input$responsetable_rows_selected),]
        row.names(RV$answers) <- NULL
        write.table(x = RV$answers, file = file.path('response/', "app_results.tsv"), append = FALSE,
                    row.names = FALSE, col.names = TRUE, qmethod = "double")
    }
  })
  
  # RENDER DATA TABLES IN SIDEBAR MENU TABS
  output$usertable <- DT::renderDataTable(user_data(), options = list(scrollX = TRUE))
  output$tasktable <- DT::renderDataTable(server=FALSE,{datatable(RV$tasks,
                                                        extensions = "RowReorder",  
                                                        callback = JS(callback_tasks),
                                                        options = list(scrollX = TRUE,rowReorder = TRUE,order = list(list(0, 'asc')))
                                                        )})
  output$responsetable <- DT::renderDataTable(responses(),options = list(scrollX = TRUE))
  output$questiontable <- DT::renderDataTable(server=FALSE,{datatable(questionss(), 
                                                                  extensions = "RowReorder",  
                                                                  callback = JS(callback_questions),
                                                                  options = list(scrollX = TRUE,rowReorder = TRUE,order = list(list(0, 'asc')))
                                                                  )})
  isolate(output$texttable <- DT::renderDataTable(textconfig(), options = list(scrollX = TRUE)))
  isolate(output$texttable_survey <- DT::renderDataTable(textconfig_survey(), options = list(scrollX = TRUE)))
  
  # ROW REORDER FOR TASK AND QUESTIONNAIRE CSV
  observeEvent(input[["rowreorder"]], {
    old <- unlist(input[["rowreorder"]]$old)
    new <- as.numeric(unlist(input[["rowreorder"]]$new))
    RV$tasks[new, ] <- RV$tasks[old, ]   
    RV$tasks$TASK <- paste("Task",c(1:dim(RV$tasks)[1]))
    userDir <- "CSV_inputs/"
    write.table(x = RV$tasks, file = file.path(userDir, "tasksdescrip.csv"), append = FALSE,
                row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
    
  })
  observeEvent(input[["rowreorder_quest"]], {
    old <- unlist(input[["rowreorder_quest"]]$old)
    new <- as.numeric(unlist(input[["rowreorder_quest"]]$new))
    RV$survey[new, ] <- RV$survey[old, ]   
    userDir <- "CSV_inputs/"
    write.table(x = RV$survey, file = file.path(userDir, "questions.csv"), append = FALSE,
                row.names = FALSE, col.names = TRUE, sep = ",", qmethod = "double")
    
  })
  
  
  # CONFIGURATION TAB
  configtab <- 
    tabItem(tabName ="config",
            fluidPage(
              tabsetPanel(id="subtabs", type="pills",
              tabPanel("General Configurations",
                     tabsetPanel(type = "tabs", id = "tabs", 
                        footer = list(div(actionButton("new_row", "Add new row"),style="float:left"),
                                                                  div(actionButton("delete_row","Delete a row"),style="float:right")),
                        tabPanel("Users", 
                                 box(
                                   width = NULL,
                                   status = "primary",
                                   h4(icon("info-circle"),"Do not use the same Username for different users"),
                                   DT::dataTableOutput("usertable"),
                                   textInput("uname", "User Name"),
                                   textInput('passs','Password'),
                                   selectInput("eng", ("Engine"),
                                               choices = list("1" = 1, "2" = 2), selected = 2),
                                   selectInput("permit", ("Admin Permit"),
                                               choices = list("TRUE" = TRUE, "FALSE" = FALSE), selected = FALSE),
                                   
                                 )
                        ),
                        tabPanel("Task Configuration", 
                                 box(
                                   width = NULL,
                                   status = "primary",
                                   h4(icon("info-circle"),"You can drag the rows to change the order"),
                                   DT::dataTableOutput("tasktable"),
                                   textInput("DESCRIPTION", 'Task description'),
                                   numericInput('TIME_MIN','Minutes',10,
                                                min = 0, max = 60),
                                   numericInput('TIME_SEC','Seconds', 0,
                                                min = 0, max = 60),
                                   textInput('COUNTDOWN_MESSAGE','Countdown Message'),
                                 )
                        ),
                        tabPanel("Questionnaire",
                                 box(
                                   width = NULL,
                                   status = "primary",
                                   h4(icon("info-circle"),HTML("You can drag the rows to change the order.<br>Look at the “shinysurveys” r package documentation to know which kind of questions are supported in this survey.")),
                                   DT::dataTableOutput("questiontable"),
                                   textInput("question", "Question"),
                                   textInput("option", 'Option'),
                                   textInput('input_type','Input Type '),
                                   textInput('input_id','Input ID'),
                                 )
                        ))),
            tabPanel("Text Configurations",
                     tabsetPanel(type = "tabs", id = "tabs2",
                                 tabPanel("Tasks Text",
                                 box(
                                   width = NULL,
                                   status = "primary",
                                   DT::dataTableOutput("texttable"),
                                   splitLayout(cellWidths = c("60%", "40%"),div(textInput("task_page_title", "Task Page Title")), div(style = "margin-top: 25px;",actionButton("change_task_title","Change task title"))),
                                   splitLayout(cellWidths = c("60%", "40%"),div(textInput("finished_tasks_title", "Finish tasks title")), div(style = "margin-top: 25px;", actionButton("change_finished_tasks_title","Change finish task title"))),
                                   splitLayout(cellWidths = c("60%", "40%"),div(textInput("finished_tasks_descrip", "Finish tasks description")), div(style = "margin-top: 25px;", actionButton("change_finished_tasks_descrip","Change finish task description"))),
                                 )),
                                  tabPanel("Survey Text",
                                    box(width = NULL,
                                         status = "primary",
                                         DT::dataTableOutput("texttable_survey"),
                                         splitLayout(cellWidths = c("60%", "40%"),div(textInput("survey_page_title", "Survey Page Title")), div(style = "margin-top: 25px;",actionButton("change_survey_title","Change survey title"))),
                                        )
                                            
                                    ))))))
  # RESULTS TAB
  resultstab <- tabItem(
    tabName="results",
    tabsetPanel(type = "tabs", id = "tabs", footer = list(#actionButton("new_row", "Add new row"),
                                                          actionButton("delete_row_response","Delete a row")),
    tabPanel("Responses",
             box(
               width = NULL,
               status = "primary",
               DT::dataTableOutput("responsetable"),
               downloadButton("downloadData", "Download"),
               actionButton("clear_responses", "Clear all the responses"),
             )
    ))
  )
  
  # HTML TAB
  markdowntab <- tabItem(
    tabName="init_markdown",
    tabsetPanel(type = "tabs", id = "tabss", 
      tabPanel("HTML",
               box(
                 width = NULL,
                 status = "primary",
                 htmlOutput("render_after_upload"),
                 fileInput("file1", "Choose html file to include in the initial explanation",
                           multiple = FALSE,
                           accept = c(".html")),
                 downloadButton("downloadMardown", "Download current html"),
               )
      )))
  
  # ENGINE LINKS TAB
  enginetab <- isolate(tabItem(
    tabName="en_link",
    tabsetPanel(type = "tabs", id = "tabss", 
                tabPanel("Change the iframe src",
                         box(
                           width = NULL,
                           status = "primary",
                           h3("Src Engine 1"),
                           textInput("html_frame_1", label = h4("Please change only the iframe src:", tipify(icon("info-circle"), title ="KeyQ does not load every time the page is changed. To know the number of pages visited per user we must see the Google Analytics page of this engine.", placement ="right")), value = isolate(includeHTML(file.path("www/iframe1_link.html")))),
                           actionButton("save_frame_1","Save Iframe 1"),
                           h3("Src Engine 2"),
                           textInput("html_frame", label = h4("Please change only the iframe src:", tipify(icon("info-circle"), title = "The current src implements a reverse proxy, with the Cloudflare service, designed to bypass some Cross-Origin Resource Sharing restrictions. In our case, we could not display the Pubmed URL in an iframe because of the same origin policy. If you change this src and you get the same problem with a new URL, you can also overcome it with the “Ignore X-Frame headers” Chrome or Mozilla extensions.", placement ="right")), value = isolate(includeHTML(file.path("www/iframe2_link.html")))),
                           actionButton("save_frame","Save Iframe 2")
                         )
                         
                ))))
  
  # SAVE NEW IFRAME SRC
  observeEvent(input$save_frame, {
    write.table(input$html_frame, 
                file="www/iframe2_link.html", 
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    updateTextInput(session, "html_frame", value = includeHTML("www/iframe2_link.html"))
  })
  
  observeEvent(input$save_frame_1, {
    write.table(input$html_frame_1, 
                file="www/iframe1_link.html", 
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    updateTextInput(session, "html_frame_1", value = includeHTML("www/iframe1_link.html"))
  })
  
  
  ## UI BODY
  output$body <- renderUI({
    shiny:::hasCurrentRestoreContext()
    if (USER$login == TRUE ) {
      i<-match(input$userName,users$USER_NAME)
      num_engine <- paste("tab",users$Engine[i],sep="")
      num_engine_admin <- paste("tab",input$type_engine,sep="")
      if (PAGE$splash == FALSE ){
        if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
          tabItems(
            tabItem(
              tabName ="splash", 
              class = "active",
                    fluidRow(
                      if (questions() == FALSE){
                        box(width = 12,
                            htmlOutput("first_html"),
                            actionButton("questions", "Go to questionnaire", class = "btn-primary"),)}
                      else if(questions()) {
                        isolate(title_for_survey <- textconfig_survey()["survey_title",])
                        if (!is.null(input$rerender_survey)){
                        fluidRow(
                          column(12, align = "center",
                          box(
                              width = 12,
                              div(id="btitle",h1(isolate(title_for_survey))),
                              div(id="bSurvey",source("shiny_survey.R",local = TRUE)$value),
                              div(style = "height:20px;"),
                              div(actionButton("back", "Back", class = "btn-primary"), style="float:right"),
                              uiOutput("showbuttons")
                          )))
                        }
                        else{
                          fluidRow(
                            column(12, align = "center",
                                   box(
                                     width = 12,
                                     div(id="btitle",h1(title_for_survey)),
                                     div(id="bSurvey",source("shiny_survey.R",local = TRUE)$value),
                                     div(style = "height:20px;"),
                                     div(actionButton("back", "Back", class = "btn-primary"), style="float:right"),
                                     uiOutput("showbuttons")
                                   )))
                        }}
                    )),
            configtab,
            resultstab,
            markdowntab,
            isolate(enginetab)
            )
        }
        else {
          tabItems(
            tabItem(
              tabName ="splash",
                    class = "active",
                    fluidRow(
                      if (questions() == FALSE){
                        box(width = 12,
                            htmlOutput("first_html"),
                            actionButton("questions", "Go to questionnaire", class = "btn-primary"),)}
                      else{
                        title_for_survey <- textconfig_survey()["survey_title",]
                        fluidRow(
                          column(12, align = "center",
                                 box(
                                   width = 12,
                                   div(id="btitle",h1(title_for_survey)),
                                   div(id="bSurvey",source("shiny_survey.R",local = TRUE)$value),
                                   div(style = "height:20px;"),
                                   div(actionButton("back", "Back", class = "btn-primary"), style="float:right"),
                                   uiOutput("showbuttons")
                                 ),
                                 ))}
                    )))
          
        }}
      else{
        if(credentials["admin"][which(credentials$username_id==isolate(input$userName)),]){
          tabItems(
            tabItem(tabName ="engine", class = "active",
                    fluidRow(
                      column(12, align = "center",
                      box(width = 12, 
                          div(id="btitle",h1(isolate(textconfig()["task_page_title",]))),
                          div(id="bSurvey",
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
                          div(style="height:5px;"),
                          div(id="bCountdown",h4(textOutput('timeleft'))),
                          tags$head(tags$style("#timeleft{color: red;
                                   font-size: 20px;``
                                   }"
                          ))),
                          div(style="height:7px;"),
                          htmlOutput(num_engine_admin),
                          div(style="height:5px;"),
                          textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "100px"),
                          actionButton("submit_answer", "Submit", class = "btn-primary"),
                      )))),
            resultstab)
        }
        else {
          tabItems(
            tabItem(tabName ="engine", class = "active",
                    fluidRow(
                      column(12, align = "center",
                      box(width = 12, 
                          div(id="btitle",h1(isolate(textconfig()["task_page_title",]))),
                          div(id="bSurvey",
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
                          div(style="height:5px;"),
                          div(id="bCountdown",h4(textOutput('timeleft'))),
                          tags$head(tags$style("#timeleft{color: red;
                                   font-size: 20px;``
                                   }"
                          )),
                          div(style="height:7px;"),
                          htmlOutput(num_engine),
                          div(style="height:5px;"),
                          textAreaInput("txt", labelMandatory("Enter the answer below:"),height = "100px"),
                          actionButton("submit_answer", "Submit", class = "btn-primary"),
                      )))))
          ) 
        }
      }
      
    }
    else {
      loginpage
    }
  })
  # end of ui body
  
  # If the survey has been submitted, show start button and disable back button
  sub <- reactiveFileReader(1000, session, "submitted.rds", readRDS)
  observe(
    if(sub()){
      disableActionButton("back",session)
      output$showbuttons <- renderUI({ 
            div(actionButton("start", "Start", class = "btn-primary"), style="float:left")
      })
      saveRDS(FALSE, file= "submitted.rds")
    })
  
  # Render task text
  output$task <- renderText({
    task_numberr <- c()
    z = c(1:length(task_descriptionn()$TASK))
    for(val in z){
      task_numberr <- append(task_numberr, toString((task_descriptionn()$TASK)[val]))
    }
    if (input$submit_answer+1 > length(task_numberr) | dim(task_descriptionn())[1]==0 ){
      (textconfig()["finished_tasks_title",])
    }
    else{
      paste(task_numberr[input$submit_answer +1],"of",length(task_numberr))
    }
  })
  
  #Render task description text
  output$descrip <- renderText({
    task_numberr <- c()
    z = c(1:length(task_descriptionn()$TASK))
    for(val in z){
      task_numberr <- append(task_numberr, toString((task_descriptionn()$TASK)[val]))
    }
    if (input$submit_answer+1 > length(task_numberr) | dim(task_descriptionn())[1]==0){
      if((credentials["admin"][which(credentials$username_id==isolate(input$userName)),])==FALSE){
        file.remove('df.RData')
        }
        textconfig()["finished_tasks_descrip",]
    }
    else{
      task_dess <- c()
      z = c(1:length(task_descriptionn()$TASK))
      for(val in z){
        task_dess <- append(task_dess, toString((task_descriptionn()$DESCRIPTION)[val]))
      }
      task_dess[input$submit_answer +1]
    }
  })
  
  # Render html's with the iframes 
  getPage1<-function() {
    return(includeHTML("www/iframe1.html"))
  }
  getPage2<-function() {
    return(includeHTML("www/iframe2.html"))
  }
  
  output$tab1<-renderUI({getPage1()})
  output$tab2<-renderUI({getPage2()})
  count <- reactive({ input$count })
  array_of_times <- reactive({input$time_stamp_load})
  array_of_sec <- reactive({input$time_stamp_load_seconds})
  
  observeEvent(input$submit_answer,{
    output$tab1<-renderUI({getPage1()})
    output$tab2<-renderUI({getPage2()})
    count <- reactive({ input$count }) 
    array_of_times <- reactive({input$time_stamp_load})
    array_of_sec <- reactive({input$time_stamp_load_seconds})
  })
  
  # Render countdown clock
  output$timeleft <- renderText({
    if(dim(task_descriptionn())[1] ==0){
      minutes(0)
      seconds(0)}
      paste("Time left:", minutes(), "M", seconds(), "S")
  })
  
  # Download button for response data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      openxlsx::write.xlsx(responses(), file)
    })
  
  # Clear all responses button
  observeEvent(input$clear_responses,{
    fileName1 <- sprintf("app_results.tsv")
    responsesDir <- file.path("response/")
    file = file.path(responsesDir, fileName1)
    RV$answers <- data.frame()
    write.table(x = RV$answers, file = file.path(responsesDir, fileName1), append = FALSE, 
                row.names = FALSE, col.names = FALSE, qmethod = "double")
  })
  
  # Include initial HTML
  getHTML<-function() {
    return(includeHTML("init_html.html"))
  }
  # Render initial HTML
  output$first_html <- renderUI({getHTML()})
  
  # Upload HTML in HTML tab
  observeEvent(input$file1,{
    html_dat <- read_html(input$file1$datapath)
    html_dat<- paste(as.character(html_dat), collapse = "\n")
    write.table(html_dat, 
                file='init_html.html', 
                quote = FALSE,
                col.names = FALSE,
                row.names = FALSE)
    output$render_after_upload<-renderUI({getHTML()})
    output$first_html <- renderUI({getHTML()
      
    })
  })
  
  # Donwload current initial HTML
  output$downloadMardown <- downloadHandler(
    filename = function() {
      "init_html.html"
    },
    content = function(file) {
      html_dat <- read_html("init_html.html")
      html_dat<- paste(as.character(html_dat), collapse = "\n")
      write.table(html_dat, 
                  file=file, 
                  quote = FALSE,
                  col.names = FALSE,
                  row.names = FALSE)
    })
  
  # Change the iframe src tab and minutes/seconds 
  observeEvent(input$type_engine,{
    task_description <- read.csv('CSV_inputs/tasksdescrip.csv')
    min <- c(task_description$TIME_MIN)
    sec <- c(task_description$TIME_SEC)
    minutes(min[1])
    seconds(sec[1])
    updateTextInput(session, "html_frame", value = includeHTML("www/iframe2_link.html"))
    updateTextInput(session, "html_frame_1", value = includeHTML("www/iframe1_link.html"))
  })
  
  # We start the tasks
  observeEvent(input$start,{
    PAGE$splash <- TRUE })
  
}