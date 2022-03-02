# ## Only run examples in interactive R sessions
library(shiny)
# library(stringr)
# 
# 
# task_descprition <- read.csv('CSV_inputs/tasks_descrip.csv')
# min <- c()
# sec <- c()
# y = c(1:length(task_descprition$TASK))
# for (val in y) {
#   t <- (str_split((task_descprition$TIME_MIN_SEC)[val],":"))
#   min <- append(min, as.integer(sapply(t,"[[",1)))
#   sec <- append(sec, as.integer(sapply(t,"[[",2)))
# }
# 
# i <- match(task_number[input$submit +1],task_descprition$TASK)
# 
# minutes <- reactiveVal(min[i])
# seconds <- reactiveVal(sec[i])
# ################
# minutes <- 10
# seconds <- 0
# 
# time <- function(minutes,seconds){
#   while (TRUE){
#     if (seconds == -1){
#       seconds = 59
#       minutes = (minutes - 1)
#     }
#     print(paste("Minutes", minutes, "Second",seconds))
#     seconds = (seconds - 1)
#     if (minutes ==0 & seconds ==-1){
#       break
#     }
#   }
# }
# 
# 
# minutes = NA
# if (is.na(minutes)){
#   paste("hola")}
# 
# ###############
# 
# min <- c()
# sec <- c()
# y = c(1:length(task_descprition$TASK))
# for (val in y) {
#   t <- (str_split((task_descprition$TIME_MIN_SEC)[val],":"))
#   min <- append(min, as.integer(sapply(t,"[[",1)))
#   sec <- append(sec, as.integer(sapply(t,"[[",2)))
# }
# 
# i <- match(task_number[input$submit +1],task_descprition$TASK)
# 
# minutes <- reactiveVal(min[i])
# seconds <- reactiveVal(sec[i])
# active <- reactiveVal(FALSE)
# 
# 
# observe({
#   invalidateLater(1000, session)
#   isolate({
#     if (USER$login == TRUE & minutes()>0 & seconds()>0) {
#       active(TRUE)
#       if(active())
#       {
#         if (seconds == -1){
#           seconds = 59
#           minutes = (minutes - 1)
#         }
#         seconds = (seconds - 1)
#         if(minutes ==0 & seconds ==-1)
#         {
#           active(FALSE)
#           showModal(modalDialog(
#             title = "Important message",
#             "Countdown completed!"
#           ))
#         }
#       }}
#   })
# })
# 
# observeEvent(input$submit, {
#   i <- match(task_number[input$submit +1],task_descprition$TASK)
#   
#   minutes(min[i])
#   seconds(sec[i])
#   })
# 
# library(lubridate)
# 
# # Output the time left.
# output$timeleft <- renderText({
#   paste("Time left: ", minutes, "M ", seconds, "S")
# })



