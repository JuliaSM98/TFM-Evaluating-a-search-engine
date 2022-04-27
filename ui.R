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
library(RSQLite)
library(DBI)
library(tidyverse)
library(markdown)

appCSS <- ".mandatory_star { color: red; }
           } "

header <- dashboardHeader( title = "Welcome", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), shinyjs::inlineCSS(appCSS), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")