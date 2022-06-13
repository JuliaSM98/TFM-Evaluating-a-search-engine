options(shiny.error = browser)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(lubridate)
library(base)
library(tidyverse)
library(xlsx)
library(shinysurveys)
library(data.table)
library(bcrypt) 
library(xml2)
library(shinyBS)

# Mandatory star css
appCSS <- ".mandatory_star { color: red; }
           } "

## PARTS OF THE UI
header <- dashboardHeader( title = "", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
# We define the WaitUntil function so that the head loads before the jQueries in the html documents of the server
body <- dashboardBody(
  tags$script(HTML(
    "
            var WaitUntil = (function(){
                var waitUntil=function(condfunc, callback, interval, trys){
                    var getGuid = getGuid||function() {
                        var d = new Date().getTime();
                        var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
                            var r = (d + Math.random()*16)%16 | 0;
                            d = Math.floor(d/16);
                            return (c=='x' ? r : (r&0x3|0x8)).toString(16);
                        });
                        return uuid;
                    };
                    var timer = {}, counter={};
                    var waiter = function(condfunc, callback, interval, trys, guid){
                        guid = guid || getGuid();
                        interval = interval || 100;
                        trys = trys || 300;
                        counter[guid] = counter[guid] ? counter[guid]++ : 1;
                        if(counter[guid]>trys){
                            if(timer[guid]) clearTimeout(timer[guid]);
                            //callback('fail');
                            return;
                        }
                        timer[guid] = setTimeout(function(){
                            if(condfunc()){
                               if(timer[guid]) clearTimeout(timer[guid]);
                                callback('ok');
                            } else {
                                if(timer[guid]) clearTimeout(timer[guid]);
                                waiter(condfunc, callback, interval, trys, guid);
                            }
                        }, interval);
                    }
                    waiter(condfunc, callback, interval, trys);
                }
                return waitUntil;
            })();            
            "
  )),
  shinyjs::useShinyjs(), shinyjs::inlineCSS(appCSS), uiOutput("body")
)

## UI
ui <- dashboardPage(header, sidebar, body,
                    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap_custom.css"),tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
        function(message) {
          console.log(message)
          eval(message.code);
        }
      );
    '))), skin = "blue")
                    