#Fagr shiny

library(shiny)
library(shinyjs)
library(glue)
library(tidyverse)
library(lubridate) 
library(httr)

# Define UI for displaying current time ----
ui <- fluidPage(
    useShinyjs(),
    htmlOutput("inc1"),
    textInput("zipp", label = h3("Your ZIP code:"), value = ""),
    radioButtons("calc", label = h3("Prayer calculation Method:"),
                 choices = list("ISNA - Islamic Society of North America" = 2, "MWL - Muslim World League." = 3), 
                 selected = 2),
    actionButton("act", "Submit"),
    hr(),
    htmlOutput("inc"),
    h2(textOutput("localtime")),
    h2(textOutput("tillFagr")),
    br(),
    br(),
    h2(textOutput("target1")),
    h2(textOutput("till1")), 
    uiOutput('my_audio1')
    
)