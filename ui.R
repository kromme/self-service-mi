library(TAL)
library(ggplot2)
library(RODBC)
library(dplyr)
library(caret)
library(plotly)
library(shiny)
library(shinydashboard)
source('functions.R')

header <- dashboardHeader(
  title = "Sauna Insights"
)

body <- dashboardBody(
  
  tabPanel("Parameters",
           
           fluidPage(
             
             
             fluidRow(
               column(4,
                      h4("1. Select analysis"),
                      uiOutput("choose_analysis"),
                      p(''),
                      h4("2. Select top decile to select"),
                      sliderInput('top', 'Top no of customers', 10, 60, 10, step = 10),
                      p(''),
                      h4("3. Choose variable"),
                      uiOutput("choose_column")
                      
               ),
               column(8,
                      
                      h3("Insights"),
                      p('This dashboards provides marketeers insights in all models currently running within the Data Science Center. 
                        '),
                      
                      h4('Distribution'),
                      p('Following graph shows the distribution per decile.'),
                      plotlyOutput('decileplot'),
                      
                      htmlOutput('statText'),
                      plotlyOutput('graph'),
                      
                      
                      p(''),p(''),p(''),p(''),p(''),p(''),
                      p('Jeroen.kromme')
                      
                      
                      )
             )
           )
  )
  
)


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE, width = 1),
  body
)
