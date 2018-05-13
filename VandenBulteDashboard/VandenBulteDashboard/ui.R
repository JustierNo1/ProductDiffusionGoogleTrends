#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Van den Bulte Simulation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("search_term",
                   "Search Term:",
                   c("Etherum"="Etherum",
                     "Iphone X"="Iphone X",
                     "World of Warcraft"="World of Warcraft")),
       dateRangeInput("time", label = "Date range", start = "2017-12-01"),
       numericInput("Population","Population Size:",
                    value=10000,
                    min=100,
                    max=100000),
       sliderInput("p1","p1:",min=0,max=1,step = 0.05, value=0.005),
       sliderInput("q1","q1:",min=0.01,max=1,step = 0.05, value=0.23),
       sliderInput("p2","p2:",min=0,max=1,step = 0.05, value=0),
       sliderInput("q2","q2:",min=0,max=1,step = 0.05, value=0.21),
       sliderInput("theta","theta:",min=0,max=1,step = 0.01, value=0.37),
       sliderInput("w","w:",min=0.001,max=1,step= 0.01, value=0.0018)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
       plotOutput("GoogleTrends")),
      fluidRow(
       plotOutput("Simulation"))
    )
  )
))
