#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram


ui <- fluidPage(
   
   # Application title
   titlePanel("Van den Bulte Simulation"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(12,
            "Google Trends",
            fluidRow(
              column(6,
                     selectInput('Search_Tearm', 'Search Term:',
                                 choices = c("Blu Ray"="Blu Ray",
                                                "Avengers"="Avengers",
                                                "Etherum"="Etherum")),
                     )
              ),
              column(width = 6,
                     "Fluid 6")
            )
     )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

