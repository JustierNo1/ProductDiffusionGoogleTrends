#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(gtrendsR)
library(ggplot2)
library(pracma)
library(data.table)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$GoogleTrends <- renderPlot({
    
    #Set system time
    Sys.setenv(TZ = "UTC")
    
    #Retrieve Data
    google.trends = gtrends(c(input$search_term), gprop = "web", time = paste(format(input$time[1]), format(input$time[2])))[[1]]
    google.trends = google.trends[,c("date","hits")]
    ##Plot data using smooth to see underlying pattern.
    ggplot(data=google.trends, aes(x= date, y = hits))+
      geom_line(size=1)+
      geom_smooth()+
      theme_classic()+
      labs(title = "Google Trends")+
      theme(axis.title = element_blank(),
            plot.title = element_text(size = rel(2)))
    
  })
  output$Simulation <- renderPlot({

    pop_immitators <- round((1-input$theta)*input$Population)
    pop_innovators <- round(input$theta*input$Population)
    F1 <- 0 #accumulated adopted innovators
    F2 <- 0 #accumulated adopted immitators
    
    #history of accumulated adoption
    F1_hist <- c()
    F2_hist <- c()
    
    #at the beginning nobody has adopted -> all zero
    innovators <- rep(0,pop_innovators)
    immitators <- rep(0,pop_immitators)
    
    #adoption Loop
    while (TRUE){
      #Calculate the adoption rates F1 and F2 after every period
      F1 <- length(which(innovators==1))/pop_innovators
      F2 <- length(which(immitators==1))/pop_immitators
      
      F1_hist <- c(F1_hist,F1)
      F2_hist <- c(F2_hist,F2)
      
      h1 <- input$p1+input$q1*F1 #Likelihood of adoption for innovators
      h2 <- input$p2+input$q2*(input$w*F1+(1-input$w)*F2) #likelihood of adoption for immitators
      
      #adoption threshold
      for (i in 1:length(innovators)){
        if (innovators[i]==1 | runif(1)<h1){
          innovators[i] <- 1
        }
      }
      for (i in 1:length(immitators)){
        if (immitators[i]==1 | runif(1)<h2){
          immitators[i] <- 1
        }
      }
      #exit condition is reached when innovators and immitators have fully adopted
      if (F1 >= 1 & F2>=1){
        break
      }
    }
    #Calculate the derivative
    f1 <- gradient(F1_hist)
    f2 <- gradient(F2_hist)
    
    #calculate the adoptions at t for innovators, immitators and both added
    adoptions_innov <- diff(F1_hist)
    adoptions_immit <- diff(F2_hist)
    adoptions_all <- adoptions_innov*input$theta+adoptions_immit*(1-input$theta)
    
    all <- data.frame(adoptions_innov,adoptions_immit,adoptions_all, time=seq_along(adoptions_innov))
    all <- melt(all, id.vars = "time")
    
    ggplot(data=all,aes(x=time,y = value,fill=variable,color=variable))+
      geom_line(size=1)+
      scale_color_manual(breaks=c("adoptions_all", "adoptions_innov", "adoptions_immit"),
                         labels=c("All", "Innovators", "Immitators"),
                         values = c("#0072B2", "#D55E00", "#CC79A7"))+
      labs(title = "Adoption Rate")+
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            legend.key.height = unit(3,"line"),
            legend.text = element_text(size=14),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(colour = "black"),
            legend.position=c(0.9,0.5),
            plot.title = element_text(size = rel(2)))
  })
  
})
