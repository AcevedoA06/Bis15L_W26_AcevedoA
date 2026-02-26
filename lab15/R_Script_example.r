library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)


server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    msleep %>% 
      filter(vore!="NA") %>% 
      ggplot(aes(x = vore,
                 y = .data[[input$y]], 
                 fill = vore))+
      geom_boxplot(alpha=0.75)+
      labs(title = "Sleep Variable by Vore Type",
           x="Vore",
           fill="Vore Type")+
      theme_minimal()
  })
}

shinyApp(ui, server)