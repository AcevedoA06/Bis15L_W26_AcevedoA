
library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)


elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()


ui <- fluidPage(
  theme=shinythemes::shinytheme("superhero"),
  
  dashboardHeader(title="Comparison of Growth Factors by Sex"),
  
  dashboardSidebar(disable=T),
  
  dashboardBody(
    
    selectInput("y",
                "Select Variable",
                choices = c("height", "age"),
                selected = "height"),
    plotOutput("plot", width = "500px", height = "400px")
  ) 
)
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    elephants %>% 
      ggplot(aes( x=sex,
                  y=.data[[input$y]],
                  fill=sex))+
      geom_boxplot()+
      theme_minimal()
  })
  
}

shinyApp(ui, server)