#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(gutenbergr)
library(tidytext)
library(tidyverse)
library(fs)
library(shiny)
library(tidyverse)
library(shiny)
library(ggrepel)
library(datasets)
library(wordcloud2)
library(ggplot2)
library(stringr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(tidytext)
theme_set(theme_classic())


sentiment_time <- read_rds("sentiment_time.rds")


### UI


#### Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    navbarPage(
        "Monstrosity of Language in Gothic Lit",
        tabPanel(
            title = "Introduction"
        ),
        tabPanel(
           "Sentiment Analysis",
           htmlOutput("sentintro"),
           selectInput("title", "Novel",
                       c("Carmilla" = "Carmilla",
                         "Dracula" = "Dracula",
                         "Frankenstein" = "Frankenstein; Or, The Modern Prometheus",
                         "The Phantom of the Opera" = "The Phantom of the Opera",
                         "Jekyll and Hyde" = "The Strange Case of Dr. Jekyll and Mr. Hyde"
                         )),
           plotOutput("sentplot")
        )
    ))


###Server




server <- function(input, output) {
    output$sentintro <- renderUI({
        HTML(
            "Yee haw"
        )
    })
    output$sentplot <- renderPlot({
        sentiment_time %>% 
            filter(title == input$title) %>% 
            ggplot(aes(index, sentiment, fill = title)) +
                geom_col(show.legend = FALSE)
    })

    
    
    
    
    
}


shinyApp(ui = ui, server = server)

