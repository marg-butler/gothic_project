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
library(wordcloud)
library(RColorBrewer)
theme_set(theme_classic())


sentiment_time <- read_rds("sentiment_time.rds")
tidy_gothic <- read_rds("tidy_gothic.rds")
messy_gothic <- read_rds("messy_gothic.rds")
cloudish_gothic <- read_rds("cloudish_gothic.rds")
dense_gothic <- read_rds("dense_gothic.rds")
reg_gothic <- read_rds("reg_gothic.rds")

### UI


#### Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("slate"),
    
    navbarPage(
        "Gothic Literature and Monstrosity",
        tabPanel(
            title = "About",
            h5("By Margaret Butler"),
            h2("Tracking the Monstrous Across Five Gothic Novels"),
            p(
                "Monstrosity in literature, gothic lit in particular, tends to evoke specific images and draws on a very specific linguistic tradition. 
                I was initially curious about tracking shared word usage and pronoun usage in classic gothic novels that investigate the relationship between 
                humanity and monstrosity."
            ),
            br(),
            p(
              "Pronouns are used in books such as Mary Shelley's "
              ),
            em("Frankenstein"),
              (" to connote a transition from a character's perception as human to monstrous. 
              I was interested in seeing if this was a tradition one could track statistically in other books or if it was even a a traceable trend that five 
              classic examples of monster literature followed."
              ),
            br(),
            p(
            "The first few graphics I have give visualization to words trends throughout the novels—more specifically, we get to see a visual representation 
            of what exactly makes these novels “gothic”: the negative sentiment, the specific words used, and so on. This is the springboard into pronoun and 
            monstrosity analysis of the greater question: can looking at word usage analytically help us identify negativity and monstrosity in literature?"
              ),
            br(),
            h3("Summaries"),
            h5("Carmilla"),
            h5("Dracula"),
            h5("Frankenstein"),
            h5("The Phantom of the Opera"),
            h5("The Strange Case of Dr. Jekyll and Mr. Hyde"),
            br(),
            h3("About the Author")
            
        ),
        tabPanel(
           "Sentiment Analysis",
           htmlOutput("sentintro"),
           selectInput("title", "Select Text:",
                       c("Carmilla" = "Carmilla",
                         "Dracula" = "Dracula",
                         "Frankenstein" = "Frankenstein; Or, The Modern Prometheus",
                         "The Phantom of the Opera" = "The Phantom of the Opera",
                         "Jekyll and Hyde" = "The Strange Case of Dr. Jekyll and Mr. Hyde"
                         )),
           plotOutput("sentplot")
        ),
        tabPanel(
            "Most Common Words",
            htmlOutput("comintro"),
            sidebarLayout(
                sidebarPanel(
                    checkboxGroupInput("books", "Novel",
                                       c("Carmilla" = "Carmilla",
                                         "Dracula" = "Dracula",
                                         "Frankenstein" = "Frankenstein; Or, The Modern Prometheus",
                                         "The Phantom of the Opera" = "The Phantom of the Opera",
                                         "Jekyll and Hyde" = "The Strange Case of Dr. Jekyll and Mr. Hyde"),
                                       selected = c("Carmilla", "Dracula", "Frankenstein; Or, The Modern Prometheus", "The Phantom of the Opera", "The Strange Case of Dr. Jekyll and Mr. Hyde")
                    ),
                    checkboxGroupInput("words", "Word",
                                       c( "time" = "time",
                                          "night" = "night",
                                          "eyes" = "eyes",
                                          "door" = "door",
                                          "day" = "day",
                                          "hand" = "hand",
                                          "dear" = "dear",
                                          "life" = "life",
                                          "found" = "found",
                                          "voice" = "voice",
                                          "he" = "he",
                                          "she" = "she",
                                          "it" = "it",
                                          "creature" = "creature",
                                          "monster" = "monster"  
                                       ),
                                       selected = c("time", 
                                                    "night", 
                                                    "eyes", 
                                                    "day", 
                                                    "door",
                                                    "hand",
                                                    "dear",
                                                    "life",
                                                    "found",
                                                    "voice")
                    )  
                ),
                mainPanel(
                    plotOutput("regrplot")
                )
            )
        ),
        tabPanel(
            "Monstrosity",
            sidebarLayout(
                sidebarPanel(
                    selectInput("novel", "Novel",
                                c("Carmilla" = "Carmilla",
                                  "Dracula" = "Dracula",
                                  "Frankenstein" = "Frankenstein; Or, The Modern Prometheus",
                                  "The Phantom of the Opera" = "The Phantom of the Opera",
                                  "Jekyll and Hyde" = "The Strange Case of Dr. Jekyll and Mr. Hyde")),
                    checkboxGroupInput("word", "Word:",
                                       c("time" = "time",
                                         "night" = "night",
                                         "eyes" = "eyes",
                                         "door" = "door",
                                         "day" = "day",
                                         "hand" = "hand",
                                         "dear" = "dear",
                                         "life" = "life",
                                         "found" = "found",
                                         "voice" = "voice",
                                         "he" = "he",
                                         "she" = "she",
                                         "it" = "it",
                                         "creature" = "creature",
                                         "monster" = "monster"),
                                       selected = "time")
                ),
                mainPanel(
                    plotOutput("densplot")
                )
            )
        )
    ))


###Server




server <- function(input, output) {
    output$sentintro <- renderUI({
        HTML(
            "The following graphics represent positive over negative sentiment throughout the course of each book. 
            As you can see if you poke around, sentiment in all the texts is overwhelmingly negative. This tracks 
            with what one expects of gothic literature; in stories about death and monstrosity, negative sentiment 
            really ought to be dominant. The question that emerges of this is: are books that have more negative 
            sentiment more monstrous?"
        )
    })
    output$sentplot <- renderPlot({
        sentiment_time %>% 
            filter(title == input$title) %>% 
            ggplot(aes(index, sentiment, fill = sentiment)) +
                geom_col(show.legend = FALSE) +
                labs(x = "Pages",
                     y = "Positive Over Negative Sentiment")
    })
    output$comintro <- renderUI({
        HTML(
            "The followin wordcloud represents the most frequent or common words in each novel. 
            The size of the word indicates how frequently it is used."
        )
    })
    output$regrplot <- renderPlot({
        reg_gothic %>% 
            filter(books == input$books) %>% 
            filter(words == input$words) %>% 
            filter(words == "time" |
                    words == "night" |
                    words == "eyes" |
                    words == "day" |
                    words == "door" |
                    words == "hand" |
                    words == "dear" |
                    words == "life" |
                    words == "found" |
                    words == "voice" |
                    words == "he" |
                    words == "she" |
                    words == "it" |
                    words == "creature") %>% 
            count(books, index = row_num %/% 100, words) %>% 
            ggplot(aes(index, n, color = words)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "lm", se = FALSE) +
            labs(
                x = "Line",
                y = "Frequency of Words"
            )
    })
    output$densplot <- renderPlot({
        dense_gothic %>% 
            filter(novel == input$novel) %>% 
            filter(word == input$word) %>%
            filter(word == "time" |
                       word == "night" |
                       word == "eyes" |
                       word == "day" |
                       word == "door" |
                       word == "hand" |
                       word == "dear" |
                       word == "life" |
                       word == "found" |
                       word == "voice" |
                       word == "he" |
                       word == "she" |
                       word == "it" |
                       word == "creature"
            ) %>% 
            ggplot(aes(x = index, group = word, fill = word)) +
            geom_density(adjust=1.5, alpha=.4)
    })
    
    

    
    
    
    
    
}


shinyApp(ui = ui, server = server)

