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
            h2("Tracking the Monstrous Across Five Gothic Novels"),
            p(
                "Monstrosity in literature, gothic lit in particular, tends to evoke very specific images. As an avid reader of horror literature, 
                I wanted to look at text and word usage in some classic gothic literature texts. I chose five different gothic lit books to analyze 
                the word and pronoun usage within after asking the question: how do these books use language to define monstrosity? The pronoun 
                question stems from an observed shift in pronoun usage from he or she to it when the true nature of the creature or monster is revealed 
                in many of the texts. This project sprung from a desire to see if that shift was traceable."
            ),
            br(),
            h2("Synopses of the Five Books"),
            p(
              "The focus has shifted from looking primarily at pronoun usage to centering on the words authors use to signal gothic themes and monstrosity. 
              The pages that follow include my attempts to investigate the individual words that the authors use and see if there is a similar pattern 
              among the most frequently used words in each novel. Would such a pattern say anything about the works’ gothicism? Can looking at word usage 
              analytically help us identify negativity and monstrosity in literature?"
              ),
            tabsetPanel(type = "tabs",
                        tabPanel("Carmilla", htmlOutput("carmsum")),
                        tabPanel("Dracula", htmlOutput("dracsum")),
                        tabPanel("Frankenstein", htmlOutput("fransum")),
                        tabPanel("Phantom", htmlOutput("phansum")),
                        tabPanel("Jekyll & Hyde", htmlOutput("jeksum"))
            ),
            br(),
            h3("About the Author and the Sources"),
            p(
                "Hello, friends. My name is Margaret Butler, and I am an undergraduate at Harvard pursuing a joint concentration in English and the Comparative Study of Religion. 
                This project was completed for the Fall 2019 iteration of Gov 1005. While I’m reluctant to consider myself even a junior data scientist, I have enjoyed mixing data 
                science with one of my favorite areas of literature. The data from this project comes largely from Project Gutenberg, though I would be remiss if I did not thank the 
                authors of the texts themselves."
            )
            
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
            "Word Frequency",
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
    output$carmsum <- renderUI({
        HTML(
            "Laura"
        )
    })
    output$dracsum <- renderUI({
        HTML(
            "Jonathan Harker"
        )
    })
    output$fransum <- renderUI({
        HTML(
            "Vicktor Frankenstein"
        )
    })
    output$phansum <- renderUI({
        HTML(
            "Christine Daae"
        )
    })
    output$jeksum <- renderUI({
        HTML(
            "John Utterson"
        )
    })
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
            ggplot(aes(index, sentiment, fill = ifelse(sentiment < 0,'green','red'))) +
                geom_col(show.legend = FALSE) +
                labs(title = "Positive v. Negative Sentiment Over the Course of the Novel",
                    x = "Pages",
                     y = "Positive Over Negative Sentiment")
    })
    output$comintro <- renderUI({
        HTML(
            "The following regression traces the frequency of the top ten most frequent words across all five novels as well as the pronouns he, she, and it, and the words creature and monster."
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

