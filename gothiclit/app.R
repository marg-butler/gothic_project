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
            p(
              "The focus has shifted from looking primarily at pronoun usage to centering on the words authors use to signal gothic themes and monstrosity. 
              The pages that follow include my attempts to investigate the individual words that the authors use and see if there is a similar pattern 
              among the most frequently used words in each novel. Would such a pattern say anything about the works’ gothicism? Can looking at word usage 
              analytically help us identify negativity and monstrosity in literature?"
              ),
            h2("Unfamiliar with the Books? Start Here"),
            tabsetPanel(type = "tabs",
                        tabPanel("Carmilla", 
                                 sidebarLayout(
                                     sidebarPanel(img(src = "Carmilla.png", height = 350, width = 200)),
                                     mainPanel(htmlOutput("carmsum")))
                                 ),
                        tabPanel("Dracula", 
                                 sidebarLayout(
                                     sidebarPanel(img(src = "Dracula.png", height = 350, width = 200)),
                                     mainPanel(htmlOutput("dracsum")))
                                 ),
                        tabPanel("Frankenstein", 
                                 sidebarLayout(
                                     sidebarPanel(img(src = "frankenstein.png", height = 350, width = 200)),
                                     mainPanel(htmlOutput("fransum")))
                                 ),
                        tabPanel("Phantom", 
                                 sidebarLayout(
                                     sidebarPanel(img(src = "Phantom.png", height = 350, width = 200)),
                                     mainPanel(htmlOutput("phansum")))
                                 ),
                        tabPanel("Jekyll & Hyde", 
                                 sidebarLayout(
                                     sidebarPanel(img(src = "jekyll.png", height = 350, width = 200)),
                                     mainPanel(htmlOutput("jeksum")))
                        )
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
           h1("Sentiment Analysis"),
           htmlOutput("sentintro"),
           selectInput("title", "Novel",
                       c("Carmilla" = "Carmilla",
                         "Dracula" = "Dracula",
                         "Frankenstein" = "Frankenstein; Or, The Modern Prometheus",
                         "The Phantom of the Opera" = "The Phantom of the Opera",
                         "Jekyll and Hyde" = "The Strange Case of Dr. Jekyll and Mr. Hyde"
                         )),
           plotOutput("sentplot")
        ),
        tabPanel(
            "Word Frequency",
            h1("Word Density"),
            p("The following plot tracks the frequency of the top ten most common words throughout the course of each novel.
              It also maps the use of the pronouns he, she, and it, which have been included alongside the words monster and 
              creature in an effort to locate a correlation between pronoun usage and monstrosity."),
            br(),
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
            ),
            p("I recommend looking at the top ten words separately from the pronouns. 
              To get the arc of what I was trying to investigate with pronouns, please 
              look at he, she, it, and monster across the novels. For Carmilla, I recommend 
              checking she alone, as the monster we are interested in is female.")
        ),
        tabPanel(
            "Regression",
            h1("Linear Regression"),
            htmlOutput("comintro"),
            br(),
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
            ),
            p("I recommend looking at the top ten words separately from the pronouns. 
              To get the arc of what I was trying to investigate with pronouns, please 
              look at he, she, it, and monster across the novels. For Carmilla, I recommend 
              checking she alone, as the monster we are interested in is female.")
        )
    ))


###Server




server <- function(input, output) {
    output$carmsum <- renderUI({
        HTML(
            "<h2><em>Carmilla</em></h2>
<h3>Author: Sheridan Le Fanu</h3>
<p>The work is presented as part of the casenotes of a Doctor Hesselius. The story follows Laura, a pretty but isolated young 
woman who lives with her widowed father in a manner. One day, a woman knocks on their door claiming her carriage has broken down 
and begs admittance into the house for her sickly daughter Carmilla, who cannot travel with her because she needs haste. Laura 
and her father take Carmilla in, and Laura and Carmilla become very close despite the guests odd habits. Carmilla sleeps most of 
the day, does not seem to eat, and is terribly pale. While she resides at their manor, Laura’s health deteriorates and she develops 
vivid nightmares. When a doctor is summoned and a family friend comes to visit, it is revealed that Laura is being prayed on by a 
vampire, none other than the thought to be deceased Countess Mircalla Karnstein. The vampire has gone around and preyed on young 
women in the years since her turning all under anagrams of her own name. Carmilla is discovered during the day lying in her own 
coffin at the Karnstein manor, surrounded with blood. She is swiftly beheaded, and Laura is saved.</p><br>
<p>Le Fanu’s tale is one of the first vampire tales, and predates Stoker’s <em>Dracula</em>. Sadly, this shorter tale is lesser known.</p>
"
        )
    })
    output$dracsum <- renderUI({
        HTML(
            "<h2><em>Dracula</em></h2>
            <h3>Author: Bram Stoker</h3>
            <p>Jonathan Harker goes to Transylvania to help Count Dracula finalize his plans to move to England despite warnings from the 
            native Transylvanians. When Harker realizes he is trapped in the castle and that Dracula himself is a supernatural being, he 
            attempts to escape. Ine England, Harker’s fiancee Mina corresponds with friend Lucy at the same time that a ship wrecks on the 
            coast of England carrying only several coffins of wood from Dracula’s castle and a wolf that runs off into town. Lucy becomes ill 
            and bears the signs of a vampire bite. Van Helsing is summoned to assist, and when Lucy succumbs to death and becomes a vampire, 
            he leads the others to stake her, behead her, and stuff her mouth with garlic. Mina goes to Budapest when Jonathan arrives there 
            half mad. The two return married to try and assist with the Dracula problem. While trying to locate all the coffins of dirt, 
            Dracula preys upon Mina who begins changing. Van Helsing goes back to Transylvania to eradicate Dracula and his castle for good.<p>"
        )
    })
    output$fransum <- renderUI({
        HTML(
            "<h2><em>Frankenstein</em></h2>
            <h3>Author: Mary Shelley</h3>
            <p>Robert Walden is a captain on an arctic exploration when his ship becomes stuck in ice. A half frozen man stumbles upon them and 
            tells Walden his story. Victor Frankenstein lives a charmed life before waltzing off to the University of Ingolstadt to study. After 
            the death of his mother, he becomes fascinated with death, and seeks to overcome it. Using the techniques of long dead alchemists, 
            Frankenstein spends his time at university trying to create life. When he succeeds, he is horrified because the creature he had built 
            to be beautiful is ugly. He faints and runs away. His creature runs out into the unknown. Frankenstein’s best friend comes to visit, 
            and they go home together. There, Frankenstein encounters the creature after his own creation kills his younger brother. The creature 
            demands that Frankenstein builds him a companion. Frankenstein finds and island and a hut to do so in, but in a fit of rage and fear, 
            destroys the bride he is creating. His creature swears vengeance, and when Frankenstein marries his adopted sister Elizabeth, the 
            creature kills her on their wedding night. Frankenstein spends the rest of his life chasing the creature before dying on a ship in 
            the Arctic. The creature carries him off into the ice.<p>"
        )
    })
    output$phansum <- renderUI({
        HTML(
            "<h2><em>The Phantom of the Opera</em></h2>
            <h3>Author: Gaston Leroux</h3>
            <p>New managers buy the Paris Opera House and quickly discover that it is haunted by the Opera Ghost. Christine Daae, a young dancer, 
            shows her prowess in singing when the lead soprano is sick. Childhood lover and Viscount de Chagny, Raoul sees her and falls in love 
            with her all over again, when he tries to get closer to her, he is rebuffed due to the interference of what Christine calls the Angel 
            of Music. Meanwhile opera owners are running into trouble as they keep trying to slight and ignore the phantom. When his box is sold 
            during a show, the phantom brings the chandelier down. Raoul and Christine both discover that the phantom is actually a man who has a 
            face like death living beneath the opera house. The phantom, or Erik, is in love with Christine, and after trapping Raoul in a torture 
            chamber, tells Christine she must either marry the Phantom or sentence Raoul to death. She chooses the Phantom, but Erik sees how much 
            the two love each other and lets them go. Erik, Christine, and Raoul disappear.<p>"
        )
    })
    output$jeksum <- renderUI({
        HTML(
            "<h2><em>The Strange Case of Dr. Jekyll and Mr. Hyde</em></h2>
            <h3>Author: Robert Louis Stevenson</h3>
            <p>John Utterson is perplexed by his friend Dr. Henry Jekyll’s will, which states that if he dies or goes missing, his entire estate
            is to pass to an unknown Mr. Edward Hyde. Utterson goes looking for Hyde and discovers that he is a horrible person despite his manners. 
            Believing his friend is being blackmailed, Utterson tries to figure out what jekyll’s connection to Hyde is. He drops the matter when 
            nothing turns up until Hyde commits a murder. At the same time, Jekyll stops leaving his house or even seeing friends. One night Jekyll’s 
            butler gets Utterson to come over, saying that the man in his master’s room is not Jekyll, but Hyde. They burst into the room to find Hyde 
            dead, no Jekyll, and three documents addressed to Utterson. They recount the story of how Jekyll turned into Hyde in order to express his 
            more deplorable inclinations. Hyde overtook Jekyll in the end, and killed himself to escape the imprisonment that he would have faced when 
            Utterson found him.<p>"
        )
    })
    output$sentintro <- renderUI({
        HTML(
            "<p>The following graphics represent positive over negative sentiment throughout the course of each book. 
            As you can see if you poke around, sentiment in all the texts is overwhelmingly negative. This tracks 
            with what one expects of gothic literature; in stories about death and monstrosity, negative sentiment 
            really ought to be dominant. The question that emerges of this is: are books that have more negative 
            sentiment more monstrous?<p>
            <br>
            <p>Note: The way sentiment is tabulated is by comparing the overall positive versus negative words over roughly 
            every 100 lines. On its own, this does not necessarily mean that the tone of these are overall positive or negative. 
            The arc of the novels would suggest that this is a fairly accurate representation of the course of sentiment<p>"
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
            "The following regressions trace the frequency of the top ten most frequent words across all five novels as well as the pronouns he, she, and it, and the words creature and monster."
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
            geom_density(adjust=1.5, alpha=.4) +
            labs(
                x = "Pages",
                y = "Frequency of Words"
            )
    })
    
    

    
    
    
    
    
}


shinyApp(ui = ui, server = server)

