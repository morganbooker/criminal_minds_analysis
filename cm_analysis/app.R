
# Load in necessary libraries

library(shiny)
library(knitr)
library(fs)
library(tm)
library(rvest)
library(janitor)
library(memoise)
library(tidytext)
library(markdown)
library(wordcloud)
library(tidyverse)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)


# Read in data from scripts

cm_words <- read_rds("./objects/cm_words.rds")

cm_name <- read_rds("./objects/cm_name.rds")

cm_name_season <- read_rds("./objects/cm_name_season.rds")

cm_buzz <- read_rds("./objects/cm_buzz.rds")

cm_buzz_season <- read_rds("./objects/cm_buzz_season.rds")

cm_caught <- read_rds("./objects/cm_caught.rds")

cm_caught_season <- read_rds("./objects/cm_caught_season.rds")

cm_gender <- read_rds("./objects/cm_gender.rds")

cm_gender_season <- read_rds("./objects/cm_gender_season.rds")

cm_crim <- read_rds("./objects/cm_crim.rds")

cm_crim_season <- read_rds("./objects/cm_crim_season.rds")

# Create word cloud (based on shiny website tutorial) and create column for word
# cloud choices bar

select_season <<- unique(cm_words$season)

getTermMatrix <- memoise(function(season) {
  
  # Specify that wordcloud should facet by season
  
  if (!(season %in% select_season))
    stop("Unknown season")
  
  # Specify that words should appear based on their frequency per season
  
  words <- cm_words$word[cm_words$season == season]
  
  # Focus on specified words and remove any punctuation, numbers, and basic
  # words like "that, "and", etc.
  
  myCorpus = Corpus(VectorSource(words))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "that", 
                      "he", "she", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  
})


# Create options for input selector for General Overview tab

general_options <- c("Character Names",
                     "Buzzwords",
                     "Criminal Caught?",
                     "Criminal Gender",
                     "Criminal Types")

general_options_season <- c("Character Names",
                     "Buzzwords",
                     "Criminal Caught?",
                     "Criminal Gender",
                     "Criminal Types")

ui <- navbarPage(
    "Criminal Minds Analysis",
    
    theme = shinytheme("cyborg"),
    
    tabPanel("General Overview",
             tabsetPanel(
               tabPanel("All Seasons",
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Create input selector
                            
                            selectInput("general_op",
                                        "Area of Focus",
                                        choices = general_options,
                                        selected = general_options[1])
                          ),
                          
                          mainPanel(
                            plotOutput("cm_overview")
                          )
                        )),
               tabPanel("By Season",
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Create input selector
                            
                            selectInput("general_op_season",
                                        "Area of Focus",
                                        choices = general_options_season,
                                        selected = general_options_season[1])
                          ),
                          
                          mainPanel(
                            plotOutput("cm_overview_season")
                          )
                        ))
             ),
             ),
    
    tabPanel("Word Cloud",
             fluidPage(
               titlePanel("Word Cloud"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selection", 
                               "Season:",
                               choices = select_season,
                               selected = select_season[1]),
                   actionButton("update", "Click Me!"),
                   hr(),
                   sliderInput("freq",
                               "Minimum Word Count:",
                               min = 1, max = 800, value = 500),
                   sliderInput("max",
                               "Maximum Number of Words:",
                               min = 1, max = 61, value = 30)
                 ),
                 mainPanel(
                   plotOutput("plot")
                 )
               )
             )),
    
    
    tabPanel("Criminal Analysis",
             fluidPage(
               titlePanel("Caught by Season"),
               p("Work in progress")
             )),
    
    tabPanel("Future Models Discussion",
             fluidPage(
               p("A way to further this project would be to build a 
                  model that predicts if the criminal will be caught 
                  based on the frequency of certain words. Building 
                  that sort of model is beyond the scope of this course."),
             )),
    tabPanel("About",
             fluidPage(
               withMathJax(includeMarkdown("about.Rmd"))
             ))
)

server <- function(input, output) {
  
  output$cm_overview <- renderPlot({
    if(input$general_op == "Character Names") {
      cm_name
    }
    else if(input$general_op == "Buzzwords") {
      cm_buzz
    }
    else if(input$general_op == "Criminal Caught?") {
      cm_caught
    }
    else if(input$general_op == "Criminal Gender") {
      cm_gender
    }
    else if(input$general_op == "Criminal Types") {
      cm_crim
    }
  })
  
  output$cm_overview_season <- renderPlot({
    if(input$general_op_season == "Character Names") {
      cm_name_season
    }
    else if(input$general_op_season == "Buzzwords") {
      cm_buzz_season
    }
    else if(input$general_op_season == "Criminal Caught?") {
      cm_caught_season
    }
    else if(input$general_op_season == "Criminal Gender") {
      cm_gender_season
    }
    else if(input$general_op_season == "Criminal Types") {
      cm_crim_season
    }
  })

  
  terms <- reactive({
    input$update
    
    isolate({
      withProgress({
        setProgress(message = "Buzzzz...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors = brewer.pal(8, "Spectral"))
  })
   
}



shinyApp(ui, server)

# to do:
# comment all files
# maybe make bar plots interactive--hover over and you'll see the count
# create folder just for rds files since there are a lot