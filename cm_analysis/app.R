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
library(RColorBrewer)

rmdfiles <- c("./about.Rmd")
sapply(rmdfiles, knit, quiet = T)

# Read in data from script

cm_words <- read_rds("./cm_words.rds")

cm_name <- read_rds("./cm_name.rds")

cm_name_season <- read_rds("./cm_name_season.rds")

cm_buzz <- read_rds("./cm_buzz.rds")

cm_buzz_season <- read_rds("./cm_buzz_season.rds")

cm_caught <- read_rds("./cm_caught.rds")

cm_caught_season <- read_rds("./cm_caught_season.rds")

# Create column for choices bar

select_season <<- unique(cm_words$season)

getTermMatrix <- memoise(function(season) {
  
  if (!(season %in% select_season))
    stop("Unknown season")
  
  words <- cm_words$word[cm_words$season == season]
  
  myCorpus = Corpus(VectorSource(words))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "that", "he", "she", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
  
})

# general overview of data options

general_options <- c("Character Names",
                     "Buzzwords",
                     "Criminal Caught?")

ui <- navbarPage(
    "Criminal Minds Analysis",
    tabPanel("General Overview",
             fluidPage(
               titlePanel("Overview of Data"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("general_op",
                               "Area of Focus",
                               choices = general_options,
                               selected = general_options[1])
                 ),
                 mainPanel(
                   plotOutput("cm_name"),
                   plotOutput("cm_name_season"),
                   plotOutput("cm_buzz"),
                   plotOutput("cm_buzz_season"),
                   plotOutput("cm_caught"),
                   plotOutput("cm_caught_season")
                 )
               )
             )),
    tabPanel("Catching Criminals",
             fluidPage(
               titlePanel("Caught by Season"),
               p("Work in progress")
             )),
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
    tabPanel("Modeling",
             fluidPage(
               p("Modelesque discussion to be added later as per Preceptor piazza note"),
             )),
    tabPanel("About",
             fluidPage(
                 withMathJax(includeMarkdown("about.md"))
             ))
)

server <- function(input, output) {
  
  output$cm_name <- renderPlot({
    if(input$general_op == "Character Names") {
      cm_name
    }
  })
  
  output$cm_name_season <- renderPlot({
    if(input$general_op == "Character Names") {
      cm_name_season
    }
  })
  
  output$cm_buzz <- renderPlot({
    if(input$general_op == "Buzzwords") {
      cm_buzz
    }
  })
  
  output$cm_buzz_season <- renderPlot({
    if(input$general_op == "Buzzwords") {
      cm_buzz_season
    }
  })
  
  output$cm_caught <- renderPlot({
    if(input$general_op == "Criminal Caught?") {
      cm_caught
    }
  })
  
  output$cm_caught_season <- renderPlot({
    if(input$general_op == "Criminal Caught?") {
      cm_caught_season
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
                  colors = brewer.pal(8, "RdPu"))
  })
  
}

shinyApp(ui, server)

# to do:
# comment all files
# figure out why word cloud is not showing words by season
# maybe make bar plots interactive--hover over and you'll see the count
# figure out why plots move down page in general overview section
# create folder just for rds files since there are a lot