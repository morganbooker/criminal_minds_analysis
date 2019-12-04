
#### LIBARIES ####

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

#### READ IN DATA FROM SCRIPTS ####

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

reid <- read_rds("./objects/reid.rds")

rossi <- read_rds("./objects/rossi.rds")

hotch <- read_rds("./objects/hotch.rds")

jj <- read_rds("./objects/jj.rds")

morgan <- read_rds("./objects/morgan.rds")

prentiss <- read_rds("./objects/prentiss.rds")

garcia <- read_rds("./objects/garcia.rds")

gideon <- read_rds("./objects/gideon.rds")

greenaway <- read_rds("./objects/greenaway.rds")

unsub <- read_rds("./objects/unsub.rds")

kill <- read_rds("./objects/kill.rds")

victim <- read_rds("./objects/victim.rds")

killer <- read_rds("./objects/killer.rds")

profile <- read_rds("./objects/profile.rds")

murder <- read_rds("./objects/murder.rds")

serial <- read_rds("./objects/serial.rds")

blood <- read_rds("./objects/blood.rds")

suspect <- read_rds("./objects/suspect.rds")

criminal <- read_rds("./objects/criminal.rds")

serial_killer <- read_rds("./objects/serial_killer.rds")

kidnapper <- read_rds("./objects/kidnapper.rds")

spree_killer <- read_rds("./objects/spree_killer.rds")

stalker <- read_rds("./objects/stalker.rds")

cop_killer <- read_rds("./objects/cop_killer.rds")

serial_rapist <- read_rds("./objects/serial_rapist.rds")

copycat <- read_rds("./objects/copycat.rds")

robber <- read_rds("./objects/robber.rds")

family_a <- read_rds("./objects/family_a.rds")

proxy_killer <- read_rds("./objects/proxy_killer.rds")

alive_1 <- read_rds("./objects/alive_1.rds")

alive_2 <- read_rds("./objects/alive_2.rds")

alive_3 <- read_rds("./objects/alive_3.rds")

alive_4 <- read_rds("./objects/alive_4.rds")

alive_5 <- read_rds("./objects/alive_5.rds")

#### WORD CLOUD #### 

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

#### INPUT OPTIONS ####

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

character_options <- c("Penelope Garcia",
                       "Jason Gideon",
                       "Elle Greenaway",
                       "Aaron Hotchner",
                       "Jennifer Jareau",
                       "Derek Morgan",
                       "Emily Prentiss",
                       "Spencer Reid",
                       "David Rossi")

buzzword_options <- c("Blood",
                      "Criminal",
                      "Kill",
                      "Killer",
                      "Murder",
                      "Profile",
                      "Serial",
                      "Suspect",
                      "Unsub",
                      "Victim")

criminal_options <- c("Cop Killer",
                      "Copycat Killer",
                      "Family Annihilator",
                      "Kidnapper",
                      "Proxy Killer",
                      "Robber",
                      "Serial Killer",
                      "Serial Rapist",
                      "Spree Killer",
                      "Stalker")

alive_options <- c("Season 1",
                   "Season 2",
                   "Season 3",
                   "Season 4",
                   "Season 5")

#### SHINY APP ####

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
             tabsetPanel(
               tabPanel("Characters",
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Create input selector
                            selectInput("character_op",
                                        "Which Character?",
                                        choices = character_options,
                                        selected = character_options[1]),

                          ),
                          
                          mainPanel(
                            plotOutput("character_plot")
                          )
                        )),
               
               tabPanel("Buzzwords",
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Create input selector
                            selectInput("buzz_op",
                                        "Which Word?",
                                        choices = buzzword_options,
                                        selected = buzzword_options[1]),
                            
                            p("Only the top 10 buzzwords were used.")
                            
                          ),
                          
                          mainPanel(
                            plotOutput("buzz_plot")
                          )
                          
                        )),
               
               tabPanel("Criminal Type",
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Create input selector
                            selectInput("crim_op",
                                        "Which Type of Criminal?",
                                        choices = criminal_options,
                                        selected = criminal_options[1]),
                            
                            p("Only the top 10 criminal types were used.")
                            
                          ),
                          
                          mainPanel(
                            plotOutput("crim_plot")
                          )
                        )),
               tabPanel("Is the Criminal Alive in the End?",
                        sidebarLayout(
                          sidebarPanel(
                            
                            # Create input selector
                            selectInput("alive_op",
                                        "Pick a Season",
                                        choices = alive_options,
                                        selected = alive_options[1]),
                            
                          ),
                          
                          mainPanel(
                            plotOutput("alive_plot")
                          )
                        ))
             ),
             
    ),
    

    
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
                  colors = brewer.pal(8, "RdYlBu"))
  })
  
  output$character_plot <- renderPlot({
    if(input$character_op == "Penelope Garcia") {
      garcia
    }
    else if(input$character_op == "Jason Gideon") {
      gideon
    }
    else if(input$character_op == "Elle Greenaway") {
      greenaway
    }
    else if(input$character_op == "Aaron Hotchner") {
      hotch
    }
    else if(input$character_op == "Jennifer Jareau") {
      jj
    }
    else if(input$character_op == "Derek Morgan") {
      morgan
    }
    else if(input$character_op == "Emily Prentiss") {
      prentiss
    }
    else if(input$character_op == "Spencer Reid") {
      reid
    }
    else if(input$character_op == "David Rossi") {
      rossi
    }
  })
   
  
  output$buzz_plot <- renderPlot({
    if(input$buzz_op == "Unsub") {
      unsub
    }
    else if(input$buzz_op == "Kill") {
      kill
    }
    else if(input$buzz_op == "Victim") {
      victim
    }
    else if(input$buzz_op == "Killer") {
      killer
    }
    else if(input$buzz_op == "Profile") {
      profile
    }
    else if(input$buzz_op == "Murder") {
      murder
    }
    else if(input$buzz_op == "Serial") {
      serial
    }
    else if(input$buzz_op == "Blood") {
      blood
    }
    else if(input$buzz_op == "Suspect") {
      suspect
    }
    else if(input$buzz_op == "Criminal") {
      criminal
    }
  })
  
  output$crim_plot <- renderPlot({
    if(input$crim_op == "Cop Killer") {
      cop_killer
    }
    else if(input$crim_op == "Copycat Killer") {
      copycat
    }
    else if(input$crim_op == "Family Annihilator") {
      family_a
    }
    else if(input$crim_op == "Kidnapper") {
      kidnapper
    }
    else if(input$crim_op == "Proxy Killer") {
      proxy_killer
    }
    else if(input$crim_op == "Robber") {
      robber
    }
    else if(input$crim_op == "Serial Killer") {
      serial_killer
    }
    else if(input$crim_op == "Serial Rapist") {
      serial_rapist
    }
    else if(input$crim_op == "Spree Killer") {
      spree_killer
    }
    else if(input$crim_op == "Stalker") {
      stalker
    }
  })
  
  output$alive_plot <- renderPlot({
    if(input$alive_op == "Season 1") {
      alive_1
    }
    else if(input$alive_op == "Season 2") {
      alive_2
    }
    else if(input$alive_op == "Season 3") {
      alive_3
    }
    else if(input$alive_op == "Season 4") {
      alive_4
    }
    else if(input$alive_op == "Season 5") {
      alive_5
    }
  })

  
}



shinyApp(ui, server)

# to do:
# comment all files
# maybe make bar plots interactive--hover over and you'll see the count
# create folder just for rds files since there are a lot