
#### LIBARIES ####

# Load in necessary libraries

library(shiny)
library(knitr)
library(fs)
library(tm)
library(gt)
library(rvest)
library(janitor)
library(memoise)
library(vembedr)
library(tidytext)
library(markdown)
library(wordcloud)
library(tidyverse)
library(moderndive)
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

mod_table <- read_rds("./objects/mod_table.rds")

mod_table_buzz <- read_rds("./objects/mod_table_buzz.rds")

mod_table_type <- read_rds("./objects/mod_table_type.rds")

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

regression_options <- c("Characters",
                        "Buzzwords",
                        "Criminal Type")

#### UI ####

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
                            
                            # Place plot output here
                            
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
                            
                            # Place plot output here
                            
                            plotOutput("cm_overview_season"),
                            
                            br(),
                            
                            # Place disclaimer text here
                            
                            textOutput("cm_text_season")
                            
                          )
                          
                        ))
               
             ),
             
             ),
    
    tabPanel("Word Cloud",
             
             fluidPage(
               
               titlePanel("Word Cloud"),
               
               sidebarLayout(
                 
                 sidebarPanel(
                   
                  # Create input selector
                   
                   selectInput("selection", 
                               "Season:",
                               choices = select_season,
                               selected = select_season[1]),
                   
                   # Create action button to switch between seasons
                   
                   actionButton("update", "Click Me!"),
                   
                   hr(),
                   
                   # Create minimum word count with the maximum being 800 since
                   # that's the highest frequency of any of the chosen words. I
                   # chose an initial starting value of 500 because it gives a
                   # nice amount of words in the cloud
                   
                   sliderInput("freq",
                               "Minimum Word Count:",
                               min = 1, max = 800, value = 500),
                   
                   # Create a maximum number of words with the max being 61
                   # since that's the total number of buzzwords and character
                   # names. I chose an initial starting value of 30 because it
                   # puts a nice amount of words in the cloud
                   
                   sliderInput("max",
                               "Maximum Number of Words:",
                               min = 1, max = 61, value = 30),
                   
                   p("Note: Only character names and buzzwords are included in the
                      word cloud.")
                   
                 ),
                 
                 mainPanel(
                   
                   # Place wordcloud here
                   
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
                            
                            # Place plot output here
                            
                            plotOutput("character_plot"),
                            
                            br(), 
                            
                            # Place plot description here
                            
                            textOutput("character_text")
                            
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
                            
                            # Place plot output here
                            
                            plotOutput("buzz_plot"), 
                            
                            br(),
                            
                            # Place plot descriptions here
                            
                            textOutput("buzz_text")
                            
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
                            
                            # Place plot output here
                            
                            plotOutput("crim_plot"),
                            
                            br(), 
                            
                            # Place plot description here
                            
                            textOutput("crim_text")
                            
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
                            
                            # Place plot output here
                            
                            plotOutput("alive_plot"),
                            
                            br(),
                            
                            # Place plot clarification here
                            
                            p("For clarification, criminals are classified as 
                              'both' (i.e. alive and dead) if they were in 
                              group and some members lived but others died."),
                            
                            br(),
                            
                            p("Criminals were classified as 'unknown' if it 
                               was unclear at the end of the episode whether 
                               they were alive or dead; this usually only 
                               happened with episodes that ended on vague
                               cliffhangers."),
                            
                            br(),
                            
                            p("Criminals were classified as caught and dead 
                               if the team caught the criminal, but the 
                               criminal died afterwards. This often occurred 
                               when the criminal managed to commit suicide 
                               in custody or was killed in custody.")
                            
                          )
                          
                        ))
               
             ),
             
    ),
    
    tabPanel("Regression",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 # Create input selector
                 
                 selectInput("regression_op",
                             "Area of Focus",
                             choices = regression_options,
                             selected = regression_options[1]),
                 
               ),
               
               mainPanel(
                 
                 # Display regression table output
                 
                 gt_output("regression_table"),
                 
                 br(),
                 
                 # Display table descriptions
                 
                 textOutput("regression_text")
                 
               )
               
             )
      
    ),
    
    tabPanel("About",
             
             fluidPage(
               
               # Place image above about page content
               
               imageOutput("logo", width = "100%", height = "100%"),
               
               br(),
               
               # Embed youtube video and center it
               
               h3("Recap Video"),
               
               embed_url("https://www.youtube.com/watch?v=7PyCC9Yzn0k&feature=youtu.be") %>% 
                 div(align = "center"),
               
               # This allows me to include my Rmd about page on my shiny app
               
               withMathJax(includeMarkdown("about.Rmd")),
               
               # Place image below about page content
               
               imageOutput("cast", width = "100%", height = "100%")
               
             ))
    
)

#### SERVER ####

server <- function(input, output) {
  
  # Create conditions to display the general overview plots where different
  # plots are shown depending on what the input is
  
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
  
  # Create conditions to display the general overview plots where different
  # plots are shown depending on what the input is
  
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
  
  # Display disclaimer
  
  output$cm_text_season <- renderText({
    
    if(input$general_op_season == "Character Names") {
      "Disclaimer: In Seasons 1 and 2, there are suspects named Emily and
       David, which is why these character names show up in the count before
       the characters ever appear on the show. On the show, the writers 
       generally refrain from having suspects with the same names as the 
       characters, so once Emily Prentiss and David Rossi became series 
       regulars in Seasons 2 and 3 respectively, the chart accurately 
       reflects the frequency of their names being spoken."
    }
    else if(input$general_op_season == "Buzzwords") {
      "Buzzwords were only kept in this chart if they were said more than
       15 times in at least 1 of the 5 seasons."
    }
    else if(input$general_op_season == "Criminal Gender") {
      "If a criminal is classified as 'both', this means that there were
       at least two criminals, one of whom was female, one of whom was male."
    }
    else if(input$general_op_season == "Criminal Types") {
      "Criminal types were only kept in this chart if there were more than
       2 of that type of criminal in at least 1 of the 5 seasons."
    }
    
  })

  # Create the wordcloud ouput here and make it interactive
  
  terms <- reactive({
    
    input$update
    
    # Set up the loading section, putting my own custom message as the wordcloud
    # loads up on the page, and set up the selectors
    
    isolate({
      
      withProgress({
        
        setProgress(message = "Buzzzz...")
        
        getTermMatrix(input$selection)
        
      })
      
    })
    
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  # Render the wordcloud plot
  
  output$plot <- renderPlot({
    
    v <- terms()
    
    wordcloud_rep(names(v), 
                  v, 
                  scale = c(4,0.5),
                  min.freq = input$freq, 
                  max.words = input$max,
                  colors = brewer.pal(8, "RdYlBu"))
  })
  
  # Create conditions to display the character name plots where different
  # plots are shown depending on what the input is
  
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
  
  # Create conditions to describe character plots
  
  output$character_text <- renderText({
    
    if(input$character_op == "Penelope Garcia") {
      "Penelope Garcia is the BAU's Technical Analyst who assists the BAU
       by using her hacking and computer skills to obtain information about
       potential suspects including arrest records, family history, and
       current addresses among other things. If her name is rarely said in
       an episode, this suggests that the team were not taking advantage of
       her ability to provide them with helpful information about the suspects,
       and perhaps that would contribute to them not being able to catch the
       criminal."
    }
    else if(input$character_op == "Jason Gideon") {
      "Senior Supervisory Special Agent Jason Gideon was the BAU's best
       criminal profiler in his time on the show. He retired in the first
       episode of Season 3, but was still mentioned by the team after his
       retirement. In episodes where Gideon's name is said more than 15
       times, the criminal always evades capture, suggesting that when
       we hear his name at very high frequencies, we should expect the
       criminal to go free. When his name is said less than 15 times,
       however, there is more variability in whether or not the criminal
       is captured."
    }
    else if(input$character_op == "Elle Greenaway") {
      "Supervisory Special Agent Elle Greenaway was an agent with in the BAU
       who specialized in sexual offense crimes. She left the BAU when she
       started developing symptoms of PTSD after being shot by an unsub in the
       middle of Season 2, and was never mentioned again by the team after
       Season 3. There does not appear to be a consistent pattern that emerges
       for the frequency of her name and criminal capture."
    }
    else if(input$character_op == "Aaron Hotchner") {
      "Supervisory Special Agent Aaron 'Hotch' Hotchner is the BAU's Unit 
       Chief, the leader of the team. He is very serious and focused on
       leading the team efficiently and effectively, but is also the character
       who's personal life with his son and wife is most prominently featured
       on the show. In Season 5, he becomes the obsession of a serial killer, 
       'The Reaper', who eventually goes on to murder Hotch's wife in the middle
       of the season, leading to Hotch's name being said more often in Season 5 
       episodes. Whenever Hotch's name is said more than 20 times in an 
       episode, the criminal is always caught, suggesting that the more his
       name is said, the more likely we will see the criminal caught in the
       end. It is also worth noting that his name is the one said most
       frequently across all five seasons."
    }
    else if(input$character_op == "Jennifer Jareau") {
      "Jennifer 'JJ' Jareau is the BAU's Communications Liaison who serves as
       the team's point of contact with the police and media officials. Her
       name is often said a lot as she is the individual responsible for
       picking the cases the team pursues, introducing the team to local police,
       and informing media strategies for the case. JJ appears in less episodes 
       in Season 4 due to the actress' maternity leave. There does not appear
       to be a consistent pattern for frequency of JJ's name and criminal
       capture across all 5 seasons; however, in Seasons 4 and 5, every time
       her name is said more than 7 times, the criminal is caught."
    }
    else if(input$character_op == "Derek Morgan") {
      "Supervisory Special Agent Derek Morgan is a BAU agent who specializes in
       explosives, fixations, and obsessive behaviors. His name is said over 60
       times in a Season 2 episode where he is falsely accused of a murder in
       his hometown. There does not really appear to be a consistent pattern
       for frequency of Morgan's name and criminal capture; however, the
       outlier episode from Season 2 really skews the overall distribution."
    }
    else if(input$character_op == "Emily Prentiss") {
      "Supervisory Special Agent Emily Prentiss is a BAU agent who replaced
       Elle Greenaway in Season 2. She is fluent in multiple languages, which
       makes her an important assest in cases where suspects or witnesses are
       not fluent in English. When her name is said more than 12 times in an
       episode, the criminal tends to be caught in the end."
    }
    else if(input$character_op == "Spencer Reid") {
      "Dr. Spencer Reid is a Supervisory Special Agent in the BAU and is a 
       certified genius with an eidetic memory. Reid is the youngest member
       of the team and is consider to be the most popular character on the
       show. He has two major multiple-episode story arcs that lead to an
       increase of the frequency of his name. In Season 2, he was kidnapped
       and tortured by an unsub and in Season 4, there were multiple
       episodes dedicated to a crime he witnessed as a child. Except for
       one episode in Season 2, in general, whenever Reid's name is said
       more than 10 times in an episode, the criminal is caught, suggesting
       that the more often we hear his name, the more likely we will see
       the criminal get caught."
    }
    else if(input$character_op == "David Rossi") {
      "Senior Supervisory Special Agent David Rossi is one of the founders of
       the BAU. He retired early from the FBI in 1997, but left retirement and
       rejoined the team after Jason Gideon left the BAU in Season 3. When
       Rossi's name is said over 12 times in an episode, the criminal is
       always caught, suggesting that the more often we hear his name, the
       more likely the criminal will be caught."
    }
    
  })
   
  # Create conditions to display the buzzword plots where different
  # plots are shown depending on what the input is
  
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
  
  # Create conditions to describe the buzzword plots
  
  output$buzz_text <- renderText({
    
    if(input$buzz_op == "Unsub") {
      "'Unsub' is the most frequently said buzzword across all 5 seasons.
       Whenever it is said more than 15 times in an episode, the criminal
       is always caught. However, it's worth noting that this word tends
       to have more within-season variability than most other buzzwords."
    }
    else if(input$buzz_op == "Kill") {
      "There is a considerable amount of variation in the frequency of 'kill'
       being said in an episode and criminal capture. There does not appear
       to be any consistent pattern here."
    }
    else if(input$buzz_op == "Victim") {
      "There is a considerable amount of variaibility in the frequency of
       'victim' and criminal capture within each season. There is no clear
       pattern here except for in Season 5 where the more often 'victim' is
       said, the more often the criminal is caught."
    }
    else if(input$buzz_op == "Killer") {
      "Except for one episode in Season 1, whenever 'killer' is said more than
       7 times in an episode, the criminal is always caught."
    }
    else if(input$buzz_op == "Profile") {
      "There does not appear to be a consistent pattern across all 5 seasons.
       However, in Season 3, the more often 'profile' is said, the more likely
       the criminal evades capture, which is an interesting find."
    }
    else if(input$buzz_op == "Murder") {
      "There does not appear to be a consistent pattern across all 5 seasons;
       however, in Seasons 2, 4, and 5, when 'murder' is said more than 3
       times, the criminal is always caught."
    }
    else if(input$buzz_op == "Serial") {
      "In Seasons 1 and 2, the more often 'serial' is said and in the episode
       with the highest frequency of the word, the criminal evades capture.
       Interestingly, we see the exact opposite relationship in Seasons 3,
       4, and 5."
    }
    else if(input$buzz_op == "Blood") {
      "Whenever 'blood' is said more than 7 times in an episode, the criminal
       is always caught."
    }
    else if(input$buzz_op == "Suspect") {
      "Whenever 'suspect' is said more than 5 times in an episode, the criminal
       is caught. However, there is very little variability in Seasons 3 and 
       5."
    }
    else if(input$buzz_op == "Criminal") {
      "Whenever 'criminal' is said more than 4 times in an episode, the criminal
       is always caught."
    }
    
  })
  
  # Create conditions to display the criminal type plots where different
  # plots are shown depending on what the input is
  
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
  
  # Create conditions to display the criminal type plot descriptions
  
  output$crim_text <- renderText({
    
    if(input$crim_op == "Cop Killer") {
      "In general, it appears that cop killers are more likely to be caught
       than not, with no cop killers escaping in Season 2 or 3."
    }
    else if(input$crim_op == "Copycat Killer") {
      "In Season 1, there was only one copycat killer, and they evaded capture. For
       all other seasons, copycat killers are more likely to be caught
       than evade capture."
    }
    else if(input$crim_op == "Family Annihilator") {
      "In general, family annihilators are more likely to be caught than evade
       capture; however in Season 2, there was only one family annihilator, 
       and they evaded capture."
    }
    else if(input$crim_op == "Kidnapper") {
      "In general, kidnappers are more likely to be caught than evade capture.
       The chance of being caught seems particulary high in Seasons 3, 4, and 5
       whereas with Season 2, there was an almost 50-50 shot of being caught
       or evading capture."
    }
    else if(input$crim_op == "Proxy Killer") {
      "In Seasons 2, 3, and 5, proxy killers are more likely to be caught than
       evade capture. In Season 1, there was only one proxy killer, and they 
       evaded capture. In Season 4, there were no proxy killers."
    }
    
    else if(input$crim_op == "Robber") {
      "Robbers are more likely to be caught in Seasons 1 and 3. There appears
       to be a 50-50 shot of being caught in Season 5, and an almost 50-50 shot
       of being caught in Season 4. Seasons 2 did not have any robbers."
    }
    else if(input$crim_op == "Serial Killer") {
      "Serial killers are more likely to be caught than evade capture with
       Season 4 showing the most extreme version of this trend with the BAU
       catching over 15 serial killers, only letting 2 evade capture."
    }
    else if(input$crim_op == "Serial Rapist") {
      "In Seasons 1-4, serial rapists are more likely to be caught than not;
       however, in Season 5, there was a 50-50 chance of being caught or
       evading capture."
    }
    else if(input$crim_op == "Spree Killer") {
      "Spree killers are more likely to be caught than evade capture across
       all 5 seasons. Season 5 shows the most extreme version of this
       trend with the BAU capturing 10 spree killers, only letting 1 evade
       capture."
    }
    else if(input$crim_op == "Stalker") {
      "In all seasons but Season 2, stalkers are more likely to be caught
       than evade capture. Interestingly enough, in Season 2, stalkers are
       more likely to evade capture than be caught, which is the only time
       we see this trend across all of the top 10 criminal types in a season
       where there were criminals of this type to be caught and evade capture."
    }
    
  })
  
  # Create conditions to display the alive vs. dead plots where different
  # plots are shown depending on what the input is
  
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
  
  # Create conditions to display gt table output
  
  output$regression_table <- render_gt({
    
    if(input$regression_op == "Characters") {
      mod_table
    }
    else if(input$regression_op == "Buzzwords") {
      mod_table_buzz
    }
    else if(input$regression_op == "Criminal Type") {
      mod_table_type
    }

  })
  
  output$regression_text <- renderText({
    
    if(input$regression_op == "Characters") {
      "In this regression table, all character names are being compared with 
      Aaron Hotchner, whose name represents the reference group. A positive 
      regression coefficient means that with every additional time the 
      character’s name is said, the criminal is more likely to be 
      caught compared to when Hotch’s name is said. A negative coefficient 
      means that with every additional time the character’s name is said, the
      criminal is less likely to be caught compared to when Hotch’s 
      name is said. Here we see that when Rossi, Prentiss, Garcia, or Reid’s 
      names are said, the criminal is more likely to be caught compared to 
      when Hotch’s name is said whereas the opposite can be said about when 
      Morgan, Greenaway, Gideon, or JJ’s names are said. However, it is 
      important to note that the only statistically significant relationships 
      here are for Rossi, Greenaway, and Gideon’s names."
    }
    else if(input$regression_op == "Buzzwords") {
      "In this regression table, all buzzwords are being compared to the word 
      ‘abducted’, which represents the reference group. A positive regression 
      coefficient means that with every additional time the word is said, the
      criminal is more likely to be caught compared to when 
      ‘abducted’ is said. A negative coefficient means that with every 
      additional time the word is said, the criminal is less likely 
      to be caught compared to when ‘abducted’ is said. Here we say that 
      ‘cannibal’, ‘con’, ‘homicidal’, ‘kidnapper’, ‘pedophile’, ‘poison’, 
      ‘proxy’, ‘sadism’, ‘souvenir’, ‘stalker’, ‘thrill’, ‘trophy’, and ‘unsub’
      have positive coefficients, meaning when these words are said more the 
      criminal is more likely to be caught whereas the opposite can be said 
      about the other buzzwords since they have negative regression 
      coefficients. However, it is important to note that the only 
      statistically significant relationships here are for the words ‘bomber’,
      ‘copycat’, ‘criminals’, ‘hitman’, ‘homicide’, and ‘terrorist’."
    }
    else if(input$regression_op == "Criminal Type") {
      "This regression table examines the relationship between criminal capture
      and the type of killers, focusing only on the top 10 killer types. All of
      the killer types in the table are being compared to cop killers, who 
      serve as the reference group. A positive regression coefficient means 
      that a criminal in that category is more likely to be caught 
      than a cop killer whereas a negative coefficient means that a criminal in
      that category is less likely to be caught than a cop killer. 
      Only family annihilators and spree killers have positive coefficients, 
      meaning that criminals of these types are more likely to be caught than 
      cop killers whereas the opposite is true for the rest of the listed 
      killers. Interestingly enough, none of these relationships are 
      statistically significant, so based on this regression we don’t have
      sufficient evidence to believe that criminal type influences the 
      likelihood a criminal will be caught."
    }
    
  })
  
  # Display images in about page
  
  output$logo <- renderImage({
    
    list(src = "./images/logo_transp.png",
         height = 200,
         width = 500,
         style = "display: block; margin-left: auto; margin-right: auto;")},
    deleteFile = FALSE
  )
  
  output$cast <- renderImage({
    
    list(src = "./images/all_cast.jpg",
         height = 300,
         width = 600,
         style = "display: block; margin-left: auto; margin-right: auto;")},
    deleteFile = FALSE
  )

  
}


# Render shiny app

shinyApp(ui, server)
