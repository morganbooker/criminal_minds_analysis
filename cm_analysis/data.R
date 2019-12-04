
# Load in necessary libraries

library(fs)
library(rvest)
library(janitor)
library(tidytext)
library(tidyverse)

# Load in script data from online
# Load in base url for scrapping

base_url <- "https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=criminal-minds&episode="

# Specify the season for the url by noting how many scripts we'll need for each
# season. For example, there are 22 scripts to scrap for season 1 and so on.

s <- c(rep(1, each = 22), 
       rep(2, each = 22),
       rep(3, each = 20), 
       rep(4, each = 26), 
       rep(5, each = 23))

# Add the season code to url

season <- paste0("s0", s)

# Specify the specific episodes to add to the url. Note, that the script for
# season 2 episode 21 is not on the website, so manually skipped that.

ep <- c(rep(1:22, each = 1), 
        rep(1:20, each = 1), 
        rep(22:23, each = 1), 
        rep(1:20, each = 1), 
        rep(1:26, each = 1), 
        rep(1:23, each = 1))

# Add episode code to url

episode <- ifelse(ep < 10, paste0("e0", ep), paste0("e", ep))

# Create place to store scripts

all.scripts <- NULL

# Scrape scripts by pasting together the season/episode elements from above

for (i in 1:113) {
  url <- paste0(base_url, season[i], episode[i])
  webpage <- read_html(url)
  script <- webpage %>% html_node(".scrolling-script-container")
  all.scripts[i] <- html_text(script, trim = TRUE)
}

# Create a tibble to store all the scripts adding the season and episode code to
# the rows

cm <- as.data.frame(all.scripts, stringsAsFactors = FALSE)
counter <- paste0(season, episode)
row.names(cm) <- counter[1:113]
colnames(cm) <- "text"
cm_scripts <- as_tibble(cm)

# Add episode column to script tibble

cm_scripts <- cm_scripts %>% 
  mutate(episode = paste0(season, episode)) %>% 
  select(episode, text)

# Read in csv data and specify column types, filter out the missing episode. Get
# rid of 2x21 since that script is missing. Change episode column format to match
# script tibble. Put episode column first just so it's easier to understand data

cm_data <- read_csv("./cm_analysis/cm_data.csv", col_type = cols(
  ep = col_character(),
  caught = col_character(),
  alive = col_character(),
  personal_story = col_character(),
  intro_quote = col_character(),
  closing_quote = col_character(),
  criminal_gender = col_character(),
  group_size = col_double(),
  number_of_victims = col_double(),
  criminal_type_1 = col_character(),
  criminal_type_2 = col_character(),
  criminal_type_3 = col_character(),
  criminal_type_4 = col_character(),
  criminal_type_5 = col_character()
)) %>% 
  filter(ep != "2x21") %>% 
  mutate(episode = paste0(season, episode)) %>% 
  select(-ep) %>% 
  select(episode, everything())

# Join the two datasets by episode column

cm_joint <- left_join(cm_scripts, cm_data, by = "episode")

# Create list of main character names

bau <- c("jason", "gideon", "aaron", "hotch", "hotchner", 
         "david", "dave", "rossi", "spencer", "spence", 
         "reid", "derek", "morgan", "jennifer", "jj", 
         "jareau", "emily", "prentiss", "penelope", 
         "garcia", "elle", "greenaway")

# Create list of important buzzwords

buzzwords <- c("unsub", "suspect", "murder", "criminal", 
               "blood", "kill", "killer", "serial", "killers", 
               "homicide", "homicidal", "psychopath", "sociopath", 
               "sadist", "sadism", "signature", "trophy", 
               "souvenir", "stalker", "victim", "profile", "arson", 
               "arsonist", "bomber", "pedophile", "poison", "spree", 
               "rapist", "rape", "kidnapper", "abducted", "hitman", 
               "thrill", "con", "proxy", "gangster", "copycat", 
               "terrorist", "cannibal", "criminals")

# Separate each script into individual words, only include pre-selected
# buzzwords. These buzzwords are based off my knowledge of the show.

cm_words <- cm_joint %>% 
  unnest_tokens(word, text) %>% 
  mutate(season =
           case_when(
             str_detect(episode, "s01") ~ "Season 1",
             str_detect(episode, "s02") ~ "Season 2",
             str_detect(episode, "s03") ~ "Season 3",
             str_detect(episode, "s04") ~ "Season 4",
             str_detect(episode, "s05") ~ "Season 5"
           )) %>%
  filter(word %in% c(buzzwords, bau))

# Add season column

cm_season <- cm_data %>% 
  mutate(season =
           case_when(
             str_detect(episode, "s01") ~ "Season 1",
             str_detect(episode, "s02") ~ "Season 2",
             str_detect(episode, "s03") ~ "Season 3",
             str_detect(episode, "s04") ~ "Season 4",
             str_detect(episode, "s05") ~ "Season 5"
           ))

# Write object out to use in shiny app later

write_rds(cm_words, "./cm_analysis/objects/cm_words.rds")

write_rds(cm_season, "./cm_analysis/objects/cm_season.rds")


