
# Load in necessary libraries

library(fs)
library(tm)
library(rvest)
library(janitor)
library(memoise)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(RColorBrewer)

# Load in script data from online (will comment specifics later)

base_url <- "https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=criminal-minds&episode="
s <- c(rep(1, each = 22), rep(2, each = 22), rep(3, each = 20), rep(4, each = 26), rep(5, each = 23))
season <- paste0("s0", s)
ep <- c(rep(1:22, each = 1), rep(1:20, each = 1), rep(22:23, each = 1), rep(1:20, each = 1), rep(1:26, each = 1), rep(1:23, each = 1))
episode <- ifelse(ep < 10, paste0("e0", ep), paste0("e", ep))
all.scripts <- NULL

#s02e21 missing from website, so had to adjust

for (i in 1:113) {
  url <- paste0(base_url, season[i], episode[i])
  webpage <- read_html(url)
  script <- webpage %>% html_node(".scrolling-script-container")
  all.scripts[i] <- html_text(script, trim = TRUE)
}

# put all scripts into data frame, make a tibble

cm <- as.data.frame(all.scripts, stringsAsFactors = FALSE)
counter <- paste0(season, episode)
row.names(cm) <- counter[1:113]
colnames(cm) <- "text"
cm_scripts <- as_tibble(cm)

# add episode column

cm_scripts <- cm_scripts %>% 
  mutate(episode = paste0(season, episode)) %>% 
  select(episode, text)


# Read in csv data and specify column types, filter out the missing episode
# Get rid of 2x21 since that script is missing
# Change episode column format to match script tibble
# Put episode column first

cm_data <- read_csv("cm_data.csv", col_type = cols(
  episode = col_character(),
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

# Create lists of important buzzwords

bau <- c("jason", "gideon", "aaron", "hotch", "hotchner", 
         "david", "dave", "rossi", "spencer", "spence", 
         "reid", "derek", "morgan", "jennifer", "jj", 
         "jareau", "emily", "prentiss", "penelope", 
         "garcia", "elle", "greenaway")

buzzwords <- c("unsub", "suspect", "murder", "criminal", 
               "blood", "kill", "killer", "serial", "killers", 
               "homicide", "homicidal", "psychopath", "sociopath", 
               "sadist", "sadism", "signature", "trophy", 
               "souvenir", "stalker", "victim", "profile", "arson", 
               "arsonist", "bomber", "pedophile", "poison", "spree", 
               "rapist", "rape", "kidnapper", "abducted", "hitman", 
               "thrill", "con", "proxy", "gangster", "copycat", 
               "terrorist", "cannibal", "criminals")

# Separate the words, only include pre-selected buzzwords
# Add season column

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

write_rds(cm_words, "cm_analysis/cm_words.rds")

write_rds(cm_season, "cm_analysis/cm_season.rds")


