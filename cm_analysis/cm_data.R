
# Load necessary libraries

library(fs)
library(tm)
library(rvest)
library(janitor)
library(memoise)
library(tidytext)
library(wordcloud)
library(tidyverse)
library(RColorBrewer)

# read in objects

read_rds("./cm_analysis/cm_season.rds")

# have character names/nicknames all count as one word

cm_words_bau <- cm_words %>% 
  filter(word %in% bau) %>% 
  mutate(word = case_when(
    word == "spencer" | word == "spence" | word == "reid" ~ "Spencer Reid",
    word == "derek" | word == "morgan" ~ "Derek Morgan",
    word == "aaron" | word == "hotch" | word == "hotchner" ~ "Aaron Hotchner",
    word == "david" | word == "dave" | word == "rossi" ~ "David Rossi",
    word == "jason" | word == "gideon" ~ "Jason Gideon",
    word == "emily" | word == "prentiss" ~ "Emily Prentiss",
    word == "jennifer" | word == "jj" | word == "jareau" ~ "Jennifer Jareau",
    word == "penelope" | word == "garcia" ~ "Penelope Garcia",
    word == "elle" | word == "greenaway" ~ "Elle Greenaway"
  ))

cm_words_buzz <- cm_words %>% 
  filter(word %in% buzzwords)

# plot of most frequent words

cm_name <- cm_words_bau %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       fill = "Word Count",
       title = "How many times do they say your name?",
       subtitle = "Based on the first five seasons of Criminal Minds")

cm_buzz <- cm_words_buzz %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       fill = "Word Count",
       title = "How many times do they this key word?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# plot of most frequent words by season

cm_name_season <- cm_words_bau %>% 
  group_by(season) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~season) +
  labs(x = NULL,
       y = NULL,
       fill = "Word Count",
       title = "How many times do they say your name each season?")

cm_buzz_season <- cm_words_buzz %>% 
  group_by(season) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 15) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~season) +
  labs(x = NULL,
       y = NULL,
       fill = "Word Count",
       title = "How many times do they say this key word each season?",
       subtitle = "Based on the first five seasons of Criminal Minds")

cm_caught <- cm_season %>% 
  group_by(caught) %>% 
  count() %>% 
  drop_na() %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL,
       y = "Number of Episodes",
       title = "Number of Times the BAU Caught the Criminal",
       subtitle = "Based on the first five seasons of Criminal Minds.")

cm_caught_season <- cm_season %>% 
  group_by(caught, season) %>% 
  count() %>% 
  drop_na() %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  labs(x = NULL,
       y = "Number of Episodes",
       title = "Number of Times the BAU Caught the Criminal each Season",
       subtitle = "Based on the first five seasons of Criminal Minds.")

# Write out plots

write_rds(cm_name, "./cm_analysis/cm_name.rds")

write_rds(cm_name_season, "./cm_analysis/cm_name_season.rds")

write_rds(cm_buzz, "./cm_analysis/cm_buzz.rds")

write_rds(cm_buzz_season, "./cm_analysis/cm_buzz_season.rds")

write_rds(cm_caught, "./cm_analysis/cm_caught.rds")

write_rds(cm_caught_season, "./cm_analysis/cm_caught_season.rds")



