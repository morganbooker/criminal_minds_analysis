
#### LIBRARIES ####

# Load necessary libraries

library(fs)
library(rvest)
library(janitor)
library(tidytext)
library(tidyverse)
library(RColorBrewer)

#### DATA FRAMES ####

# Read in objects

read_rds("./cm_analysis/objects/cm_season.rds")

read_rds("./cm_analysis/objects/cm_words.rds")

# Mutate the words so that all versions of a character's name (first name, last
# name, nickname) all count as one word so it's easier to count the aggregate of
# character name appearance instead of counting each separate part of the name
# individually

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

# Filter words to only look at the buzzwords

cm_words_buzz <- cm_words %>% 
  filter(word %in% buzzwords)

#### CHARACTER NAMES ####

# Create a plot of the frequency of which character names are said in the first
# five seasons of the show, reording by frequency, making sure to flip the
# coordinates so the plot labels are easier to read, adding color to emphasize
# even further the number of times each word is said, and changing the theme of
# the plot because I didn't like how the gray looked

cm_name <- cm_words_bau %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_light() +
  labs(x = NULL,
       y = NULL,
       title = "How many times do they say your name?",
       subtitle = "Based on the first five seasons of Criminal Minds")


# Create a similar plot as above of frequency of character names, but this time,
# group and facet by season as well

cm_name_season <- cm_words_bau %>% 
  group_by(season) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~season) +
  theme_light() +
  labs(x = NULL,
       y = NULL,
       title = "How many times do they say your name each season?")

#### BUZZWORDS ####

# Create a plot of the frequency of which buzzwords are said in the first
# five seasons of the show, reording by frequency, making sure to flip the
# coordinates so the plot labels are easier to read, adding color to emphasize
# even further the number of times each word is said, and changing the theme of
# the plot because I didn't like how the gray looked

cm_buzz <- cm_words_buzz %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_light() +
  labs(x = NULL,
       y = NULL,
       title = "How many times do they this key word?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Create a similar plot as above of frequency of character names, but this time,
# group and facet by season as well

cm_buzz_season <- cm_words_buzz %>% 
  group_by(season) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 15) %>% 
  mutate(word = reorder(word, n)) %>% 
  
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~season) +
  theme_light() +
  labs(x = NULL,
       y = NULL,
       title = "How many times do they say this key word each season?",
       subtitle = "Based on the first five seasons of Criminal Minds")

#### CRIMINAL CAUGHT? ####

# Create plots that show how often criminals are caught across the first five
# seasons by grouping by caught counting and dropping missing variables, hiding
# the legend because it's redundant, and changing the colors and themes to
# something I liked better

cm_caught <- cm_season %>% 
  group_by(caught) %>% 
  count() %>% 
  drop_na() %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = NULL,
       y = "Number of Episodes",
       title = "Number of Times the BAU Caught the Criminal",
       subtitle = "Based on the first five seasons of Criminal Minds.")

# Create plots a similar plot as above, but this time group and facet by season
# as well

cm_caught_season <- cm_season %>% 
  group_by(caught, season) %>% 
  count() %>% 
  drop_na() %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  facet_wrap(~season) +
  labs(x = NULL,
       y = "Number of Episodes",
       title = "Number of Times the BAU Caught the Criminal each Season",
       subtitle = "Based on the first five seasons of Criminal Minds.")

#### CRIMINAL GENDER ####

# Create plots that show how criminal genders across the first five
# seasons by grouping by caught counting and dropping missing variables, hiding
# the legend because it's redundant, and changing the colors and themes to
# something I liked better

cm_gender <- cm_season %>%
  drop_na(criminal_gender) %>% 
  
  ggplot(aes(x = criminal_gender, fill = criminal_gender)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = NULL,
       y = "Number of Episodes",
       title = "Gender Distribution of Criminals in Criminal Minds",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Create plots a similar plot as above, but this time group and facet by season
# as well

cm_gender_season <- cm_season %>%
  drop_na(criminal_gender) %>% 
  group_by(season) %>% 
  
  ggplot(aes(x = criminal_gender, fill = criminal_gender)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", "firebrick2")) +
  theme_light() +
  facet_wrap(~season) +
  labs(x = NULL,
       y = "Number of Episodes",
       title = "Gender Distribution of Criminals in Criminal Minds each Season",
       subtitle = "Based on the first five seasons of Criminal Minds")

#### CRIMINAL TYPE ####

# Combine all the criminal type columns into one and get rid of missing values
# so I can look at all the criminal types that appear instead of having to look
# at the five different categories per episode

cm_type <- cm_season %>% 
  select(episode, season, 
         criminal_type_1, 
         criminal_type_2, 
         criminal_type_3, 
         criminal_type_4, 
         criminal_type_5,
         caught) %>% 
  pivot_longer(c(criminal_type_1, 
                 criminal_type_2, 
                 criminal_type_3, 
                 criminal_type_4, 
                 criminal_type_5)) %>% 
  drop_na()

# Create plots that show how common different criminal types are across the
# first five seasons, reordering by frequency, hiding the legend because it's
# redundant, and changing the colors and themes to something I liked better

cm_crim <- cm_type %>% 
  count(value, sort = TRUE) %>% 
  mutate(value = reorder(value, n)) %>% 
  
  ggplot(aes(x = value, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_light() +
  labs(y = "Criminal Type Count",
       x = NULL,
       title = "Type of Criminals Caught by BAU",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Create a similar plot as above, but this time, group and facet by season as
# well, and only keep criminal types that appear more than once or else the
# plots are too crowded

cm_crim_season <- cm_type %>% 
  group_by(season) %>% 
  count(value, sort = TRUE) %>%
  filter(n > 2) %>% 
  
  ggplot(aes(x = value, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~season) +
  theme_light() +
  labs(x = NULL,
       y = "Criminal Type Count",
       title = "Type of Criminals Caught by BAU each Season",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Only criminal types that appear more than once are included.")

#### WRITE OUT PLOTS ####

# Write out plots

write_rds(cm_name, "./cm_analysis/objects/cm_name.rds")

write_rds(cm_name_season, "./cm_analysis/objects/cm_name_season.rds")

write_rds(cm_buzz, "./cm_analysis/objects/cm_buzz.rds")

write_rds(cm_buzz_season, "./cm_analysis/objects/cm_buzz_season.rds")

write_rds(cm_caught, "./cm_analysis/objects/cm_caught.rds")

write_rds(cm_caught_season, "./cm_analysis/objects/cm_caught_season.rds")

write_rds(cm_gender, "./cm_analysis/objects/cm_gender.rds")

write_rds(cm_gender_season, "./cm_analysis/objects/cm_gender_season.rds")

write_rds(cm_crim, "./cm_analysis/objects/cm_crim.rds")

write_rds(cm_crim_season, "./cm_analysis/objects/cm_crim_season.rds")



