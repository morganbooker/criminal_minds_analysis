
#### LIBRARIES ####

# Load in necessary libraries

library(tidyverse)
library(gt)
library(moderndive)

#### DATA FRAMES ####

# Read in data frames

cm_words_bau <- read_rds("./objects/cm_words_bau.rds")

cm_words_buzz <- read_rds("./objects/cm_words_buzz.rds")

cm_type <- read_rds("./objects/cm_type.rds")

# Keep only top 10 killer types

cm_type_regression <- cm_type %>% 
  filter(value %in% c("cop killer",
                      "copycat",
                      "family annihilator",
                      "kidnapper",
                      "proxy killer",
                      "robber",
                      "serial killer",
                      "serial rapist",
                      "spree killer",
                      "stalker"))

#### CONVERT CAUGHT TO DUMMY VARIABLE ####

# Modify caught so it is a numeric binary dummy variable and drop NAs. Group
# by season for analysis.

mod_data <- cm_words_bau %>%
  mutate(caught = case_when(
    caught == "yes" ~ "1",
    caught == "no" ~ "0"
  ),
  caught = as.numeric(caught)) %>% 
  drop_na(caught) %>% 
  group_by(season)

mod_data_buzz <- cm_words_buzz %>%
  mutate(caught = case_when(
    caught == "yes" ~ "1",
    caught == "no" ~ "0"
  ),
  caught = as.numeric(caught)) %>% 
  drop_na(caught) %>% 
  group_by(season)

mod_data_type <- cm_type_regression %>% 
  mutate(caught = case_when(
    caught == "yes" ~ "1",
    caught == "no" ~ "0"
  ),
  caught = as.numeric(caught)) %>% 
  drop_na(caught) %>% 
  group_by(season)

#### CHARACTER NAME REGRESSION ####

# Fit a regression model of character names on caught

mod <- lm(caught ~ word, data = mod_data)

# Create a gt table of regression points

mod_table <- mod %>% 
  
  # Use tidy to get regression points
  
  tidy() %>% 
  
  # Rename the terms column to character names
  
  mutate(term = fct_recode(
    term,
    "David Rossi" = "wordDavid Rossi",
    "Derek Morgan" = "wordDerek Morgan",
    "Elle Greenaway" = "wordElle Greenaway",
    "Emily Prentiss" = "wordEmily Prentiss",
    "Jason Gideon" = "wordJason Gideon",
    "Jennifer Jareau" = "wordJennifer Jareau",
    "Penelope Garcia" = "wordPenelope Garcia",
    "Spencer Reid" = "wordSpencer Reid"
  )) %>% 
  
  gt() %>% 
  
  # Hide the test statstic column because won't use it for analysis
  
  cols_hide("statistic") %>% 
  
  # Rename the column labels to make them look prettier 
  
  cols_label(
    term = "Character Name",
    estimate = "Estimate",
    std.error = "Standard Error",
    p.value = "P-Value"
  ) %>% 
  
  # Realign the columns so they look nicer
  
  cols_align(
    align = "left",
    columns = vars(term)
  ) %>% 
  
  cols_align(
    align = "center",
    columns = vars(estimate, std.error, p.value)
  ) %>% 
  
  tab_header(title = "Examining the Relationship between\nCharacter Name 
                      Frequency and Criminal Capture",
             subtitle = "Based on the first five seasons of Criminal Minds") %>% 
  
  # Round all the values to 3 decimal points
  
  fmt_number(columns = vars(estimate, std.error, p.value), decimals = 3) %>% 
  
  # Change the color of the table to make it look better with shiny theme
  
  tab_options(table.background.color = "lightskyblue")

#### BUZZWORD REGRESSION ####

# Fit a regression model of buzzwords on caught

mod_buzz <- lm(caught ~ word, data = mod_data_buzz)

# Create a gt table of regression points

mod_table_buzz <- mod_buzz %>% 
  
  # Use tidy to get regression points
  
  tidy() %>% 
  
  # Rename the terms column to buzzwords
  
  mutate(term = fct_recode(
    term,
    "Arson" = "wordarson",
    "Arsonist" = "wordarsonist",
    "Blood" = "wordblood",
    "Bomber" = "wordbomber",
    "Cannibal" = "wordcannibal",
    "Con" = "wordcon",
    "Copycat" = "wordcopycat",
    "Criminal" = "wordcriminal",
    "Criminals" = "wordcriminals",
    "Hitman" = "wordhitman",
    "Homicidal" = "wordhomicidal",
    "Homicide" = "wordhomicide",
    "Kidnapper" = "wordkidnapper",
    "Kill" = "wordkill",
    "Killer" = "wordkiller",
    "Killers" = "wordkillers",
    "Murder" = "wordmurder",
    "Pedophile" = "wordpedophile",
    "Poison" = "wordpoison",
    "Profile" = "wordprofile",
    "Proxy" = "wordproxy",
    "Psychopath" = "wordpsychopath",
    "Rape" = "wordrape",
    "Rapist" = "wordrapist",
    "Sadism" = "wordsadism",
    "Sadist" = "wordsadist",
    "Serial" = "wordserial",
    "Signature" = "wordsignature",
    "Sociopath" = "wordsociopath",
    "Souvenir" = "wordsouvenir",
    "Spree" = "wordspree",
    "Stalker" = "wordstalker",
    "Suspect" = "wordsuspect",
    "Terrorist" = "wordterrorist",
    "Thrill" = "wordthrill",
    "Trophy" = "wordtrophy",
    "Unsub" = "wordunsub",
    "Victim" = "wordvictim",
  )) %>% 
  
  gt() %>% 
  
  # Hide the test statstic column because won't use it for analysis
  
  cols_hide("statistic") %>% 
  
  # Rename the column labels to make them look prettier 
  
  cols_label(
    term = "Buzzword",
    estimate = "Estimate",
    std.error = "Standard Error",
    p.value = "P-Value"
  ) %>% 
  
  # Realign the columns so they look nicer
  
  cols_align(
    align = "left",
    columns = vars(term)
  ) %>% 
  
  cols_align(
    align = "center",
    columns = vars(estimate, std.error, p.value)
  ) %>% 
  
  tab_header(title = "Examining the Relationship between\nBuzzword 
                      Frequency and Criminal Capture",
             subtitle = "Based on the first five seasons of Criminal Minds") %>% 
  
  # Round all the values to 3 decimal points
  
  fmt_number(columns = vars(estimate, std.error, p.value), decimals = 3) %>% 
  
  # Change the color of the table to make it look better with shiny theme
  
  tab_options(table.background.color = "lightskyblue")

#### CRIMINAL TYPE REGRESSION ####

# Fit a regression model of criminal types on caught

mod_type <- lm(caught ~ value, data = mod_data_type)

# Create a gt table of regression points

mod_table_type <- mod_type %>% 
  
  # Use tidy to get regression points
  
  tidy() %>% 
  
  # Rename the terms column to criminal type names
  
  mutate(term = fct_recode(
    term,
    "Copycat" = "valuecopycat",
    "Family Annihilator" = "valuefamily annihilator",
    "Kidnapper" = "valuekidnapper",
    "Proxy Killer" = "valueproxy killer",
    "Robber" = "valuerobber",
    "Serial Killer" = "valueserial killer",
    "Serial Rapist" = "valueserial rapist",
    "Spree Killer" = "valuespree killer",
    "Stalker" = "valuestalker",
    
  )) %>% 
  
  gt() %>% 
  
  # Hide the test statstic column because won't use it for analysis
  
  cols_hide("statistic") %>% 
  
  # Rename the column labels to make them look prettier 
  
  cols_label(
    term = "Criminal Type",
    estimate = "Estimate",
    std.error = "Standard Error",
    p.value = "P-Value"
  ) %>% 
  
  # Realign the columns so they look nicer
  
  cols_align(
    align = "left",
    columns = vars(term)
  ) %>% 
  
  cols_align(
    align = "center",
    columns = vars(estimate, std.error, p.value)
  ) %>% 
  
  tab_header(title = "Examining the Relationship between\nCriminal Type 
                      Frequency and Criminal Capture",
             subtitle = "Based on the top 10 killer types in the first 
                         five seasons of Criminal Minds") %>% 
  
  # Round all the values to 3 decimal points
  
  fmt_number(columns = vars(estimate, std.error, p.value), decimals = 3) %>% 
  
  # Change the color of the table to make it look better with shiny theme
  
  tab_options(table.background.color = "lightskyblue")

#### WRITE OUT TABLES ####

# Write out objects 

write_rds(mod_table, "./cm_analysis/objects/mod_table.rds")

write_rds(mod_table_buzz, "./cm_analysis/objects/mod_table_buzz.rds")

write_rds(mod_table_type, "./cm_analysis/objects/mod_table_type.rds")

