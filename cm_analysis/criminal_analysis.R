
#### LIBRARIES ####

# Load in necessary libraries

library(fs)
library(tidyverse)

#### DATA FRAMES #####

# Load in data frames

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

##### CHARACTERS #####

# All plots are made in a similar way. First, I group by season and filter out
# the specific character name. Then, I group by season, episode, and whether the
# episode was caught and count these values, dropping missing caught data so
# that I can have the right type of grouping for the plot. Next, I create a
# ggplot where episode is the grouping variable, I then dodge episodes so that
# each episode appears as its own bar on the plot. I then facet by season so we
# can see all episodes in a season, then used my own color scale because I
# thought it looked nicer than the default and I changed the theme because I
# didn't like how the default gray background looked.

# Spencer Reid

reid <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Spencer Reid") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Spencer Reid?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Aaron Hotchner

hotch <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Aaron Hotchner") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Aaron Hotchner?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# David Rossi

rossi <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "David Rossi") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying David Rossi?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Derek Morgan

morgan <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Derek Morgan") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Derek Morgan?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Emily Prentiss

prentiss <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Emily Prentiss") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Emily Prentiss?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Jason Gideon

gideon <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Jason Gideon") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Jason Gideon?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Jennifer Jareau

jj <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Jennifer Jareau") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Jennifer Jareau?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Penelope Garcia

garcia <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Penelope Garcia") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Penelope Garcia?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Elle Greenaway

greenaway <- cm_words_bau %>% 
  group_by(season) %>% 
  filter(word == "Elle Greenaway") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying Elle Greenaway?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Write out the objects

write_rds(reid, "./cm_analysis/objects/reid.rds")

write_rds(hotch, "./cm_analysis/objects/hotch.rds")

write_rds(rossi, "./cm_analysis/objects/rossi.rds")

write_rds(morgan, "./cm_analysis/objects/morgan.rds")

write_rds(prentiss, "./cm_analysis/objects/prentiss.rds")

write_rds(gideon, "./cm_analysis/objects/gideon.rds")

write_rds(jj, "./cm_analysis/objects/jj.rds")

write_rds(garcia, "./cm_analysis/objects/garcia.rds")

write_rds(greenaway, "./cm_analysis/objects/greenaway.rds")

##### BUZZWORDS #####

# All plots are made in a similar way. First, I group by season and filter out
# the specific buzzword. Then, I group by season, episode, and whether the
# episode was caught and count these values, dropping missing caught data so
# that I can have the right type of grouping for the plot. Next, I create a
# ggplot where episode is the grouping variable, I then dodge episodes so that
# each episode appears as its own bar on the plot. I then facet by season so we
# can see all episodes in a season, then used my own color scale because I
# thought it looked nicer than the default and I changed the theme because I
# didn't like how the default gray background looked.

# Only the Top 10 Buzzwords from the general overview were used for simplicity
# reasons.

# Unsub

unsub <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "unsub") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying unsub?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Kill

kill <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "kill") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying kill?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Victim

victim <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "victim") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying victim?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

## Killer

killer <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "killer") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying killer?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Profile

profile <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "profile") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying profile?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Murder

murder <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "murder") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying murder?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Serial

serial <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "serial") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying serial?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Blood

blood <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "blood") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying blood?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Suspect

suspect <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "suspect") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying suspect?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Criminal

criminal <- cm_words_buzz %>% 
  group_by(season) %>% 
  filter(word == "criminal") %>% 
  group_by(season, episode, caught) %>% 
  count() %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, group = episode, fill = caught)) +
  geom_col(position = "dodge", color = "white", show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Times Said in Episode",
       title = "Is there a relationship between the criminal being caught and saying criminal?",
       subtitle = "Based on the first five seasons of Criminal Minds",
       caption = "Note: each bar represents a different episode")

# Write out objects

write_rds(unsub, "./cm_analysis/objects/unsub.rds")

write_rds(kill, "./cm_analysis/objects/kill.rds")

write_rds(victim, "./cm_analysis/objects/victim.rds")

write_rds(killer, "./cm_analysis/objects/killer.rds")

write_rds(profile, "./cm_analysis/objects/profile.rds")

write_rds(murder, "./cm_analysis/objects/murder.rds")

write_rds(serial, "./cm_analysis/objects/serial.rds")

write_rds(blood, "./cm_analysis/objects/blood.rds")

write_rds(suspect, "./cm_analysis/objects/suspect.rds")

write_rds(criminal, "./cm_analysis/objects/criminal.rds")

##### CRIMINAL TYPE #####

# All plots are made in a similar way. First, I group by season and filter out
# the specific criminal type. Then, I group by season and whether the episode
# was caught and count these values, dropping missing caught data so that I can
# have the right type of grouping for the plot. I don't need to group by episode
# like I did previously because (except on rare occasions) there is usually only
# one killer per episode. Next, I create a ggplot and facet by season, then I
# used my own color scale because I thought it looked nicer than the default and
# I changed the theme because I didn't like how the default gray background
# looked.

# Only the top 10 killer types from the general overview section were used for
# simplicity reasons

# Serial Killer

serial_killer <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "serial killer") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Serial Killers",
       title = "Is there a relationship between being a serial killer and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Kidnapper

kidnapper <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "kidnapper") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Kidnappers",
       title = "Is there a relationship between being a kidnapper and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Spree Killer

spree_killer <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "spree killer") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Spree Killers",
       title = "Is there a relationship between being a spree killer and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Stalker

stalker <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "stalker") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Stalkers",
       title = "Is there a relationship between being a stalker and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Cop Killer

cop_killer <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "cop killer") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Cop Killers",
       title = "Is there a relationship between being a cop killer and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Serial Rapist

serial_rapist <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "serial rapist") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Serial Rapists",
       title = "Is there a relationship between being a serial rapist and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Copycat

copycat <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "copycat") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Copycats",
       title = "Is there a relationship between being a copycat killer and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Robber

robber <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "robber") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Robbers",
       title = "Is there a relationship between being a robber and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Family Annihilator

family_a <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "family annihilator") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Serial Killers",
       title = "Is there a relationship between being a family annihilator and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Proxy Killer

proxy_killer <- cm_type %>% 
  group_by(season) %>% 
  filter(value == "proxy killer") %>% 
  count(value, caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = caught)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season) +
  scale_fill_manual(values = c("dodgerblue", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = "Number of Proxy Killers",
       title = "Is there a relationship between being a proxy killer and being caught?",
       subtitle = "Based on the first five seasons of Criminal Minds")

# Write out objects

write_rds(serial_killer, "./cm_analysis/objects/serial_killer.rds")

write_rds(kidnapper, "./cm_analysis/objects/kidnapper.rds")

write_rds(spree_killer, "./cm_analysis/objects/spree_killer.rds")

write_rds(stalker, "./cm_analysis/objects/stalker.rds")

write_rds(cop_killer, "./cm_analysis/objects/cop_killer.rds")

write_rds(serial_rapist, "./cm_analysis/objects/serial_rapist.rds")

write_rds(copycat, "./cm_analysis/objects/copycat.rds")

write_rds(robber, "./cm_analysis/objects/robber.rds")

write_rds(family_a, "./cm_analysis/objects/family_a.rds")

write_rds(proxy_killer, "./cm_analysis/objects/proxy_killer.rds")

##### ALIVE #####

# All plots are made in a similar way. First, I group by season and filter out
# the specific season. Then, I count how many times the criminal was caught and
# alive, dropping missing caught data so I can have the right type of grouping
# for the plot. There's no need to group by episode here because the criminal
# can only be caught or escape once per episode, so grouping by episode is
# redunant and dodging by episode would make the plot very messy for no good
# reason. Next, I create a ggplot and dodge by position so it's clear what the
# counts are for each as opposed to the default stacked chart. I then used my
# own color scale because I thought it looked nicer than the default and I
# changed the theme because I didn't like how the default gray background
# looked.

# Season One

alive_1 <- cm_season %>% 
  group_by(season) %>% 
  filter(season == "Season 1") %>% 
  count(caught, alive) %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = alive)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", 
                               "darkorchid3", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = NULL,
       title = "Are criminals caught dead or alive?",
       subtitle = "Based on the season one of Criminal Minds",
       fill = "Criminal Alive?")

# Season Two

alive_2 <- cm_season %>% 
  group_by(season) %>% 
  filter(season == "Season 2") %>% 
  count(caught, alive) %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = alive)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", 
                               "darkorchid3", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = NULL,
       title = "Are criminals caught dead or alive?",
       subtitle = "Based on the season two of Criminal Minds",
       fill = "Criminal Alive?")

# Season Three

alive_3 <- cm_season %>% 
  group_by(season) %>% 
  filter(season == "Season 3") %>% 
  count(caught, alive) %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = alive)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", 
                               "darkorchid3", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = NULL,
       title = "Are criminals caught dead or alive?",
       subtitle = "Based on the season three of Criminal Minds",
       fill = "Criminal Alive?")

# Season Four

alive_4 <- cm_season %>% 
  group_by(season) %>% 
  filter(season == "Season 4") %>% 
  count(caught, alive) %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = alive)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", 
                               "darkorchid3", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = NULL,
       title = "Are criminals caught dead or alive?",
       subtitle = "Based on the season four of Criminal Minds",
       fill = "Criminal Alive?")

# Season Five

alive_5 <- cm_season %>% 
  group_by(season) %>% 
  filter(season == "Season 5") %>% 
  count(caught, alive) %>% 
  drop_na(caught) %>% 
  
  ggplot(aes(x = caught, y = n, fill = alive)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("darkslateblue", "dodgerblue", 
                               "darkorchid3", "firebrick2")) +
  theme_light() +
  labs(x = "Criminal Caught?",
       y = NULL,
       title = "Are criminals caught dead or alive?",
       subtitle = "Based on the season five of Criminal Minds",
       fill = "Criminal Alive?")

# Write out objects

write_rds(alive_1, "./cm_analysis/objects/alive_1.rds")

write_rds(alive_2, "./cm_analysis/objects/alive_2.rds")

write_rds(alive_3, "./cm_analysis/objects/alive_3.rds")

write_rds(alive_4, "./cm_analysis/objects/alive_4.rds")

write_rds(alive_5, "./cm_analysis/objects/alive_5.rds")


