
# GH_AI_Symposium 2019-01-11 -----------------------------------------------------

# load the tools
library(tidyverse)
library(readxl)
library(janitor) 

# load the data
data <- read_excel("global_mortality.xlsx") %>% clean_names()

# have a first look at the data
str(data)

# check which countries don't have a country_code
data %>% filter(is.na(country_code)) %>% select(country)

# select only the Netherlands and Germany
nl_de <- data %>% filter(country == "Netherlands" | country == "Germany")

# check minimum and maximum of included year per country
nl_de %>% 
  group_by(country) %>% 
  summarise(min(year), max(year))

# deleting country_code variable which is not needed anymore
nl_de$country_code <- NULL

# bring data in tidy format
nl_de <- nl_de %>% 
  gather(key = "cause", value = "percent", -c(year, country))

nl_de

# check which fields still contain missing values
nl_de %>% filter(is.na(percent))

# recode missing into zeros
nl_de <- nl_de %>% 
  mutate(percent = if_else(is.na(percent), 0, percent))

nl_de %>% filter(is.na(percent))

# get the mean percentage per cause of all years per country
nl_de_all_years <- nl_de %>% 
  group_by(country, cause) %>% 
  summarise(mean_percent = mean(percent)) 

# plot all mean of all causes in scatter plot
ggplot(nl_de_all_years, aes(x = cause, y = mean_percent)) +
  geom_point() 

# that was horrible - let's try a bar chart
ggplot(nl_de_all_years, aes(cause, mean_percent)) +
  geom_col() 

# much better but we need some reordering
ggplot(nl_de_all_years, aes(x = reorder(cause, -mean_percent), y = mean_percent)) +
  geom_col() 

# I want to have the causes on the y-axis
ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent)) +
  geom_col() +
  coord_flip()

# don't like the cause coding - gonna remove the _percent
nl_de_all_years <- nl_de_all_years %>% 
  mutate(cause = str_remove(cause, "_percent"), 
         cause = str_replace_all(cause, "_", " "),
         cause = toupper(cause))

ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent)) +
  geom_col() +
  coord_flip()

# those axis need some proper labeling
ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent)) +
  geom_col() +
  coord_flip() +
  labs(x = "CAUSE OF DEATH", y = "MEAN PERCENT")

# let's get this plot per country
ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent)) +
  geom_col() +
  coord_flip() +
  labs(x = "CAUSE OF DEATH", y = "MEAN PERCENT") +
  facet_wrap("country")

# nice! now, some polishing
library(ggpubr)

ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent)) +
  geom_col() +
  coord_flip() +
  labs(x = "CAUSE OF DEATH", y = "MEAN PERCENT") +
  facet_wrap("country") +
  theme_pubr()

# let's give the countries some colours
ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent, fill = country)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(x = "CAUSE OF DEATH", y = "MEAN PERCENT") +  
  facet_wrap("country") +
  theme_pubr()

# don't like that legend and I want a black outline around the bars
ggplot(nl_de_all_years, aes(reorder(cause, mean_percent), mean_percent, fill = country)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(x = "CAUSE OF DEATH", y = "MEAN PERCENT") +
  facet_wrap("country") +
  guides(fill = "none") +
  theme_pubr()

# THAT'S IT! 8 lines for a nice publishable graph!



# bonus: fancy animation!
library(gganimate)

# this time per year
nl_de_per_years <- nl_de %>% 
  group_by(country, cause, year) %>% 
  summarise(mean_percent = mean(percent)) %>% 
  ungroup() %>% 
  mutate(cause = str_remove(cause, "_percent"), 
         cause = str_replace_all(cause, "_", " "),
         cause = toupper(cause))

year_plot <- ggplot(nl_de_per_years, aes(reorder(cause, mean_percent), mean_percent, fill = country)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(x = "", y = "MEAN PERCENT") +
  ggtitle("CAUSE OF DEATH \nPER COUNTRY", 
          subtitle = "IN {closest_state}") +
  facet_wrap("country") +
  guides(fill = "none") +
  theme_pubr() +
  theme(title = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "plain", size = 10))

anim <- year_plot +
  transition_states(year, transition_length = 2, state_length = 1)

animate(anim, width = 800, height = 600, ani.res = 300)

anim_save(filename = "death_cause.mp4")

