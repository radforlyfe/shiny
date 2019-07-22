# splits = sapply(strsplit(df$participant_age_group, split='||', fixed = TRUE), function(x){ strsplit(x, '::', fixed = TRUE)})
library(shiny)
library(tidyverse)
library(ggplot2)
library(stringr)


split_age = df %>% 
  select(id,participant_age_group) %>%
  separate_rows(participant_age_group, sep="\\|+")
split_gender = df %>% select(id,participant_gender) %>% separate_rows(participant_gender, sep="\\|+")
split_type = df %>% select(id,participant_type) %>% separate_rows(participant_type, sep="\\|+")


age = split_age %>% 
  filter(!is.na(participant_age_group) & (participant_age_group != "")) %>% 
  group_by(id) %>% 
  summarise(count=n()) 
gender = split_gender %>% 
  filter(!is.na(participant_gender) & (participant_gender != "")) %>% 
  group_by(id) %>% 
  summarise(count=n())
type = split_type %>% 
  filter(!is.na(participant_type) & (participant_type != "")) %>% 
  group_by(id) %>% 
  summarise(count=n())

abc = inner_join(age, inner_join(gender, type, by="id"), by="id")

keep_ids = abc %>% 
  filter(count == count.x & count == count.y) %>% 
  select(id)

a1 = split_age %>% 
  filter(id %in% keep_ids$id)
b1 = split_gender %>% 
  filter(id %in% keep_ids$id)
c1 = split_type %>% 
  filter(id %in% keep_ids$id)

abc1 = cbind(a1,b1,c1)
abc1 <- abc1[,-c(3,5)]

unique(abc1$participant_age_group)
abc1 <- abc1 %>%
  mutate(participant_age_group = str_extract(participant_age_group, '[a-zA-Z]+'),
         participant_gender = str_extract(participant_gender, '[a-zA-Z]+'),
         participant_type = str_extract(participant_type,'[a-zA-Z]+'))
unique(abc1$participant_type)


class(df$date)

