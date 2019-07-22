library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(shinydashboard)

raw.df <- read.csv(file="./data/gun-violence-data_01-2013_03-2018.csv", stringsAsFactors = FALSE)

df <- raw.df

drops <- c("address", "incident_url", "source_url", "incident_url_fields_missing", "congressional_district","gun_stolen",
           "gun_type","incident_characteristics", "location_description", "n_guns_involved", "notes", "participant_name", 
           "participant_relationship", "participant_age","participant_status", "sources", "state_house_district", "state_senate_district")
df = df[, !names(df) %in% drops]


df = df %>% 
  rename(id=incident_id, city=city_or_county) %>% 
  mutate(date = as.Date(date),
         month = lubridate::month(date),
         year = lubridate::year(date),
         n_victims = n_killed + n_injured) %>% 
  arrange(date) 


unique(df$year)
unique(df$city)

#Exploring Location Related Trends
#make it selectionable with each state
df %>% 
  mutate(weekday = weekdays(date),wd=wday(date))%>% 
  filter(year!=2013 & year!=2018) %>%
  group_by(weekday, wd, state) %>% 
  summarise(dead= sum(n_killed),
            injured = sum(n_injured)) %>%
  gather(key=victims, value=total, dead, injured) %>% 
  ggplot(aes(x=weekday)) +
  geom_col(alpha=0.5, aes(y=total, fill=victims)) +
  facet_wrap(~state)

df = df %>%
  mutate(abbreviation = 
  case_when(grepl('District of Columbia',state) ~'dc',
            grepl('Mississippi', state) ~ 'MS',
            grepl('Oklahoma', state) ~ 'OK',
            grepl('Delaware', state) ~ 'DE',
            grepl('Minnesota', state) ~ 'MN',
            grepl('Illinois', state) ~ 'IL', 
            grepl('Arkansas', state) ~ 'AR', 
            grepl('New Mexico', state) ~ 'NM',
            grepl('Indiana', state) ~ 'IN',
            grepl('Maryland', state) ~ 'MD',
            grepl('Louisiana', state) ~ 'LA',
            grepl('Idaho', state) ~ 'ID',
            grepl('Wyoming', state) ~ 'WY',
            grepl('Tennessee', state) ~ 'TN',
            grepl('Arizona', state) ~ 'AZ',
            grepl('Iowa', state) ~ 'IA',
            grepl('Michigan', state) ~ 'MI',
            grepl('Kansas', state) ~ 'KS', 
            grepl('Utah', state) ~ 'UT', 
            grepl('Virginia', state) ~ 'VA', 
            grepl('Oregon', state) ~ 'OR', 
            grepl('Connecticut', state) ~ 'CT',
            grepl('Montana', state) ~ 'MT',
            grepl('California', state) ~ 'CA', 
            grepl('Massachusetts', state) ~'MA',
            grepl('West Virginia', state) ~ 'WV',
            grepl('South Carolina', state) ~ 'SC', 
            grepl('New Hampshire', state) ~ 'NH',
            grepl('Wisconsin', state) ~ 'WI',
            grepl('Vermont', state) ~ 'VT',
            grepl('Georgia', state) ~ 'GA', 
            grepl('North Dakota', state) ~ 'ND',
            grepl('Pennsylvania', state) ~ 'PA',
            grepl('Florida', state) ~ 'FL',
            grepl('Alaska', state) ~ 'AK',
            grepl('Kentucky', state) ~ 'KY',
            grepl('Hawaii', state) ~ 'HI',
            grepl('Nebraska', state) ~ 'NE',
            grepl('Missouri', state) ~ 'MO',
            grepl('Ohio', state) ~ 'OH',
            grepl('Alabama', state) ~ 'AL',
            grepl('Rhode Island', state) ~ 'RI',
            grepl('South Dakota', state) ~ 'SD',
            grepl('Colorado', state) ~ 'CO',
            grepl('New Jersey', state) ~ 'NJ',
            grepl('Washington', state) ~ 'WA',
            grepl('North Carolina', state) ~ 'NC',
            grepl('New York', state) ~ 'NY',
            grepl('Texas', state) ~ 'TX',
            grepl('Nevada', state) ~ 'NV',
            grepl('Maine', state) ~ 'ME'))

#Location by Year
df %>% 
  filter(year!=2013 & year!=2018) %>%
  group_by(date, abbreviation) %>%
  summarise(count=sum(n_victims)) %>%
  ggplot(aes(x=date, y=count)) +
  geom_point(color="green") +
  geom_text(aes(label=abbreviation),
            check_overlap=TRUE,
            size=1)
names(df)

#-----------

abc1$participant_age_group

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

# 
# 
# abc1 %>% 
#   filter(participant_type=="Subject") %>% 
#   ggplot(aes(x=participant_type)) +
#   geom_bar(aes(fill=participant_age_group), position="fill") +
#   coord_polar(theta="y") + 
#   theme_classic() +
#   theme(plot.title=element_text(hjust=0.5),
#         axis.line = element_line()) + 
#   labs(title="Suspect Age Distribution",
#        x=" ",
#        y=" ")
# 
# 

# df %>%
#   filter((year>=2013) & (year<=2018))%>%
#   group_by(state) %>%
#   summarise(count = sum(n_victims)) %>%
#   ggplot(aes(x=reorder(state, desc(count)), y=count)) +
#   geom_bar(stat='identity', aes(fill = if(year==2013){
#     "red"} elseif(year==2014){
#       "violet"} elseif(year==2015){
#         "yellow"}elseif(year==2016){
#           "green"}elseif(year==2017){
#             "blue"}else{"pink"}))+
#   labs(x=' ', y=' ') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 


