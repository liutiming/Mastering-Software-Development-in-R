install.packages("readxl")
install.packages("tidyverse")
library(tidyverse)
library(readxl)

setwd("../Mastering-Software-Development-in-R/")

csv <- read_csv("_f018d9fe5547b1a722ce260af0fa71af_quiz_data/data/daily_SPEC_2014.csv.bz2")

str(csv)

q1 <- filter(csv, `State Name` == "Wisconsin", `Parameter Name` == "Bromine PM2.5 LC") %>% summarize(mean = mean(`Arithmetic Mean`))

q2 <- csv %>% 
  group_by(`Parameter Name`) %>% 
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T))

q3 <- csv %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>% 
  group_by(`State Code`, `County Code`, `Site Num`) %>% 
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T))

q4c  <- csv %>% 
  filter(`State Name` == "California", `Parameter Name` == "EC PM2.5 LC TOR") %>% 
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T)) %>% 
  pull()

q4a <- csv %>% 
  filter(`State Name` == "Arizona", `Parameter Name` == "EC PM2.5 LC TOR") %>% 
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T)) %>% 
  pull()

q4a-q4c


q5 <- csv %>% 
  filter(`Longitude` < -100, `Parameter Name` == "OC PM2.5 LC TOR") %>% 
  summarize(mean = median(`Arithmetic Mean`, na.rm = T)) %>% 
  pull()

library(readxl)
xlsx <- readxl::read_excel("_f018d9fe5547b1a722ce260af0fa71af_quiz_data/data/aqs_sites.xlsx")



q6_x <- xlsx %>% 
  filter(`Land Use` == "RESIDENTIAL", `Location Setting` == "SUBURBAN", `Longitude` >=-100) %>% 
  mutate(`Site Num` = `Site Number`)

csv6 <- mutate(csv, "State Code" = as.character(`State Code`) )

csv6$`State Code` <- as.numeric(csv6$`State Code`)
csv6$`County Code` <- as.numeric(csv6$`County Code`)
csv6$`Site Num` <- as.numeric(csv6$`Site Num`)

str(csv6)
str(q6_x)

q6_c <- left_join(q6_x, csv6, by = c("State Code", "County Code", "Site Num") ) %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>% 
  summarize(mean = median(`Arithmetic Mean`, na.rm = T)) %>% 
  pull()

###### 7
csv6 <- csv
csv6$`State Code` <- as.numeric(csv6$`State Code`)
csv6$`County Code` <- as.numeric(csv6$`County Code`)
csv6$`Site Num` <- as.numeric(csv6$`Site Num`)

q7_x <- xlsx %>% 
  filter(`Land Use` == "COMMERCIAL") %>% 
  mutate(`Site Num` = `Site Number`)

q7_c <- left_join(q6_x, csv6, by = c("State Code", "County Code", "Site Num") ) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>% 
  separate(`Date Local`, into = c("year", "month", "date")) %>% 
  group_by(month) %>% 
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T))

#####
q9 <- 
  csv6 %>% 
  filter(`State Code` == 6, `County Code` == 65, `Site Num` ==8001) %>% 
  group_by(`Date Local`, `Parameter Name`) %>%
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T)) %>% 
  filter(`Parameter Name` == "Total Nitrate PM2.5 LC" | `Parameter Name` == "Sulfate PM2.5 LC") %>% 
  spread(key = `Parameter Name`, value = `mean`) %>% 
  filter(`Total Nitrate PM2.5 LC` +`Sulfate PM2.5 LC`>10)
#####
q10 <- 
  csv6 %>% 
  filter(`Parameter Name` == "Sulfate PM2.5 LC" |`Parameter Name` == "Total Nitrate PM2.5 LC") %>% 
  group_by(`State Code`, `County Code`, `Site Num`, `Date Local`, `Parameter Name`) %>% 
  summarize(mean = mean(`Arithmetic Mean`, na.rm = T)) %>% 
  spread(key = `Parameter Name`, value = `mean`) %>% 
  group_by(`State Code`, `County Code`, `Site Num`) %>% 
  mutate(correlation = cor(`Sulfate PM2.5 LC`,`Total Nitrate PM2.5 LC`))
