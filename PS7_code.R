install.packages("stringr")
library(dplyr)
library(stringr)

setwd("C:/Users/Alex Petri/Downloads/GithubASP/PS7")
crimeData <- read.csv("March2018.csv")
crimeData <- as_tibble(crimeData)



crimeType_Day <- crimeData %>% 
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(crimeData$Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
    strsplit(x = x, split = "[-/]")[[1]][1] #[[1]][1] because the data type for strsplit is a list of observations
  })) %>% 
  group_by(crime, day) %>% #group the new tibble by the cleaned data crime and day
  summarise(count =n()) #summarize the count

class(crimeType_Day)
