install.packages("stringr")
library(dplyr)

setwd("C:/Users/Alex Petri/Downloads/GithubASP/PS7")
crimeData <- read.csv("March2018.csv")
crimeData <- as_tibble(crimeData)


#2
crimeType_Day <- crimeData %>% 
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(crimeData$Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
    strsplit(x = x, split = "[-/]")[[1]][1] #[[1]][1] because the data type for strsplit is a list of observations
  })) %>% 
  group_by(crime, day) %>% #group the new tibble by the cleaned data crime and day
  summarise(count = n()) #summarize the count

class(crimeType_Day)


#3 - Neighborhood no. 35 has 305 crimes, the highest number
crimeType_Neighborhood <- crimeData %>%
  group_by(Neighborhood) %>% #use the pipes to group it down to neighborhood
  summarise(count = n()) #summarise the number of times that neighborhood appears

arrange(crimeType_Neighborhood, desc(count)) #sort in descending order to find the highest crime neighborhood


#4 District 5 has the highest robbery proportion
library(stringr)

crimeType_Robbery <- crimeData %>%
  filter(str_detect(Description, "ROBBERY")) %>% #filter out all observations that include robbery in description
  group_by(District) %>% #group by district 
  summarise(countRobbery = n()) #count up the robbery observations

crimeType_All <- crimeData %>% #just group and summarise by crime in general
  group_by(District) %>% 
  summarise(countAll = n())
  
district_Proportion <- crimeType_All %>%
  full_join(crimeType_Robbery, by = "District") %>% #full join because i need all rows (district 0 has no robberies but still needs to be included)
  mutate(countRobbery, proportion = countRobbery/countAll) #create a new column with the proportion

arrange(district_Proportion, desc(proportion)) #sort in descending order to find the highest crime neighborhood


#5
library(ggplot2)
  
crimes <- crimeType_Day %>%

ggplot(data=crimeType_Day) +
geom_line(mapping = aes(x = day, y = count))
?geom_line
