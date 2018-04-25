install.packages("stringr")
library(dplyr)

setwd("C:/Users/Alex Petri/Downloads/GithubASP/PS7")
crimeData <- read.csv("March2018.csv")
crimeData <- as_tibble(crimeData)


#2
crimeType_Day <- crimeData %>% #cleaned and sorted type of crime per day
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
    strsplit(x = x, split = "[-/]")[[1]][1] #[[1]][1] because the data type for strsplit is a list of observations
  })) %>% 
  group_by(crime, day) %>% #group the new tibble by the cleaned data crime and day
  summarise(count = n()) #summarize the count

#the question asks us to first sort the number of crime per day by type of crime
#and then find the most common crimes in march. As a result, I will compute something for crime totals in march
#as well


crime_March <- crimeData %>%
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(crimeData$Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
    strsplit(x = x, split = "[-/]")[[1]][1] #[[1]][1] because the data type for strsplit is a list of observations
  })) %>% 
  group_by(crime) %>%
  summarise(count = n())

arrange(crime_March, desc(count)) #Larceny, Leaving the scene, and Destruction of property were the three most common crimes in march


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


#5 For my types of crime, I use the 5 most common crimes from question #2
library(ggplot2)
library(tidyr)

crimeType_Day_March <- crimeType_Day %>% #make sure the dates are in march because there are some non-march dates that are likely crimes reported in march that took place earlier
  filter(substr(day, start = 1, stop = 2) == "03" & substr(day, start = 7, stop = 10) == "2018")
  
topFive <- crimeType_Day_March %>% #filter out 5 most prominent crimes
  filter(crime == "LARCENY" | crime == "LEAVING SCENE OF ACCIDENT" | crime == "DESTRUCTION OF PROPERTY"
         | crime == "AGG.ASSAULT" | crime == "AUTO THEFT") %>%
  mutate(day, day = substr(day, start = 2, stop = 5)) #take substring of date to make it shorter and able to fit on an axis

ggplot(topFive, aes(x = day, y = count, colour = crime, group = crime)) + #plot it with appropriate labels
  geom_line() +
  xlab("Day in March") +
  ylab("Crime Occurences") +
  ggtitle("Crime Occurences for the 5 Most Common STL Crimes in March") +
  theme(axis.text.x = element_text(vjust = 0)) +
  scale_color_manual(values = c("blue", "red", "green",  
                                "orange", "purple"), labels = c("Aggravated Assault", "Auto Theft",
                                "Destruction of Property", "Larceny", "Leaving the Scene of an Accident"))


#6
larceny <- crimeData %>% #filter out larceny
  mutate(District, District = as.character(District)) %>% #District needs to be a character so it is a discrete variable for graphing
  filter(str_detect(Description, "LARCENY")) %>% 
  filter(substr(DateOccur, start = 1, stop = 2) == "03" & substr(DateOccur, start = 7, stop = 10) == "2018" & District != 0) %>% #keep only March 2018 days
  mutate(DateOccur, day = substr(DateOccur, start = 2, stop = 5)) %>% #take substring of date to make it shorter and able to fit on an axis
  group_by(District, day) %>%
  summarise(count = n()) 

ggplot(larceny, aes(x = day, y = count, colour = District, group = District)) + #plot it with appropriate labels
  geom_line() +
  xlab("Day in March") +
  ylab("Larceny Occurences") +
  ggtitle("Larceny Occurences by STL District in March") +
  scale_color_manual(values = c("lightblue", "red", "green",  
                               "orange", "purple", "black"), labels = c("1", "2", '3', '4', '5', '6'))








