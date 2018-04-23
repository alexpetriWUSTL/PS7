install.packages("stringr")
library(dplyr)

setwd("C:/Users/Alex Petri/Downloads/GithubASP/PS7")
crimeData <- read.csv("March2018.csv")
crimeData <- as_tibble(crimeData)


#2
crimeType_Day <- crimeData %>% #cleaned and sorted type of crime per day
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(crimeData$Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
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
  summarise(count = n()) %>% #summarise the number of times that neighborhood appears
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
  
crimeType <- crimeData %>%
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(crimeData$Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
    strsplit(x = x, split = "[-/]")[[1]][1] #[[1]][1] because the data type for strsplit is a list of observations
  })) %>%
  mutate(Crime, category = sapply(Crime, function(x){ #each crime is given a category, and after exploring the data i noticed each group of crimes has a unique code by 10000s (ex homicide 10000, robbery 20000)
    if(nchar(as.character(x)) == 5){
      substr(paste("0", as.character(x), sep = ""), start = 1, stop = 2) #need to take a substring, but must have the code be 01 to take a substring
    } else {
      substr(as.character(x), start = 1, stop = 2)
    }
    })
  ) %>%
  select(District, Neighborhood, day, crime, category)  #really only need these variables 
  
summaryType <- crimeType %>% 
  group_by(crime) %>% #group by crime to visualize
  summarise(count = n(), category = mean(as.numeric(category))) #call summarise to visualize what category each crime is in
  
print(arrange(summaryType, desc(category)), n = 35)

#now I want to recode what I developed above. I want to group all thefts together, all violent crimes, and all miscellaneous crimes
violentPicture <- crimeType %>%
  filter(as.numeric(category) == 1 |
                         as.numeric(category) == 17 |
                         as.numeric(category) == 14 |
                         as.numeric(category) == 9 |
                         as.numeric(category) == 4 |
                         as.numeric(category) == 2) %>%
  group_by(day, crime) %>%
  summarise(count = n()) %>%
  filter(substr(day, start = 1, stop = 2) == "03")

drugs_alchohol_Picture <- filter(as.numeric(category) == 26 |
                         as.numeric(category) == 25 |
                         as.numeric(category) == 24 |
                         as.numeric(category) == 22 |
                         as.numeric(category) == 21 |
                         as.numeric(category) == 20 |
                         as.numeric(category) == 18 |
                         as.numeric(category) == 15 |
                         as.numeric(category) == 8 = "misc")

theftPicture <- filter



ggplot(data=violentPicture, mapping = aes(x = day, y = count, col = crime)) +
  geom_line() +
  geom_point()

#6
cleaned <- crimeData %>%
  mutate(DateOccur, day = sapply(as.character(DateOccur), substr, start = 1, stop = 10)) %>%  #clean up date data - we don't want time just the day
  mutate(Description, crime = sapply(as.character(crimeData$Description), function(x){ #clean up the description data - make the crimes more generalizable (robbery without specifying carjacking)
    strsplit(x = x, split = "[-/]")[[1]][1] #[[1]][1] because the data type for strsplit is a list of observations
  })) 

districtOne <- crimeData %>% 
  filter(District == 1) 



ggplot(data = districtOne) +
  geom_line(aes(x = day, y = ))
