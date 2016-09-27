##Lab2 
##Sept 26, 2016
##Marco Pompilj
##Prof. Merchant (QSS 30.05)

#invoke necessary packages
library(dplyr)
library(readr)
library(tidyr)

#Bring in data from IPUMS and filter out years before 1960 and states Hawaii and Alaska
a <- read_csv('lab2.csv') %>% filter(!(YEAR < 1960 & STATEFIP %in% c(2,15)))

#Character variable for RACE
#Using only races found in time-span we are looking at
b <- a %>% mutate(Race=ifelse(RACED==100,'White',
             ifelse(RACED==130,'Portuguese',
             ifelse(RACED==140,'Mexican',
             ifelse(RACED==150,'Puerto Rican',
             ifelse(RACED==200,'Black/Negro',
             ifelse(RACED==210,'Mulatto', 
             ifelse(RACED==300,'American Indian/Alaska Native',
             ifelse(RACED==373,'Alaskan Mixed',
             ifelse(RACED==400,'Chinese',
             ifelse(RACED==500,'Japanese', 
             ifelse(RACED==600,'Filipino',
             ifelse(RACED==610,'Asian Indian',
             ifelse(RACED==620,'Korean',
             ifelse(RACED==630,'Native Hawaiian',
             ifelse(RACED==631,'Asiatic Hawaiian',
             ifelse(RACED==632,'Caucasian Hawaiian',
             ifelse(RACED==650,'Other Asian or Pacific Islander', 
             'Other Race n.e.c.'))))))))))))))))))

#Use group_by() and summarise() in order to determine population by year and race
c <- b %>% group_by(Race,YEAR) %>% summarise(NUMBER=sum(PERWT))

#have data table show one race for each column and one row for each year 
d <- c %>% spread(Race,NUMBER)
                                              
          
#export table as .csv
write_csv(d,'race_year.csv')

