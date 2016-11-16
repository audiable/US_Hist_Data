#Final Project Line Graph
#Marco Pompilj

#Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(maptools)
library(gtools)

#set my working directory  
setwd('/users/marcopompilj/documents/US_Hist_Data')

#access map
mapdata <- read_csv('map.csv')

#read data collected from ipums
ipums <- read_csv('usa_00011.csv',col_types = cols(PERWT=col_double()))

# filter for household heads and children (those 18 and younger)
children <- ipums %>% filter(GQ==1 & AGE<=18)

# Create dataframes for female household heads and create RACE values selecting for variables needed
mother <- ipums %>% filter(GQ==1 & SEX == 2) %>% 
  mutate(mrace = RACE) %>% select(YEAR,SERIAL,PERNUM,mrace)

# do the same for male household heads 
father <- ipums %>% filter(GQ==1 & SEX == 1) %>% 
  mutate(frace = RACE) %>% select(YEAR,SERIAL,PERNUM,frace)

# join the dataframes of mother and father to child and factor for parent/child relationship (ie, parents with same race, different race...etc)
childrenparents <- left_join(children, mother, by = c('YEAR', 'SERIAL', 'MOMLOC' = 'PERNUM')) %>%
  left_join(father, by = c('YEAR','SERIAL', 'POPLOC' = 'PERNUM')) %>%
  mutate(FINAL=factor(ifelse(MOMLOC==0 | POPLOC==0,3,
                             ifelse(mrace==frace,2,1)),
                      labels=c('Different Race Parents','Same Race Parents', 'Less than 2 Parents')))

#Create two data frames: one with total number of children in each year and one with number of children in each parental category in each year
parentrace2 <- childrenparents %>% group_by(YEAR, FINAL) %>% summarise(Number1=sum(PERWT))
parentrace3 <- childrenparents %>% group_by(YEAR) %>% summarise(Number2=sum(PERWT))

#Join data frames and keep only rows for children with parents of different races
joint1 <- left_join(parentrace2,parentrace3) %>% mutate(pct=Number1/Number2) %>% 
  filter(FINAL=='Different Race Parents')

#Calculate percentages 
percentchild <-joint1 %>% mutate(pct=Number1/Number2)

#create the visualization with appropriate specifications 
png('lineproj.png', height = 500, width = 1000) 
ggplot(joint1, aes(x=YEAR, y=pct)) + 
  geom_line() + geom_point() + geom_text(aes(label=paste(round(pct*100,2),'%',sep = '')),vjust=-.5) + labs(x = 'Years', y = 'Percent of Population') +
  scale_y_continuous(labels = scales::percent) +
  labs(title='Percent of Children with Parents of Different Races in the US')
dev.off()




