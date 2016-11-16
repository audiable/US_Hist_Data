#Final Project Map
#Marco Pompilj
#
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

#set-up map to be used later 
map1 <- ggplot() + theme_nothing(legend=TRUE) +
  geom_polygon(data=mapdata, aes(x=long,y=lat,group=group),fill='white',color='black')
png('map.png',width=1500,height=1000)
print(map1)
dev.off()

#read data collected from ipums
ipums <- read_csv('usa_00011.csv',col_types = cols(PERWT=col_double()))

# Create household head df / filter nonhouseholds and adults (those over 18)
children <- ipums %>% filter(GQ==1 & AGE<=18)

# Create spouse dataframe and filter out nonhouseholds 
mother <- ipums %>% filter(GQ==1 & SEX == 2) %>% 
  mutate(mrace = RACE) %>% select(YEAR,SERIAL,PERNUM,mrace)

#create male household head data frame
father <- ipums %>% filter(GQ==1 & SEX == 1) %>% 
  mutate(frace = RACE) %>% select(YEAR,SERIAL,PERNUM,frace)

# join the dataframes of children and parents together and assign labels to different parent-child relationships 
childrenparents <- left_join(children, mother, by = c('YEAR', 'SERIAL', 'MOMLOC' = 'PERNUM')) %>%
  left_join(father, by = c('YEAR','SERIAL', 'POPLOC' = 'PERNUM')) %>%
  mutate(FINAL=factor(ifelse(MOMLOC==0 | POPLOC==0,3,
                             ifelse(mrace==frace,2,1)),
                      labels=c('Different Race Parents','Same Race Parents', 'Less than 2 Parents')))

#Create two data frames: one with total number of children in each year and state, and one with number of children in each parental category in each year and state
parentrace <- childrenparents %>% group_by(YEAR, FINAL, STATEFIP) %>% summarise(Number1=sum(PERWT)) 
parentrace1 <- childrenparents %>% group_by(YEAR, STATEFIP) %>% summarise(Number2=sum(PERWT)) 


#Join data frames
joint <- left_join(parentrace, parentrace1) 

#calculate percentages and filter for children with parents of different races 
percentchild <-joint %>% mutate(pct=Number1/Number2) %>% 
  filter(FINAL=='Different Race Parents') 

#Create factor variable with the quantiles of a continous variable
cuts <- quantcut(percentchild$pct,q=seq(0,1,.2))

#factor population number and assign labels
dscats <- percentchild %>% mutate(Population=factor(ifelse(pct<.001,1,
                                                 ifelse(pct<.003,2,
                                                        ifelse(pct<.01,3,
                                                               ifelse(pct<.03,4,5))))))
levels(dscats$Population) <- c('>0 - <.1%','.1% - <.3%','.3% - <1%','1% - <3%','3%+')

#create new map 
newmap <- mapdata %>% mutate(STATEI=as.integer(STATEFIP))

#return rows from the left table, and any rows with matching keys from the right table
dsmap <- left_join(dscats,newmap,by=c('STATEFIP'='STATEI'))

#order map 
dsmap <- dsmap %>% arrange(order)

#create map of 1900 and set-up parameters
mapmix1 <- ggplot() + theme_nothing(legend=TRUE) +
  scale_fill_brewer(palette = "Reds") +
  geom_polygon(data=mapdata, aes(x=long,y=lat,group=group),fill='white',color='black') +
  geom_polygon(data=filter(dsmap,YEAR==1900),aes(x=long,y=lat,group=group,fill=Population),color='black')
png('map.png',width=1500,height=1000)
print(mapmix1)
dev.off()

#animate maps by creating .gif file

library(devtools)
devtools::install_github('dgrtwo/gganimate')

library(gganimate)

#create maps and assign proper specifications 
anmap <- map1 + scale_fill_brewer(palette='Reds') +
  geom_polygon(data=dsmap,aes(x=long,y=lat,group=group,fill=Population,frame=YEAR),color='black') +
  labs(title='Percent of Children with Parents of Different Races, ')

gg_animate(anmap,ani.width=1500,ani.height=1000,'anmap1.gif')




