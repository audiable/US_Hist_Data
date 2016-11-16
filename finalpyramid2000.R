#Marco Pompilj
#Population Pyramid for Final Project

#Load packages we need 
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#Read in IPUMS data
a <- read_csv('usa_00015.csv')


#Create vector of age category labels
agecats <- '0-9'
for (i in 1:7) {
  agecats <- c(agecats,paste(i,'0-',i,9,sep=''))
}
agecats <- c(agecats,'80+')

#assign labels to sex
b <- a %>% mutate(Sex=factor(SEX,labels=c('Male','Female')))

#seperate ages by 0-9,10-19,20-29,30-39,40-49,50-59,60-69,70-79,80+
c <- b %>% mutate(Age=ifelse(AGE>=80,8,floor(AGE/10)))

#assign labels to age 
d <- c %>% mutate(Age=factor(Age,labels=agecats))

#accounting for people of two races, or three or more race
e <- d %>% filter(RACE == 8 | RACE == 9)



#group by age and sex
#change male population to negatives to accomodate population pyramid structure 
g <- e %>% group_by(Age,Sex) %>% summarise(Number=sum(PERWT))
h2 <- g %>% mutate(Number=ifelse(Sex=='Male',-1 *Number,Number))

#create population pyramid with proper dimensions and orientation 
png('poppyr_finp.png',height=500,width=2000)
ggplot(data=h2,aes(x=Age,y=Number,fill=Sex)) +
  geom_bar(data=h2[h2$Sex=='Male',], stat='identity') +
  geom_bar(data=h2[h2$Sex=='Female',],stat='identity') +
  coord_flip() +


   #create appropriate breaks
  scale_y_continuous(breaks=c(-1000000,-750000,-500000,-250000,0,250000,500000,750000,1000000),
                     labels=c('1','.75','.5','.25','0','.25','.5','.75','1')) +
  
  #format graph: set proper axes, title, stylization 
  labs(y='Population in Millions',title='Population Pyramids for Mixed Race Americans in 2000') +
  scale_fill_brewer(palette='Set1') +
  guides(fill=guide_legend(title='Sex',title.position='top',reverse=TRUE)) +
  theme_bw() + theme(legend.position='bottom') 
dev.off()


