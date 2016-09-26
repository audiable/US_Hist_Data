## HIST.90.01-QSS.30.05-FA16
## Lab 1: Getting Started with R and GitHub
## Name: Marco Pompilj
## Date: September 23, 2016

library(dplyr) #Need dplyr to access files in package

## Create different vectors with title, author, year of publication "ypusblished", number in stock "nstock", and price.

title<-c("Hoover", "ASAP Villains", "How I'm Living", "Lost My Mind", "Drip Drip", "Stay True to the Game", "Young", "Low Key", "Like Me", "In Solw Motion")

author<-c("R.J Top", "Sammy Sin", "Armando Stono", "Yevi Lora", "Eve Barbara", "Hamu Zhi Shi", "Scott Mesilj", "Big Brother C", "Willow Fastor", "Lina Podavski")

ypublished<-c(1990, 1967, 1969, 2002, 1996, 1978, 1999, 2001, 1980, 1985)

nstock<-c(78, 17, 41, 82, 19, 121, 27, 89, 43, 50)

price<-c(12.99, 10.50, 17.00, 10.99, 5.99, 18.99, 15.99, 9.00, 11.75, 20.99)

#Create bookstore, "bs", using the different vectors in a dataframe
bs <- data.frame(title,author,ypublished,nstock,price)

#Sort bs by year published (descending) and, within year, by author (ascending), under a new dataframe
sort.bs <- bs %>% arrange(-ypublished, author)

##bookstore goes on sale
##books published before 1990 are 25% off
##books with over 60 in stock will be 40% off
##books both published before 1990 and with more than 60 in stock will be 50% off!

sale <- bs %>% mutate(saleprice=ifelse(ypublished<1990, price*.75,price)) %>%
                        mutate(saleprice=ifelse(nstock>60, price*.60, saleprice)) %>%
                        mutate(saleprice=ifelse(ypublished<1990&nstock>60, price*.5, saleprice))

                                       
#subset created to include only books on sale, and only author, title, and sale price                                       ifelse(nstock>60, price*.6,price)))
salebooks <-sale %>% filter(price != saleprice) %>% select(author,title,saleprice)


  
  