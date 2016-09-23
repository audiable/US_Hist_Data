#Lab2

library(dplyr)
library(readr)
library(tidyr)

a <- read_csv('lab2.csv') %>% filter(!(YEAR < 1960 & STATEFIP %in% c(2,15)))

#Character variable for RACE
b <- read_csv('lab2cross.csv') 

#Read in crosswalk
race <- read_csv('b.csv' , col_types = cols(race='r'))
