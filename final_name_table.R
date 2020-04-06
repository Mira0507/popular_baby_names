library(tidyverse)
library(ggplot2)
library(formattable)
eboy <- read_csv('e_boy.csv')
egirl <- read_csv('e_girls.csv')
nboy <- read_csv('ny_boys.csv')
ngirl <- read_csv('ny_girls.csv')

# data frames for Top 10 popular names of boys and girls in England Wales vs NYC in 2011-2015
boy_names <- data.frame(England_Wales_Boy = eboy[1:10, 'Name'], 
                        England_Wales_Boy = eboy[1:10, 'Count'],
                        NYC_Boy = nboy[1:10, 'Name'], 
                        NYC_Boy = nboy[1:10, 'Counts'])

girl_names <- data.frame(England_Wales_Girl = egirl[1:10, 'Name'],
                         England_Wales_Girl = egirl[1:10, 'Count'],
                         NYC_Girl = ngirl[1:10, 'Name'],
                         NYC_Girl = ngirl[1:10, 'Counts'])

names(boy_names) <- c('England Wales Boy Names',
                      'England Wales Boy Numbers',
                      'NYC Boy Names',
                      'NYC Boy Numbers')

names(girl_names) <- c('England Wales Girl Names',
                      'England Wales Girl Numbers',
                      'NYC Girl Names',
                      'NYC Girl Numbers')

# tables for boys and girls
table_boys <- formattable(boy_names, 
            list(area(col = c('England Wales Boy Numbers','NYC Boy Numbers')) ~ normalize_bar("gold", 0.2)))

table_girls <- formattable(girl_names, 
                           list(area(col = c('England Wales Girl Numbers','NYC Girl Numbers')) ~ normalize_bar("greenyellow", 0.2)))


