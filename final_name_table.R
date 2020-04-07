library(tidyverse)
library(formattable)
eboy <- read_csv('e_boys.csv')
egirl <- read_csv('e_girls.csv')
uboy <- read_csv('us_boys.csv')
ugirl <- read_csv('us_girls.csv')


boy_names <- data.frame(England_Wales_Boy = eboy[1:20, 'Name'], 
                        England_Wales_Boy = eboy[1:20, 'Count'],
                        US_Boy = uboy[1:20, 'Name'], 
                        US_Boy = uboy[1:20, 'Counts'])

girl_names <- data.frame(England_Wales_Girl = egirl[1:20, 'Name'],
                         England_Wales_Girl = egirl[1:20, 'Count'],
                         US_Girl = ugirl[1:20, 'Name'],
                         US_Girl = ugirl[1:20, 'Counts'])
name <- c('Names in England/Wales', 
          'Numbers in England/Wales',
          'Names in the United States', 
          'Numbers in the United States')

names(boy_names) <- name
names(girl_names) <- name

# tables for boys and girls
table_boys <- formattable(boy_names, 
            list(area(col = c('Numbers in England/Wales','Numbers in the United States')) ~ normalize_bar("gold", 0.2)))

table_girls <- formattable(girl_names, 
                           list(area(col = c('Numbers in England/Wales','Numbers in the United States')) ~ normalize_bar("greenyellow", 0.2)))


