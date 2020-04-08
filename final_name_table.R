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

# ebn20 = 20 popular england boy names
# ubn20 = 20 popular us boy names
ebn20 <- boy_names[[1]]
ubn20 <- boy_names[[3]]

# egn20 = 20 popular england girl names
# ugn20 = 20 popular us girl names
egn20 <- girl_names[[1]]
ugn20 <- girl_names[[3]]

# unique_ebn20 = england boy names not popular in the us
# unique_ubn20 = us boy names not popular in england
# common_bn20 = common boy names
unique_ebn20 <- ebn20[!ebn20 %in% ubn20]
unique_ubn20 <- ubn20[!ubn20 %in% ebn20]
common_bn20 <- ebn20[ebn20 %in% ubn20]

# unique_egn20 = england girl names not popular in the us
# unique_ugn20 = us girl names not popular in england
# common_gn20 = common girl names
unique_egn20 <- egn20[!egn20 %in% ugn20]
unique_ugn20 <- ugn20[!ugn20 %in% egn20]
common_gn20 <- egn20[egn20 %in% ugn20]