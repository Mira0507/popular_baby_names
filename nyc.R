library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

# ny = master data frame for nyc babies (both genders)
ny <- fread('Popular_Baby_Names_NYC.csv')
ny <- ny %>% rename(Year = "Year of Birth", Name = 'Child\'s First Name') %>%
        mutate(Name = toupper(Name))

# ny_girls1 = girls' yearly name count in NYC
ny_girls1 <- ny %>% 
        filter(Gender == 'FEMALE') %>% 
        group_by(Year, Gender, Name) %>% 
        summarize(Counts = sum(Count))

# ny_girls2 = girls' mean rank for five years(2011-2015) in NYC
ny_girls2 <- ny %>% 
        filter(Gender == 'FEMALE') %>% 
        group_by(Gender, Name) %>%
        summarize(Mean_Rank = round(mean(Rank))) %>%
        arrange(Mean_Rank)
ny_girls2 <- as.data.frame(ny_girls2)

# ny_girls3 = The most popular girl names for five years (2011-2015) in NYC
ny_girls3 <- ny %>% 
        filter(Gender == 'FEMALE') %>%
        group_by(Name) %>%
        summarize(Counts = sum(Count))
ny_girls3 <- as.data.frame(ny_girls3 %>% arrange(desc(Counts)))
ny_girls3 <- ny_girls3 %>% mutate(Percent = round(Counts/sum(Counts) * 100, digits = 2))

# ny_girls4 = The least popular girl names in 2011-2015 in NYC
ny_girls4 <- as.data.frame(ny_girls3 %>% arrange(Counts))
ny_girls4 <- ny_girls4 %>% mutate(Percent = round(Counts/sum(Counts) * 100, digits = 2))

# ny_boys3 = The most popular boy names for five years (2011-2015) in NYC
ny_boys3 <- ny %>% 
        filter(Gender == 'MALE') %>%
        group_by(Name) %>%
        summarize(Counts = sum(Count))
ny_boys3 <- as.data.frame(ny_boys3 %>% arrange(desc(Counts)))
ny_boys3 <- ny_boys3 %>% mutate(Percent = round(Counts/sum(Counts) * 100, digits = 2))

# ny_girls4 = The least popular girl names in 2011-2015 in NYC
ny_boys4 <- as.data.frame(ny_boys3 %>% arrange(Counts))
ny_boys4 <- ny_boys4 %>% mutate(Percent = round(Counts/sum(Counts) * 100, digits = 2))

# plots for top 10 girl/boy names
top10_girls <- ggplot(ny_girls3[1:10, ], aes(x = reorder(Name, -Counts), y = Counts)) + 
        geom_col() + 
        xlab('Names') + 
        ggtitle('10 Most Popular Girl Names in NYC in 2011-2015')
top10_boys <- ggplot(ny_boys3[1:10, ], aes(x = reorder(Name, -Counts), y = Counts)) + 
        geom_col() + 
        xlab('Names') + 
        ggtitle('10 Most Popular Boy Names in NYC in 2011-2015')

