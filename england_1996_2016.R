library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

# ew_ex = master list for england and wales (both genders)
ew <- excel_sheets('adhocallbabynames1996to2016.xls')
ew_ex <- lapply(ew, read_excel, path = 'adhocallbabynames1996to2016.xls')

# ew_boys = master data frame england and wales (boys) 
# ew_girls = master data frame england and wales (girls) 
ew_boys <- as.data.frame(ew_ex[[4]])
ew_girls <- as.data.frame(ew_ex[[5]])

# rename columns 
years <- as.character(2016:1996)
cl <- c('Rank', 'Count')
auto <- function(years, cl) {
        vec <- c('Name')
        for (x in years) {
                for (y in cl) {
                        vec <- c(vec, paste(x, y)) 
                }
        }
        vec
}
names(ew_boys) <- auto(years, cl)
names(ew_girls) <- auto(years, cl)

# trim unnecessary rows 
ew_boys <- ew_boys[6:nrow(ew_boys), ]
ew_girls <- ew_girls[6:nrow(ew_girls), ]

# count for names
# ew_boys_name_count = data frame for counts per boy name 
# ew_girls_name_count = data frame for counts per girl name 
b_name <- rep(ew_boys$Name, times = length(2016:1996))
g_name <- rep(ew_girls$Name, times = length(2016:1996))
valid_col <- paste(2016:1996, 'Count')
build_b <- function(df, col) {
        vec <- c()
        for (x in col) {
                vec <- c(vec, df[, x])
        }
        vec
}
ew_boys_name_count <- data.frame(Name = b_name, Counts = build_b(ew_boys, valid_col))
ew_boys_name_count <- ew_boys_name_count %>% 
        filter(Counts != ':') %>%
        group_by(Name) %>%
        mutate(Counts = as.numeric(Counts)) %>%
        summarize(Count = sum(Counts)) 
ew_boys_name_count <- ew_boys_name_count %>% 
        arrange(desc(Count))

ew_girls_name_count <- data.frame(Name = g_name, Counts = build_b(ew_girls, valid_col))
ew_girls_name_count <- ew_girls_name_count %>% 
        filter(Counts != ':') %>%
        group_by(Name) %>%
        mutate(Counts = as.numeric(Counts)) %>%
        summarize(Count = sum(Counts)) 
ew_girls_name_count <- ew_girls_name_count %>% 
        arrange(desc(Count))

# plots 

boys_9616 <- ggplot(ew_boys_name_count[1:10, ], aes(x = reorder(Name, -Count), y = Count)) + 
        geom_col() + 
        xlab('Names') + 
        ylab('Counts') + 
        ggtitle('10 Most Popular Boy Names in England and Wales in 1996-2016')

girls_9616 <- ggplot(ew_girls_name_count[1:10, ], aes(x = reorder(Name, -Count), y = Count)) + 
        geom_col() + 
        xlab('Names') + 
        ylab('Counts') + 
        ggtitle('10 Most Popular Girl Names in England and Wales in 1996-2016')







