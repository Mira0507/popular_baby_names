library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)

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
# ew_boys_name_count_total = data frame for counts per boy name 
# ew_girls_name_count_total = data frame for counts per girl name 
b_name <- rep(ew_boys$Name, times = length(2015:2011))
g_name <- rep(ew_girls$Name, times = length(2015:2011))
valid_col <- paste(2015:2011, 'Count')

ew_boys_name_count <- ew_boys %>% select(Name, valid_col)
ew_boys_col_count <- sapply(ew_boys_name_count[, 2:ncol(ew_boys_name_count)], as.numeric)
ew_boys_name_count <- cbind(ew_boys_name_count, ew_boys_col_count)

sum_by_row <- function(df) {
        sum_vec <- c()
        for (i in 1:nrow(df)) {
                vec <- df[i, 7:ncol(df)]
                num = sum(vec, na.rm = TRUE)
                sum_vec <- c(sum_vec, num)
        }
        sum_vec
}

ew_boy_vec <- sum_by_row(ew_boys_name_count)
ew_boys_name_count_total <- data.frame(Name = ew_boys_name_count$Name, 
                                       Count = ew_boy_vec)


ew_girls_name_count <- ew_girls %>% select(Name, valid_col)
ew_girls_col_count <- sapply(ew_girls_name_count[, 2:ncol(ew_girls_name_count)], as.numeric)
ew_girls_name_count <- cbind(ew_girls_name_count, ew_girls_col_count)
ew_girl_vec <- sum_by_row(ew_girls_name_count) 

ew_girls_name_count_total <- data.frame(Name = ew_girls_name_count$Name, 
                                        Count = ew_girl_vec)

# final data table with names and counts for girls and boys
ew_boys_name_count_total <- ew_boys_name_count_total %>%
        arrange(desc(Count))
ew_girls_name_count_total <- ew_girls_name_count_total %>% 
        arrange(desc(Count))

# e_boys.csv = a summary table for popular boy names in 2011-2015
# e_girls_csv = a summary table for popular girl names in 2011-2015
write.csv(ew_boys_name_count_total, 'e_boys.csv')
write.csv(ew_girls_name_count_total, 'e_girls.csv')

new_col_names <- c('Name', as.character(2:ncol(ew_boys_name_count)))

# Renaming
ew_boys_name_count1 <- ew_boys_name_count
ew_girls_name_count1 <- ew_girls_name_count
names(ew_boys_name_count1) <- new_col_names
names(ew_girls_name_count1) <- new_col_names

# Column trimming & renaming
ew_boys_name_count1 <- ew_boys_name_count1 %>% 
        select(Name, 7, 8, 9, 10, 11) 
ew_girls_name_count1 <- ew_girls_name_count1 %>% 
        select(Name, 7, 8, 9, 10, 11) 
names(ew_boys_name_count1) <- c('Name', '2015', '2014', '2013', '2012', '2011')
names(ew_girls_name_count1) <- c('Name', '2015', '2014', '2013', '2012', '2011')

# Selecting top 20 names
ew_boys_top20 <- ew_boys_name_count1 %>% 
        filter(Name %in% ebn20)
ew_girls_top20 <- ew_girls_name_count1 %>%
        filter(Name %in% egn20)

# Data gathering by year
ew_boys_top20 <- gather(ew_boys_top20, Year, Count, -Name)
ew_girls_top20 <- gather(ew_girls_top20, Year, Count, -Name)

# ew_boys_top20 = final top 20 names by year for boys
# ew_girls_top20 = final top 20 names by year for girls
ew_boys_top20 <- ew_boys_top20 %>% 
        arrange(Year) %>% 
        mutate(Year = as.numeric(Year))
ew_girls_top20 <- ew_girls_top20 %>%
        arrange(Year) %>% 
        mutate(Year = as.numeric(Year))

ew_boys_top10 <- ew_boys_top20 %>% 
        filter(Name %in% e_boys_top10)

ew_girls_top10 <- ew_girls_top20 %>% 
        filter(Name %in% e_girls_top10)
# plots
ew_boys_top10_plot <- ggplot(ew_boys_top10, aes(x = Year, y = Count)) +
        geom_line(aes(color = Name), size = 1.5) + 
        ylab('Numbers') + 
        ggtitle('Popularity Trend of Boy Names in England/Wales in 2011-2015')

ew_girls_top10_plot <- ggplot(ew_girls_top10, aes(x = Year, y = Count)) +
        geom_line(aes(color = Name), size = 1.5) + 
        ylab('Numbers') + 
        ggtitle('Popularity Trend of Girl Names in England/Wales in 2011-2015')

