library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)

# ny = master data frame for nyc babies (both genders)
ny <- fread('Popular_Baby_Names_NYC.csv')
ny <- ny %>% rename(Year = "Year of Birth", Name = 'Child\'s First Name')

# ew_ex = master list for england and wales (both genders)
ew <- excel_sheets('adhocallbabynames1996to2016.xls')
ew_ex <- lapply(ew, read_excel, path = 'adhocallbabynames1996to2016.xls')
ew_data_info <- rbind(ew_ex[[2]], ew_ex[[3]])

# ew_boys = master data frame england and wales (boys) 
# ew_girls = master data frame england and wales (girls) 
ew_boys <- as.data.frame(ew_ex[[4]])
ew_girls <- as.data.frame(ew_ex[[5]])

# ny_girls/boys = data frames for yearly name count sum 
ny_girls <- ny %>% 
        filter(Gender == 'FEMALE') %>% 
        group_by(Year, Gender, Name) %>% 
        summarize(Counts = sum(Count))
ny_boys <- ny %>% 
        filter(Gender == 'MALE') %>% 
        group_by(Year, Gender, Name) %>% 
        summarize(Counts = sum(Count))

# ny_girls1 = list for individual year from ny_girls
ny_girls1 <- split(ny_girls, ny_girls$Year)

# yearly_yg_pop = a vector of newborn girls population in NYC
yearly_yg_pop <- as.vector(sapply(ny_girls1, function(x) sum(x$Counts)))

# yearly_yg_name_num = a vector of names of newborn girls in NYC
yearly_yg_name_num <- as.vector(sapply(ny_girls1, nrow))

sum_count_vec <- function(vec1, vec2) {
        pv <- c(0)
        for (i in 1:length(vec1)) {
                cv <- rep(vec1[i], times = vec2[i])
                pv <- c(pv, cv)
        }
        return(pv[2:length(pv)])
}

nyg_count_sum_col <- sum_count_vec(yearly_yg_pop, yearly_yg_name_num)
ny_girls <- bind_cols(ny_girls, Total_Count = nyg_count_sum_col)
ny_girls <- ny_girls %>% mutate(Percent = Counts / Total_Count * 100)
ny_girls2 <- split(ny_girls, ny_girls$Year)



