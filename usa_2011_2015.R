library(tidyverse)
library(data.table)
library(ggplot2)

# usa = master data frame of USA baby names in 2011-2015
usa <- fread('us_2011_2015.csv')

usa_boy <- usa %>% 
        filter(Gender == 'M') %>% 
        select(Name, Gender, Count) %>%
        mutate(Name = toupper(Name))

usa_girl <- usa %>% 
        filter(Gender == 'F') %>%
        select(Name, Gender, Count) %>% 
        mutate(Name = toupper(Name))


usa_boy_count <- usa_boy %>% 
        group_by(Name) %>%
        summarize(Counts = sum(Count)) %>%
        arrange(desc(Counts))

usa_girl_count <- usa_girl %>% 
        group_by(Name) %>%
        summarize(Counts = sum(Count)) %>%
        arrange(desc(Counts))

write.csv(usa_boy_count, 'us_boys.csv')
write.csv(usa_girl_count, 'us_girls.csv')


y2015 <- fread('us_yob_2015.csv')
y2014 <- fread('us_yob_2014.csv')
y2013 <- fread('us_yob_2013.csv')
y2012 <- fread('us_yob_2012.csv')
y2011 <- fread('us_yob_2011.csv')

# a function deleting unnecessary column
trim_df <- function(df) {
        df <- df %>% 
                select(-V1) 
        return(df)
}

# column trimming
y2015 <- trim_df(y2015)
y2014 <- trim_df(y2014)
y2013 <- trim_df(y2013)
y2012 <- trim_df(y2012)
y2011 <- trim_df(y2011)

# adding a column for year
y2015 <- y2015 %>% mutate(Year = 2015)
y2014 <- y2014 %>% mutate(Year = 2014)
y2013 <- y2013 %>% mutate(Year = 2013)
y2012 <- y2012 %>% mutate(Year = 2012)
y2011 <- y2011 %>% mutate(Year = 2011)

# combining data frame from 2011 to 2015
usa_baby_by_yr <- rbind(y2015, y2014, y2013, y2012, y2011)


# popularity change of top 20 boy & girl names 
usa_boy_yr_top20 <- usa_baby_by_yr %>%
        mutate(Name = toupper(Name)) %>%
        filter(Gender == 'M', Name %in% ubn20)

usa_girl_yr_top20 <- usa_baby_by_yr %>% 
        mutate(Name = toupper(Name)) %>%
        filter(Gender == 'F', Name %in% ugn20)

# plotting
usa_boy_yr_top20 <- ggplot(usa_boy_yr_top20, aes(x = Year, y = Count, group = Name)) +
        geom_line(aes(color = Name)) + 
        geom_point(aes(color = Name)) +
        ylab('Numbers') + 
        ggtitle('Popularity Trend of Boy Names in the US in 2011-2015')

usa_girl_yr_top20 <- ggplot(usa_girl_yr_top20, aes(x = Year, y = Count, group = Name)) +
        geom_line(aes(color = Name)) + 
        geom_point(aes(color = Name)) +
        ylab('Numbers') + 
        ggtitle('Popularity Trend of Girl Names in the US in 2011-2015')