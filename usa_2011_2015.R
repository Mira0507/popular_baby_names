library(tidyverse)
library(data.table)

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