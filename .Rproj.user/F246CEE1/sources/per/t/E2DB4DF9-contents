library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# participants column is wrong
race %>% filter(participants > 0) %>% mutate(Year = year(date)) %>% select(Year) %>% unique()
race %>% mutate(Year = year(date)) %>% select(Year) %>% unique()

merge(x = ultra_rankings, y = race, all.x = T) %>%
  mutate(Year = year(date)) %>% 
  count(Year,gender) %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = Year, y = n)) +
  geom_bar(aes(fill = gender, col = gender), position = 'fill', stat = 'identity') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c('#C8E3D4','#630000')) +
  scale_color_manual(values = c('#C8E3D4','#630000')) +
  theme_minimal()