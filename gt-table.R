
# loading libraries -------------------------------------------------------

library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)
library(kableExtra)

# reading data ------------------------------------------------------------

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')


# gt-table process start --------------------------------------------------

# creating plot table (needed for adding custom ggplot)
plot_object = ratings %>% 
  filter(year>0) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(plot = map(data,~ggplot(.,aes(average)) + 
                      geom_boxplot() + 
                      theme_minimal() +
                      theme(legend.position = 'none',
                            axis.title.x = element_blank()))) %>% 
  select(-data)

# creating df with selected years
Years = ratings %>% 
  filter(year>0) %>% 
  group_by(year) %>% 
  summarise(Avg_Rating = mean(average),
            Max_Rating = max(average),
            N_Games = n()) %>% 
  filter(N_Games > 50) %>% 
  arrange(desc(Avg_Rating,Max_Rating))

# creating final plot table with selected years
plot_object = left_join(Years,plot_object) %>% 
  select(year,plot)

# creating the table
ratings %>% 
  filter(year>0) %>% 
  group_by(year) %>% 
  summarise(Avg_Rating = mean(average),
            Max_Rating = max(average),
            N_Games = n(),
            Highest_Rated_Game = paste('[',name[average==Max_Rating][1],']',sep=''),
            Highest_Rated_Game_Image = thumbnail[average==Max_Rating][1],
            Highest_Rated_Game_URL = paste('(',paste('https://boardgamegeek.com',url[average==Max_Rating][1],sep = ''),')',sep=''),
            trend = list(average),
            user_ratings = '') %>% 
  filter(N_Games > 50) %>% 
  arrange(desc(Avg_Rating,Max_Rating)) %>% 
  mutate(Highest_Rated_Game = paste0(Highest_Rated_Game,Highest_Rated_Game_URL),
         Highest_Rated_Game = map(Highest_Rated_Game,gt::md)) %>% 
  select(-Highest_Rated_Game_URL) %>% 
  gt() %>% 
  tab_header(title = html("<span style='font-family: Japan1; color:red'>Board Game Ratings</span>"),
             subtitle = md('*Years containing at least 50 games*')) %>% 
  text_transform(locations = cells_body(columns = Highest_Rated_Game_Image),
                 fn = function(x){
                   web_image(url = x,height = 50)
                 }) %>% 
  cols_align(align = 'center', columns = c(-year)) %>% 
  cols_align(align = 'left', columns = Highest_Rated_Game_Image) %>% 
  cols_align(align = 'right', columns = Highest_Rated_Game) %>% 
  tab_options(data_row.padding = px(2)) %>% 
  cols_label(Highest_Rated_Game_Image=md(''),
             year=md('Year'),
             Avg_Rating=md('Average Rating'),
             Max_Rating=md('Maximum Rating'),
             N_Games=md('New Board Games'),
             Highest_Rated_Game=md('Game of Year?'),
             trend=md('Rating Distribution'),
             user_ratings=md('User Rating Distribution')) %>%
  fmt_number(columns = Avg_Rating) %>% 
  tab_source_note(source_note = md('Source: [TidyTuesday Data](https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-25)')) %>% 
  tab_footnote(footnote = 'Pandemic years',
               locations = cells_body(columns = year,
                                      rows = year>=2020)) %>% 
  tab_footnote(footnote = 'As of January 25 Tidy Tuesday Data',
               locations = cells_body(columns = N_Games,
                                      rows = year==2022)) %>% 
  tab_options(heading.background.color = '#041562',
              heading.align = 'left',
              column_labels.background.color = '#EEEEEE') %>% 
  gt_theme_espn() %>% 
  gt_plt_dist(column = trend) %>% 
  text_transform(locations = cells_body(columns = user_ratings),
                 fn = function(x){
                   map(plot_object$plot,ggplot_image,height=px(100))
                 })

# another gt table using different plot technique
details %>% 
  # select(id,boardgamecategory) %>% 
  mutate(category = str_remove_all(boardgamecategory,"'"),
         category = str_remove_all(category,"\\["),
         category = str_remove_all(category,"\\]"),
         category = str_remove_all(category,"\""),
         category = str_split(category,",")) %>% 
  unnest(cols = c(category)) %>% 
  mutate(category = gsub(" ","",category)) %>% 
  left_join(ratings,by='id') %>% 
  group_by(category) %>% 
  summarise(count = n(),
            ratings = list(bayes_average)) %>% 
  filter(count > 500) %>% 
  gt() %>% 
  # pluck('_data','ratings') %>% 
  # str(max.level=1)
  text_transform(locations = cells_body(columns = ratings),
                 fn = function(x){
                   plot <- map(pluck(.,'_data','ratings'),spec_boxplot,same_lim=T,lim=c(0,10),width = 300, height = 100)
                   plot_svg <- map(plot,"svg_text")
                   map(plot_svg,html)
                   # str(x)
                 })

# refer https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/ to get insight on the function

gt_plot = function(x){
  
}
