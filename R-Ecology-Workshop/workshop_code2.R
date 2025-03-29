library(tidyverse)
library(lubridate)
surveys <- read_csv(file = "data/cleaned/surveys_complete_77_89.csv")

select(surveys,plot_id,species_id,hindfoot_length)

select(surveys, -record_id, -year)
view(surveys)
select(surveys, c(3:5,10))

select(surveys, where(is.numeric)) 

select(surveys, where(anyNA))

filter(surveys,year == 1985)

filter(surveys, species_id %in% c("RM","DO"))

filter(surveys, year <= 1988 & !is.na(hindfoot_length))

filter(surveys, year >= 1980 & year <= 1985)

surveys_selected <- select(surveys, year, month, species_id, plot_id)

filter(select(surveys, -day),month >= 7)

surveys_noday <- select(surveys, -day)
filter(surveys_noday, month >= 7)

surveys_sub <- surveys %>% 
  select(-day) %>% 
  filter(month >= 7)

surveys_sub <- surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, month, species_id)

surveys %>% 
  mutate(weight_kg = weight / 1000)

surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lbs = weight_kg * 2.2)

surveys %>% 
  mutate(date = paste(year,month, day, sep = "-"))

surveys %>% mutate(date = paste(year, month, day, sep = "-")) %>% 
  relocate(date, .after = year)

surveys %>% 
  mutate( date  = paste(year,month,day,sep = "-"),
          date = ymd(date)) %>% 
  relocate(date, .after = year)

surveys %>%
  mutate(date = ymd(paste(year,month,day,sep = "-"))) %>% 
  relocate(date, .after = year)

date_surveys <- surveys %>% 
  mutate(date = paste(year,month,day, sep = "-"),
         date = ymd(date)) %>% 
  relocate(date, .after = year)

ggplot(data = date_surveys, mapping = aes(x = date,y = weight)) + geom_point(alpha = 0.2)

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight,na.rm = T))

surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight,na.rm = T), n = n())

surveys %>% 
  group_by(species_id,sex) %>% 
  summarize(mean_weight = mean(weight, na.rm =T),
            n = n())

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id,sex) %>% 
  summarize(mean_weight = mean(weight),
            n = n())

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarize (mean_weight = mean(weight),
             n = n()) %>% 
  arrange(mean_weight)

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id,sex) %>% 
  summarize (mean_weight = mean(weight),
             m = n()) %>% 
  arrange(desc(mean_weight))

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  summarize(mean_weight = mean(weight),
            n = n()) %>% 
  arrange(desc(mean_weight)) %>% 
  ungroup()

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id, sex) %>% 
  mutate(mean_weight = mean(weight),
          weight_diff = weight - mean_weight) %>% 
  select(species_id,sex,contains("weight"))

caught_day <- surveys %>% 
  mutate(data = surveys, date = ymd(paste(year,month,day,sep = "-"))) %>% 
  group_by(date,sex) %>% 
  summarize(n = n())

ggplot(data = caught_day,mapping = aes(x = date, y = n, colour = sex)) + geom_line()

sp_by_plot <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(species_id,plot_id) %>% 
  summarise(mean_weight = mean(weight)) 