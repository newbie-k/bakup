library(carData)
library(ggplot2)
library(tidyverse)

df <- as_tibble(TitanicSurvival)

# 1
only_dead_df <- df[df$survived == 'no',]

ggplot(only_dead_df, aes(passengerClass, fill=sex)) +
  geom_bar() +
  geom_text(aes(label=after_stat(count)), stat = 'count', position = 'stack', vjust = 0) +
  labs(title = 'Dead count groub by sex & class',
       subtitle = 'bar plot with 2 varible',
       caption = 'TitanicSurvival in carData',
       x = 'Class', y = 'count') +
  theme_classic()

# 2
only_mt_15_df <- drop_na(df[df$age >= 15,])

ggplot(only_mt_15_df, aes(passengerClass, fill=sex)) +
  geom_bar() +
  geom_text(aes(label=after_stat(count)), stat = 'count', position = 'stack', vjust = 0) +
  labs(title = 'Passenger with age>=15 count groub by sex & class',
       subtitle = 'bar plot with 2 varible',
       caption = 'TitanicSurvival in carData',
       x = 'Class', y = 'count') +
  theme_classic()
