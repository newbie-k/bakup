library(tidyverse)
library(carData)


# 1
df <- tibble(drop_na(TitanicSurvival))
# 2
str(drop_na(TitanicSurvival))
summarise(group_by(df, survived, sex), mean_age = mean(age))
# 3
summarise(group_by(df, survived, sex), median_age = median(age))
# 4
summarise(group_by(df, survived, sex), max_age = max(age))