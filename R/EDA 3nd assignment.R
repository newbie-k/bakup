df <- mtcars

## 1
hist(df$mpg)
boxplot(df$mpg)

# Hampel
low_bound <- median(df$mpg) - 3 * mad(df$mpg)
up_bound  <- median(df$mpg) + 3 * mad(df$mpg)

out <- which(df$mpg < low_bound
             | df$mpg > up_bound)
df$mpg[out]

## 2
df['Lincoln Continental', 'disp']
max(df$disp)

library(outliers)
grubbs.test(df$disp, type = 20)

dixon.test(df$disp, opposite = T)

library(EnvStats)
rosnerTest(df$disp)
rosnerTest(df$disp)

## 3
df['Ford Pantera L', 'hp']

grubbs.test(df$hp, type = 20)

dixon.test(df$hp, opposite = T)

rosnerTest(df$hp)

## 4
grubbs.test(df$wt, type = 20, opposite = T)

rosnerTest(df$wt)
