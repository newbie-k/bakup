x <- c(1, 2, NA, 4)
is.na(x)
which(is.na(x))

a[1,2] <- NA
a

df <- data.frame(col1 = c(1:3, NA),
                 col2 = c('this', NA, 'is', 'text'),
                 col3 = c(TRUE, FALSE, TRUE, TRUE),
                 col4 = c(2.5, 4.2, 3.2, NA),
                 stringsAsFactors = FALSE)
df
is.na(df)

is.na(df$col4)

colSums(is.na(df))

x <- c(1:4, NA, 6:7, NA)
x
mean(x, na.rm = TRUE)

complete.cases(df)  # check non-na rows
df[complete.cases(df),]
df[!complete.cases(df),]

na.omit(df)  # remove na

summary(cars)  # Min. 1stQu. Median. Mean. 3rdQu. Max.
boxplot(cars)

lowbound <- quantile(cars$speed, 0.025)  # 2.5 percentile
lowbound
upbound <- quantile(cars$speed, 0.975)  # 97.5 percentile
upbound
out_idx <- which(cars$speed < lowbound | cars$speed > upbound)
cars[out_idx, 'speed']


## Practice
df <- read.table('Downloads/auto-mpg-test.txt')
str(df)

boxplot(df$V1)$out

hist(df$V1,
     xlab = 'mile per gallon',
     main = 'Cylinder Histogram',
     breaks = sqrt(nrow(df)))

low.bound <- quantile(df$V1, 0.025)
upp.bound <- quantile(df$V1, 0.975)
out_idx <- which(df$V1 < low.bound | df$V1 > upp.bound)
out_idx
