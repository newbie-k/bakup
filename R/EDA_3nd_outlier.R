library(ggplot2)

# percentiles
low_bound <- quantile(mpg$hwy, 0.025)
up_bound  <- quantile(mpg$hwy, 0.975)

out <- which(mpg$hwy < low_bound
             | mpg$hwy > up_bound)
mpg$hwy[out]
mpg[out,]

# Hampel filter
low_bound <- median(mpg$hwy) - 3 * mad(mpg$hwy)
up_bound  <- median(mpg$hwy) + 3 * mad(mpg$hwy)

out <- which(mpg$hwy < low_bound
             | mpg$hwy > up_bound)
mpg$hwy[out]
mpg[out,]

# boxplot
ggplot(mpg, aes(hwy)) +
  geom_boxplot()

# Grubbs's test
library(outliers)
grubbs.test(mpg$hwy)  # highest
grubbs.test(mpg$hwy, opposite = T)  # lowest

grubbs.test(mpg$hwy, type = 20)  # 2 highest
grubbs.test(mpg$hwy, type = 20, opposite = T)  # 2 lowest
grubbs.test(mpg$hwy, type = 11)  # 1 highest & 1 lowest
grubbs.test(mpg$hwy, type = 11, opposite = T)  # same as above

# Dixon's test
dat2 <- mpg$hwy[1:20]

dixon.test(dat2)  # lowest
dixon.test(dat2, opposite = T)  # highest


# remove outlier
#which.min(dat2)  # index of min


# Rosner's test
library(EnvStats)

rosnerTest(mpg$hwy)
