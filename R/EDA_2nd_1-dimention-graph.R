library(ggplot2)
library(skimr)
library(lubridate)

summary(mpg)
skim(mpg)

# Bar chart
freq <- table(mpg$manufacturer)
df <- as.data.frame.table(freq)
head(df)

ggplot(df, aes(Var1, Freq)) +
  geom_bar(stat = 'identity', width = 0.5, fill = 'tomato') +
  labs(title = 'title',
       subtitle = 'subtitle',
       caption = 'caption',
       x = 'x', y = 'y') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

# Pie chart
ggplot(mpg, aes(x = '', fill = factor(class))) +
  geom_bar(color = 'black') +
  coord_polar(theta = 'y') +
  labs(fill = 'class',
       title = 'title',
       caption = 'caption') +
  theme_void()

# stem and leaf
stem(mpg$displ)

# histogram
ggplot(mpg, aes(displ)) +
  geom_histogram(binwidth = 0.5, col = 'black', size = .1) +
  labs(title = 'title',
       x = 'x',
       y = 'y')

# dot plot
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, fill = 'blue') +
  labs(title = 'title',
       x = 'x',
       y = 'y') +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

# density plot
ggplot(mpg, aes(cty)) +
  geom_density(fill = 'tomato') +
  labs(title = 'title',
       subtitle = 'subtitle',
       caption = 'caption',
       x = 'x') +
  theme_classic()

# timeseries
brks <- economics$date[seq(1, 90, 12)]
lbls <- lubridate::year(brks)

ggplot(economics[1:90,], aes(date)) +
  geom_line(aes(y = psavert)) +
  labs(title = 'title',
       subtitle = 'subtitle',
       caption = 'caption') +
  scale_x_date(labels = lbls,
               breaks = brks) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor = element_blank())

# box plot
ggplot(mpg, aes(x = '', y = cty)) +
  geom_boxplot(varwidth = TRUE, fill = 'plum') +
  labs(title = 'title',
       subtitle = 'subtitle',
       caption = 'caption',
       x = '') +
  theme_classic()
