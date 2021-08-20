df <- read.csv('R/Homes.csv')

library(ggplot2)

ggplot(df, aes(Floor, Price, col=as.factor(Active))) +
  geom_point()
ggplot(df, aes(y=Price, fill=as.factor(Active))) +
  geom_boxplot()
ggplot(df, aes(y=Floor, fill=as.factor(Active))) +
  geom_boxplot()

log1.2 <- glm(Active~Price+Floor+DEd+DCr+DPa, data = df, family = binomial)
summary(log1.2)

pred <- ifelse(predict(log1.2, df) > 0.5, 1, 0)
true <- df$Active
table(true, pred)


library(caret)

Active <- ifelse(df$Active, 'Active', 'Sold')
df.new <- df
df.new$Active <- factor(Active, levels = c('Sold', 'Active'))

cv <- trainControl(method = 'cv', number = 5, seeds = rep(0, 6))

log1.cv5 <- train(Active~Price+Floor+DEd+DCr+DPa,
                  data = df.new,
                  method = "glm",
                  family = "binomial",
                  trControl = cv)
log2.cv5 <- train(Active~Price+Floor+I(Floor^2)+DEd,
                  data = df.new,
                  method = "glm",
                  family = "binomial",
                  trControl = cv)
log1.cv5.err <- data.frame(ERR = 1-log1.cv5$resample$Accuracy,
                           Resample = log1.cv5$resample$Resample)
log1.cv5.err
mean(log1.cv5.err$ERR)
log2.cv5.err <- data.frame(ERR = 1-log2.cv5$resample$Accuracy,
                           Resample = log2.cv5$resample$Resample)
log2.cv5.err
mean(log2.cv5.err$ERR)

summary(log1.cv5)
summary(log2.cv5)
