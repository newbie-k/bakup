library(ISLR)
str(Default)

attach(Default)
plot(balance, as.integer(default)-1, col='salmon')
plot(balance, income, col=default)

# GGplot
library(ggplot2)

ggplot(Default, aes(balance, income, col=default)) +
  geom_point()

ggplot(Default, aes(balance, income, fill=default)) +
  geom_boxplot()

log.balance <- glm(default~balance, data=Default, family = binomial)
summary(log.balance)

log.student <- glm(default~student, data = Default, family = binomial)
summary(log.student)

log.3 <- glm(default~balance+income+student, data = Default, family = binomial)
summary(log.3)

log.4 <- glm(default~balance+student, data = Default, family = binomial)
summary(log.4)
log.4$coefficients

prob <- predict(log.4, type = "response")
pred <- rep('No', 10000)
pred[prob > 0.5] <- 'Yes'
cf <- table(pred, default)
cf

# Accuracy
mean(pred == default)
# Precision
prec <- cf[2,2] / (cf[1,2] + cf[2,2])
prec
# Recall
recall <- cf[2,2] / (cf[2,1] + cf[2,2])
recall
