Homes <- read.csv('R/Homes.csv')

#plot(Homes$Floor, Homes$Price)

# Center
Homes_center <- data.frame(Price = Homes$Price,
                           Floor_center = scale(Homes$Floor, scale = FALSE))
model_center <- lm(Price ~ Floor_center, data = Homes_center)
summary(model_center)

variance <- anova(model_center); variance
sum(variance$`Sum Sq`)

par(mfrow=c(2, 2))
plot(model_center)
