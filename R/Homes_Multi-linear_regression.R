library(corrplot)

Homes <- read.csv('R/Homes.csv')

# 1
pairs(Homes[,c('Price', 'Floor', 'Bath', 'Bed', 'Age', 'Garage')])

# 2
model <- lm(Price ~ Floor + Lot + Bath + Bed + Age + Garage + Active + DEd + DHa + DAd + DCr + DPa
            , data = Homes)

summary(model)

par(mfrow=c(2,2))
plot(model)

# 3
model <- lm(Price ~ Floor + Lot + Bath + Bed + I(Bath*Bed) + Age + I(Age^2) + Garage + Active + DEd + DHa + DAd + DCr + DPa
            , data = Homes)

summary(model)

par(mfrow=c(2,2))
plot(model)

# 4
model <- lm(Price ~ Floor + Lot + Bath + Bed + I(Bath*Bed) + I(Age^2) + Active + DEd + DHa
            , data = Homes)

summary(model)

par(mfrow=c(2,2))
plot(model)

## New Assignment 3
library(caret)
df <- read.csv('R/Homes.csv')

cv <- trainControl(method = 'cv', number = 10, seeds = rep(0, 11))

model1 <- train(Price~Floor+Lot+Bath+Bed+Age+Garage+Active+DEd+DHa+DAd+DCr+DPa,
                data = df,
                method = "lm",
                metric = 'MSE',
                trControl = cv)
model2 <- train(Price~Floor+Lot+Bath+Bed+I(Bath*Bed)+Age+I(Age^2)+Garage+Active+DEd+DHa+DCr+DPa,
                data = df,
                method = "lm",
                trControl = cv)
model3 <- train(Price~Floor+Lot+Bath+Bed+I(Bath*Bed)+I(Age^2)+Active+DEd+DHa,
                data = df,
                method = "lm",
                trControl = cv)
model1.mse <- data.frame(MSE = model1$resample$RMSE^2,
                         Resample = model1$resample$Resample)
model1.mse
mean(model1.mse$MSE)
model2.mse <- data.frame(MSE = model2$resample$RMSE^2,
                         Resample = model2$resample$Resample)
model2.mse
mean(model2.mse$MSE)
model3.mse <- data.frame(MSE = model3$resample$RMSE^2,
                         Resample = model3$resample$Resample)
model3.mse
mean(model3.mse$MSE)
