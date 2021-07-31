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
