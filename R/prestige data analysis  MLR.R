#Dataset is in the following library
library(car)
#Type help(Prestige) to access the codebook
#Prestige <-read.table("http://socserv.socsci.mcmaster.ca/jfox/books/Companion/data/Prestige.txt", header=TRUE)
help(Prestige)

#call and view dataframe
data(Prestige) #data function to call dataframe in package
str(Prestige)  #show the details of variables and observation of dataframe
head(Prestige) #show the first 6 rows of dataframe
attach(Prestige)

summary(Prestige)
apply(Prestige[1:4],2,sd)
cor(Prestige[,1:4]) # Correlation of Prestige Dataset on numeric variables
cor.test(education,prestige)
cor.test(income,prestige)
cor.test(women,prestige)

library(corrplot) # Plotting nice correlation matrix
corrplot(cor(Prestige[,1:4]) , method = "number") # Plotting Correlation Matrix


par(mfrow=c(1,2))
hist(prestige,freq=F, col="orange")
lines(density(prestige,adjust=2), col="red", lwd=2)

hist(prestige,freq=F, breaks=15,col="pink")
lines(density(prestige,adjust=2), col="red", lwd=2)
par(mfrow=c(1,1))

#boxplot by type group
boxplot(prestige, col="light green",horizontal = T)
plot(type, prestige,col=c("blue","green","orange"),xlab="type",ylab="prestige")


# Drawing a scatterplot matrix of education, income, women, and prestige using the pairs function
plot(education,prestige,pch=16,cex=1.5,col="blue")
plot(income,prestige,pch=16,cex=1.5,col="blue")
plot(women,prestige,pch=16,cex=1.5,col="blue")

pairs(Prestige[,1:4])
pairs(Prestige[,c(1:3,6,4)])

# Drawing a scatterplot matrix by type groups
pairs(Prestige[,1:4],
      col = c("orange", "green", "blue")[type],   # Change color by group
      pch = c(8, 18, 1)[type])                  # Change points by group)
par(xpd = TRUE)
legend("topleft", fill = c("orange", "green", "blue"), legend = c(levels(type)))

par(mfrow=c(2,1))
plot(income,prestige,pch=16,col="blue")
plot(log2(income),prestige,pch=16,col="blue")
par(mfrow=c(1,1))

library(ggplot2)
# Same plot nicely done with ggplot2
ggplot(Prestige) +
  geom_point(aes(x = education, y = income), col = 'blue', size = 3) +
  ggtitle("Education vs. Income Scatterplot") +
  theme(plot.title = element_text(hjust = 0.5))

library(GGally) 
plot_income <- ggplot(data = Prestige, aes(x = prestige, y = income, col = type)) + geom_point()
plot_education <- ggplot(data = Prestige, aes(x = prestige, y = education, col = type)) + geom_point()
plot_women <- ggplot(data = Prestige, aes(x = prestige, y = women, col = type)) + geom_point()
plot_census <- ggplot(data = Prestige, aes(x = prestige, y = census, col = type)) + geom_point()
plot_grid(plot_income, plot_education, plot_women, plot_census, labels = "AUTO")
plot_ggpairs(Prestige[1:5])
  
#Correlation Matrix on Prestige Dataset
cor(Prestige[,1:4]) # Correlation of Prestige Dataset on numeric variables

library(corrplot)
corrplot(cor(Prestige[,-6]) , method = "number") # Plotting Correlation Matrix

rega <- lm(formula = prestige ~ education + income + women, data = Prestige)
summary(rega)
par(mfrow=c(2,2))
plot(rega)

regb <- lm(formula = prestige ~ education + log2(income) + women, data = Prestige)
par(mfrow=c(2,2))
summary(regb)
plot(regb)

# Dummy variables
Prestige$typeprof <- ifelse(Prestige$type=="prof",1,0)
Prestige$typewc <- ifelse(Prestige$type=="wc",1,0)
Prestige[c(1:3,41:43,90:93),]



# Fitted model for Prestige data with multiple linear regression
reg1 <- lm(formula = prestige ~ education + log2(income) + women + typeprof + typewc, data = Prestige)
summary(reg1)

par(mfrow=c(2,2))  # Split the plotting panel into a 2 x 2 grid
plot(reg1)


residualPlots(reg1)
par(mfrow=c(1,1))  # Return plotting panel to original 1 section


#remove non-significant independent variables 
reg2 <- lm(prestige ~ education + log2(income) + women, data = Prestige)
summary(reg2)
plot(reg2)

#stepwise regression
library(MASS)
step.reg1 <- stepAIC(reg1, direction="both") 
summary(step.reg1) 
step.reg1$anova

#Dummy regression without interactions
reg3 <- lm(prestige ~ education + log2(income) + women + typeprof, data = Prestige)
summary(reg3)
plot(reg3)

#Dummy regression with interactions
reg4 <- lm(prestige ~ education + log2(income) + women + typeprof+typeprof*log2(income),data = Prestige)
summary(reg4)
plot(reg4)

reg5 <- lm(prestige ~ education + log2(income) + poly(women,2) + typeprof+ typeprof*log2(income),data = Prestige)
summary(reg5)
plot(reg5)


install.packages('splines')
library(splines) # for bs (B-Spline Basis for Polynomial Splines)
reg5 <- lm(prestige ~ log(income) + bs(education,df=3) + poly(women,2),data=Prestige)
summary(reg5)
plot(reg5)

#Testing formulticolinearity
vif(reg1)  # vif funcion is in Package 'car'

# 
set.seed(1)
index <- sample(1:nrow(Prestige), nrow(Prestige)*0.8)
train <- Prestige[index,]
test <- Prestige[-index,]
reg.train <- lm(prestige ~ education + log2(income) + poly(women,2) + typeprof +
               typeprof*log2(income),data = Prestige)
summary(reg.train)

plot(reg.train)

# predicted values of prestige variable from test set
prestige.value <- test$prestige
prestige.hat <- predict(reg.train,newdata=test) # Save the predicted values
prestige.test = cbind(prestige.value,prestige.hat)
prestige.test
