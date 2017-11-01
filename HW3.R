water <- read.table("C:/Users/lynsi/Desktop/Stat 330/water.txt", header=TRUE)

#draw scatterplot and asses L-I-N-E assumptions
scatter.smooth(water$Precip, water$Runoff , pch=19, col="royal blue", xlab="Precipitation in Inches", ylab="Runoff in Acre-Feet", main="Precipitation Effect on Runoff")
cor(water$Precip, water$Runoff)
cov(water$Precip, water$Runoff)
slr <- lm(Runoff~Precip, data=water) 

#Equal Variance, Normal, Independent
library(lmtest)
bptest(slr)
plot(slr$fitted.values, slr$residuals, pch=19, col="blue", ylab="Residuals", xlab="Fitted Values", main="Residuals vs. Fitted Values")
abline(a=0, b=0, lty=2)

#Normality
library(MASS)
hist(stdres(slr), main="Histogram of Residuals", xlab="Residuals")
stdres(slr)

#Outliers
cooks.distance(slr)
which(cooks.distance(slr)>5)
ks.test(stdres(slr),"pnorm")

#fit model to data
scatter.smooth(water$Precip, water$Runoff , pch=19, col="royal blue", xlab="Precipitation in Inches", ylab="Runoff in Acre-Feet", main="Precipitation Effect on Runoff")
summary(slr)
abline(a=27014.6,b=3752.5,col="red",lwd=3,lty=1)

##Cross Validation
n.cv <- 50
bias <- rep(NA, n.cv)
rpmse <- rep(NA, n.cv)

for(i in 1:n.cv){
  #Step 1- split into test and training sets
  
  obs.test <- sample(1:43,5)
  test.data <- water[obs.test,]
  train.data <- water[-obs.test,]
  
  #Step 2- fit model to training data
  
  my.model <- lm(Runoff~Precip, data=train.data)
  
  #Step 3-Predict for test data
  
  test.preds <- (predict.lm(my.model, newdata=test.data))
  
  #Step 4-Calculate bias and RPMSE
  bias[i] <- mean((test.preds-(test.data$Runoff)))
  rpmse[i] <- sqrt(mean((test.preds-(test.data$Runoff))^2))
}

mean(bias)
mean(rpmse)


#Provide plot with fitted regression line projected onto original data.
x <- seq(0,150, length=100)
df <- data.frame(Precip=x)
y <- ((predict.lm(slr, newdata=df))^2)
plot(water$Precip, water$Runoff, pch=19, col="blue",xlab="Precipitation", ylab="Runoff", main="Precipitation Affect on Runoff")
lines(x,y, col="red")

#Confidence Level
confint(slr,level=0.95)
df <- data.frame(Precip=4.5)
predict.lm(slr,newdata=df,interval="confidence",level=0.95)
predict.lm(slr,newdata=df,interval="prediction",level=0.95)
predict.lm(slr,newdata=df,interval="prediction",level=0.95)

