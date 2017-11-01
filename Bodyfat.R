#Read in the data
bodyfat <- (read.table("BodyFat.txt", header=TRUE))
#Create a scatterplot for each characteristic
for(i in 2:ncol(bodyfat)){
  plot(bodyfat$brozek, bodyfat[,i], pch=19, xlab=names(bodyfat)[i], ylab="Body Fat")}
#Covariance
cor(bodyfat)
#fit model to data
slr <- lm(brozek ~ .,data=bodyfat) 
summary(slr)
#Evaluate assumptions
library(car)
avPlots(slr)
plot(slr$fitted.values, slr$residuals, pch=19, col="blue", ylab="Residuals", xlab="Fitted Values", main="Residuals vs. Fitted Values")
abline(a=0, b=0, lty=2)
library(MASS)
stdres(slr)
hist(stdres(slr), xlab="Standardized Residuals", main="Frequency of Residuals")
cooks.distance(slr)
which(cooks.distance(slr)>2)
ks.test(stdres(slr),"pnorm")
#Prediction Interval
df <- data.frame(age=50, weight=203, height=67, neck=40.2, chest=114.8, abdom=108.1, hip=102.5, thigh=61.3, knee=41.1, ankle=24.7, biceps=34.1, forearm=31, wrist=18.3)
predict.lm(slr, newdata=df, interval="prediction")
#Cross Validation
coverage <- numeric(250)
piw <- numeric(250)
n.cv <- 250
bias <- rep(NA, n.cv)
rpmse <- rep(NA, n.cv)
for(i in 1:n.cv){
  obs.test <- sample(1:nrow(bodyfat),25)
  test.data <- bodyfat[obs.test,]
  train.data <- bodyfat[-obs.test,]  
  my.model <- lm(brozek ~ ., data=train.data)
  test.preds <- predict.lm(my.model, newdata=test.data, interval="prediction")
  bias[i] <- mean((test.preds[,1]-test.data$brozek))
  rpmse[i] <- sqrt(mean((test.preds[,1]-test.data$brozek)^2))
  coverage[i] <- mean((test.preds[,2] < test.data$brozek) & (test.preds[,3] > test.data$brozek))
  piw[i] <- mean(test.preds[,3]-test.preds[,2])}
mean(bias)
mean(rpmse)
mean(coverage)
mean(piw)
