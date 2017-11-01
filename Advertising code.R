advertising <- read.csv(file ="C:/Users/lynsi/Downloads/Advertising (1).csv")
scatter.smooth(advertising$P, advertising$R, pch=19)
cor(advertising$P, advertising$R)
slr <- lm(P~R, data=advertising)
library(lmtest)
bptest(slr)
library(MASS)
summary(slr)
residuals <- stdres(slr)
#plots fitted values vs residuals
plot(slr$fitted.values, slr$residuals, pch=19)
plot(residuals)
hist(residuals)
cooks.distance(slr)
which(cooks.distance(slr)>3)
ks.test(residuals,"pnorm")
logslr <- lm(log(R)~log(P), data=advertising)
summary(logslr)
plot(log(P)~log(R), data=advertising)
bptest(logslr)
logres<-stdres(logslr)
ks.test(logres, "pnorm")
hist(logres)
plot(logslr$fitted.values, logslr$residuals, pch=19)

plot(advertising$P, advertising$R, pch=19)
numbers <- seq(0,25, len=100)
df <- data.frame(P=numbers)
predictions <- exp(predict.lm(logslr, newdata=df))
lines(numbers, predictions)

#9
n.cv <- 100
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
for (i in 1:n.cv)
{
  ##split into test and training sets
  obs.test <- sample(1:nrow(advertising),4)
  test.data <- advertising[obs.test,]
  training.data <- advertising[-obs.test,]
  ##fit model to training data
  my.model <- lm(log(R)~log(P),data=training.data)
  
  ##predict for test data
  test.preds <- exp(predict.lm(my.model,newdata=test.data))
  ##calculate bias and RMPSE
  bias[i] <- mean ((test.preds-test.data$R))
  rpmse[i] <- sqrt(mean((test.preds-test.data$R)^2))
}
bias
mean(bias)
