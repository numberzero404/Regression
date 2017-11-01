#STARS
stars <- read.table(file="C:/Users/lynsi/Desktop/Stars.txt", header=TRUE)
scatter.smooth(stars$temp, stars$light, pch=19)
cor(stars$light,stars$temp)

starsslr<- lm(light~temp, data=stars)
plot(starsslr$fitted.values, starsslr$residuals, pch=19)
starresiduals <- stdres(starsslr)
abline(0,0)
hist(starresiduals)

logstars<- lm(log(light)~log(temp), data=stars)
scatter.smooth(sqrt(stars$temp), sqrt(stars$light), pch=19)
scatter.smooth(log(stars$temp), log(stars$light), pch=19)

##"What's the relationship?"" This for slope:
confint(starsslr,level=.95)

min((cooks.distance(starsslr)))
stars2 <- stars[-which(cooks.distance(starsslr)>(4/43)),]
scatter.smooth(stars2$temp, stars2$light, pch=19)
stars2slr <- lm(light~temp, data=stars2)
bptest(stars2slr)
#fail to reject, equal variance p=.4577
stars2res <- stdres(stars2slr)

hist(stars2res)
plot(stars2slr$fitted.values, stars2slr$residuals, pch=19)
abline(0,0)

df<-data.frame(temp=c(30000, 40000))
predict.lm(stars2slr, newdata=df, interval="prediction", level=.95)

cor(climate$co,climate$globaltemp)
climatelm <- lm(globaltemp~co2, data=climate)

confint(climatelm,level=.99)
predict.lm(climatelm,newdata=climate,interval="confidence", level=.95)

df <- data.frame(co2=360)

predict.lm(climatelm,newdata=df,interval="confidence", level=.95)
predict.lm(climatelm,newdata=df,interval="prediction",level=.97)

n.cv <- 250
bias <- rep(NA,n.cv)
rpmse <- rep(NA,n.cv)
coverage <- numeric(250)
piw <- numeric(250)
for (i in 1:n.cv)
{
  ##split into test and training sets
  obs.test <- sample(1:nrow(climate),5)
  test.data <- climate[obs.test,]
  training.data <- climate[-obs.test,]
  ##fit model to training data
  my.model <- lm(globaltemp~co2,data=training.data)
  
  ##predict for test data
  test.preds <- predict.lm(my.model,newdata=test.data, interval="prediction")
  ##calculate bias and RMPSE
  bias[i] <- mean ((test.preds-test.data$globaltemp))
  rpmse[i] <- sqrt(mean((test.preds-test.data$globaltemp)^2))
  
  coverage[i] <- mean((test.preds[,2] < test.data$globaltemp) & (test.preds[,3] > test.data$globaltemp))
  piw[i] <- mean(test.preds[,3]-test.preds[,2])
}

mean(bias)
mean(rpmse)
mean(coverage)
mean(piw)


#center model, run again

climate$co2center <- as.numeric(climate$co2-mean(climate$co2))

scatter.smooth(climate$co2center, climate$globaltemp, pch=19, main="CO2 Emissions and Global
               Temperature Deviations from a Reference Point", xlab="Global CO2 Level",
               ylab="Global Temperature Deviation")
cor(climate$co2center,climate$globaltemp)
climatelmcenter <- lm(globaltemp~co2center, data=climate)

confint(climatelmcenter,level=.99)
predict.lm(climatelmcenter,newdata=climate,interval="confidence", level=.95)

df2 <- data.frame(co2center=360)

predict.lm(climatelmcenter,newdata=df2,interval="confidence", level=.95)
predict.lm(climatelmcenter,newdata=df2,interval="prediction",level=.97)

n.cv2 <- 250
bias2 <- rep(NA,n.cv)
rpmse2 <- rep(NA,n.cv)
coverage2 <- numeric(250)
piw2 <- numeric(250)
for (i in 1:n.cv)
{
  ##split into test and training sets
  obs.test2 <- sample(1:nrow(climate),5)
  test.data2 <- climate[obs.test,]
  training.data2 <- climate[-obs.test,]
  ##fit model to training data
  my.model2 <- lm(globaltemp~co2center,data=training.data2)
  
  ##predict for test data
  test.preds2 <- predict.lm(my.model2,newdata=test.data2, interval="prediction")
  ##calculate bias and RMPSE
  bias2[i] <- mean ((test.preds2-test.data2$globaltemp))
  rpmse2[i] <- sqrt(mean((test.preds2-test.data2$globaltemp)^2))
  
  coverage2[i] <- mean((test.preds2[,2] < test.data2$globaltemp) & (test.preds2[,3] > test.data2$globaltemp))
  piw2[i] <- mean(test.preds2[,3]-test.preds2[,2])
}

mean(bias2)
mean(rpmse2)
mean(coverage2)
mean(piw2)
