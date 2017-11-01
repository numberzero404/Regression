supervisor <- data.frame(read.table(file="C:/Users/lynsi/Desktop/Stat 330/Supervisor.txt", header=TRUE))

pairs(supervisor)                      
cor(supervisor)
superlm <- lm(Rating~.,data=supervisor)
avPlots(superlm)
pred <- data.frame(Complaints=65, Privileges=51.5, Learn=56.5, Raises=63.5, Critical=77.5, Advance=45)
predict.lm(superlm, pred)

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
  obs.test2 <- sample(1:nrow(supervisor),3)
  test.data2 <- supervisor[obs.test2,]
  training.data2 <- supervisor[-obs.test2,]
  ##fit model to training data
  my.model2 <- lm(Rating~.,data=training.data2)
  
  ##predict for test data
  test.preds2 <- predict.lm(my.model2,newdata=test.data2, interval="prediction")
  ##calculate bias and RMPSE
  bias2[i] <- mean((test.preds2-test.data2$Rating))
  rpmse2[i] <- sqrt(mean((test.preds2-test.data2$Rating)^2))
  
  #coverage2[i] <- mean((test.preds2[,2] < test.data2$globaltemp) & (test.preds2[,3] > test.data2$globaltemp))
  #piw2[i] <- mean(test.preds2[,3]-test.preds2[,2])
}

mean(bias2)
mean(rpmse2)
mean(coverage2)
mean(piw2)