#read in data
Stop_Dist <- read.table("C:/Users/lynsi/Desktop/Stat 330/StoppingDistance.txt", header=TRUE)

#draw scatterplot and asses L-I-N-E assumptions
scatter.smooth(Stop_Dist$Speed, Stop_Dist$Distance , pch=19, col="hot pink", xlab="Speed", ylab="Distance", main="Distance Required to Stop")
cor(Stop_Dist$Speed, Stop_Dist$Distance)
cov(Stop_Dist$Speed, Stop_Dist$Distance)

#fit model to data
slr <- lm(Distance~Speed, data=Stop_Dist) 
summary(slr)
abline(a=-20.131,b=3.142,col="red",lwd=3,lty=1)

library(lmtest)
bptest(slr)

#Equal Variance, Normal, Independent
plot(slr$fitted.values, slr$residuals, pch=19, col="blue", ylab="Residuals", xlab="Fitted Values", main="Residuals vs. Fitted Values")
abline(a=0, b=0, lty=2)

library(MASS)

#Normality
hist(stdres(slr), xlab="Residuals", main="Residual Frequency")
stdres(slr)

#Outliers
cooks.distance(slr)
which(cooks.distance(slr)>2)
ks.test(stdres(slr),"pnorm")

##Transformation
scatter.smooth(sqrt(Stop_Dist$Speed), sqrt(Stop_Dist$Distance) , pch=19, col="hot pink", xlab="Speed", ylab="Distance", main="Distance Required to Stop")
cor(sqrt(Stop_Dist$Speed), sqrt(Stop_Dist$Distance))

sqrt_slr <- lm(sqrt(Distance)~sqrt(Speed), data=Stop_Dist) 
summary(sqrt_slr)

bptest(sqrt_slr)
ks.test(stdres(sqrt_slr), "pnorm")

abline(a=-3.11737,b=2.10697,col="red",lwd=3,lty=1)

plot(sqrt_slr$fitted.values, sqrt_slr$residuals, pch=19, col="blue", ylab="Residuals", xlab="Fitted Values", main="Residuals vs. Fitted Values")
abline(a=0, b=0, lty=2)

#Normality
hist(stdres(sqrt_slr), xlab="Residuals", main="Residual Frequency")
stdres(sqrt_slr)


n.cv <- 50
bias <- rep(NA, n.cv)
rpmse <- rep(NA, n.cv)

for(i in 1:n.cv){
  #Step 1- split into test and training sets
  
  obs.test <- sample(1:62,6)
  test.data <- Stop_Dist[obs.test,]
  train.data <- Stop_Dist[-obs.test,]
  
  #Step 2- fit model to trianing data
  
  my.model <- lm(sqrt(Distance)~sqrt(Speed), data=train.data)
  
  #Step 3-Predict for test data
  
  test.preds <- (predict.lm(my.model, newdata=test.data))^2
  
  #Step 4-Calculate bias and RPMSE
  bias[i] <- mean((test.preds-(test.data$Distance)))
  rpmse[i] <- sqrt(mean((test.preds-(test.data$Distance))^2))
}


mean(bias)
mean(rpmse)


#Provide plot with fitted regression line projected onto original data.
x <- seq(0,150, length=100)
df <- data.frame(Speed=x)
y <- ((predict.lm(sqrt_slr, newdata=df))^2)
plot(Stop_Dist$Speed, Stop_Dist$Distance, pch=19, col="blue",xlab="Speed", ylab="Distance", main="Distance Required to Stop")
lines(x,y, col="red")
