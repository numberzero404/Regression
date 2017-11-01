#Read in MPG data
mpg <- read.table(file="C:/Users/lynsi/Desktop/MPGData.txt",header=TRUE)
names(mpg)
mpg$MPG
head(mpg)
mpg$MPG[1]
dim(mpg)
plot(mpg$Weight, mpg$MPG, pch=19, col="purple",xlab="weight", ylab="mpg", main="plot title")
scatter.smooth(mpg$Weight, mpg$MPG, pch=19, col="purple")
cor(mpg$Weight,mpg$MPG)
with(mpg,{...})
slr <- lm(MPG~Weight, data=mpg)
names(slr)
summary(slr)
coef(slr)
abline(a=51.587,b=-0.0098,col="blue",lwd=1,lty=1)
coef(slr)
lmsum <- summary(slr)
new.mpg <- data.frame(Weight=c(3000,20000))
predict.lm(slr, newdata=new.mpg)

## on number 2 of hw 1, draw a scatter plot, if linear, slr
## numerical summaries, report covariance, correlation if it's linear, really explain why
## SLR is apppropriate
## specify what all variables in the model MEAN
## fit your model==get estimates of slope, intercepts, etc
##6 don't do it
