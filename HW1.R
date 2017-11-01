#Read in windmill data:
windmills <- read.table(file="C:/Users/lynsi/Desktop/Stat 330/wmills.txt", header=TRUE)
#Plot data as a scatter plot with a curved line through it
scatter.smooth(windmills$RSpd, windmills$CSpd, pch=1, xlab="Wind Speed at Reference Site (m/s)", ylab="Wind Speed at Candidate Site (m/s)")
#Find correlation of data
cor(windmills$CSpd, windmills$RSpd)
#Find covariance of data
cov(windmills$CSpd, windmills$RSpd)
#Fit simple linear regression model (find intercept and slope)
lm(CSpd~RSpd, data=windmills)
#Plot with regression line
plot(windmills$RSpd, windmills$CSpd, pch=1, xlab="Wind Speed at Reference Site (m/s)", ylab="Wind Speed at Candidate Site (m/s)")
abline(a=3.1412,b=.7557,lwd=1,lty=1) #values from fitted linear model

