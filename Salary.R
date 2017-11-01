salary <- data.frame(read.table(file="C:/Users/lynsi/Desktop/Stat 330/Salary.txt", header=TRUE))
salary$Education <- factor(salary$Education, levels=c("HS","BS","BS+"))

boxplot(Salary~Education, data=salary, main="Education and Salary", xlab="Education Level", ylab="Salary")
boxplot(Salary~Manager, data=salary, main="Managers and Salary", xlab="Manager Position?", ylab="Salary")

#this one gives all three plots with just this command :D
library(GGally)
plot(Salary~Experience+Education+Manager, data=salary)

##crazy plot matrix
ggpairs(salary)

boxplot(Salary~Education + Manager, data=salary, main="Education and Salary", ylab="Salary")

#create interaction variable
edxman <- interaction(salary$Education,salary$Salary)

my.colors <- rainbow(nlevels(edxman))
plot(salary$Experience, salary$Salary, col=my.colors[edxman])
legend("topright",legend=c("String1","String2",...),
         col=c("color1","color2",...),lty=1,lwd=1)

##using ggplot
library(ggplot2)
ggplot(salary,aes(y=Salary,x=Experience,color=Education:Manager)) + geom_point()

sallm <- lm(Salary~Experience+Manager+Education+(Manager:Education), data=salary)

## to evaluate assumptions, use an avplot, and the same other plots (hist, etc)

