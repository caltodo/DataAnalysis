#HW 8
library(Sleuth3)
library(ggplot2)

aquifer

#1 plot

hist(aquifer$resist)
hist(aquifer$yield)
qplot(resist,yield, data=aquifer)
#resist as independent var, yield is dependent

#Regression - vars need to switch for lm!!
aq.lm <- lm(yield~resist, data=aquifer)
aq2.lm <- lm(resist~yield, data=aquifer)
summary(aq.lm) 

ggplot(aquifer, aes(x=resist, y=yield)) +   geom_point() +   geom_smooth(method=lm,se=FALSE)

pred.intervals<-data.frame(predict(aq2.lm,interval="prediction"))
head(pred.intervals) 
aq2.df2 <- cbind(aquifer, pred.intervals) #new data frame with bound aquifer with pred.int vars (slapped together)
head(aq.df2)

#the answer
ggplot(aq.df2, aes(x=resist, y=yield)) + geom_point() + geom_line(aes(y=lwr), color = "blue", linetype = "dashed") +geom_line(aes(y=upr), color = "blue", linetype = "dashed") +geom_smooth(method=lm)

ggplot(aq2.df2, aes(x=resist, y=yield)) + geom_point() + geom_line(aes(y=lwr), color = "blue", linetype = "dashed") +geom_line(aes(y=upr), color = "blue", linetype = "dashed") +geom_smooth(method=lm)


#2 -> put into table with all the data from summary
summary(aq.lm)
#Y = -2.53 + .033X


#3 -> conf int for res = 150
predict(aq.lm,data.frame(resist=150),interval="confidence")
#conf int = (2.2739, 2.7279) centered at 2.5

#4 -> normality of scatter from part 1
#the data looks to be Right skewed, which would imply it
#is not normal

# --> residuals
head(aq2.lm$residuals)
plot(aq2.lm,which=1) #the which is weird, don't worry

summary(aq2.lm)
#5 -> independence assumption

help(lm)
