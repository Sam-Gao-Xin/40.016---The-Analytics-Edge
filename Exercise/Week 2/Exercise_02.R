library(ggplot2)
#Qs1
auto <-read.csv("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Exercise/Week 2/Auto(3).csv")
str(auto)
head(auto)

#Qs1(a)
auto$horsepower <- as.numeric(as.character(auto$horsepower)) #Since horsepower data type is character therefore we need to convert
model1 <- lm(mpg~horsepower, data=auto)
summary(model1)

#Qs1(b)
# There is a strong relationship 
# The relationship is inverse

#Qs1(c)
predict.lm(model1,newdata=data.frame(horsepower=98),interval=c("confidence"),level=.99)

#Qs1(d)
cor(auto$mpg,auto$horsepower, use = "pairwise.complete.obs")
cor(auto$mpg,auto$horsepower, use = "pairwise.complete.obs")^2

#Qs1(e)
library(ggplot2)
ggplot(auto,aes(horsepower,mpg))+geom_point(na.rm=T)+geom_smooth(method="lm",na.rm=T,se=F)

#Qs1(f)
library(ggfortify)
autoplot(model1,label.size = 3)


#Qs2
library(psych)
#Qs2(a)
pairs.panels(auto,ellipses = F, lm =T, breaks=10, hist.col="blue")
str(auto)

#Qs2(b)
auto1<-subset(auto,select=-c(name))
str(auto1)
cor(auto1)

#Qs2(c)
model2 <-lm(mpg~., data=auto1)
summary(model2)
how