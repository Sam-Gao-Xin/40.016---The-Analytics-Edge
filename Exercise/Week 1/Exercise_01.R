library(ggplot2)
x <- c(4,2,6)
y <- c(1,0,-1)

#Qs1
length(x)
sum(x)
sum(x^2)
x+y
x*y
x-2
x^2
(x*y)[1:2]

#Qs2
7:11
seq(2,9)
seq(4,10,by=2)
seq(3,30,length=10)
seq(6,-4,by=-2)

#Qs3
rep(2,4)
rep(c(1,5),4)
rep(c(1,2),c(4,4))

#Qs4
x <- c(5,9,2,3,4,6,7,0,8,12,2,9)
x[2]
x[2:4]
x[c(2,3,6)]
x[c(1:5,10:12)]
x[-(10:12)]

#Qs5
x <- array(c(3, -1,2,-1), dim=c(2,2))
y <- array(c(1,0,4,1,0,-1), dim=c(2,3))
x
2*x
x*x #Element-wise multiplication
x%*%x #Matrix Multiplication
x%*%y
t(x) #Matrix Transpose
solve(x) #Solve System of Equations

x[,]

#Qs6
df <- read.csv("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Exercise/Week 1/AnonymityPoll(2).csv")
head(df)
#6(a)
summary(df)
str(df)
#6(b)
table(df$Smartphone) #Similar to value_count in Pytnon Pandas
sum(is.na(df$Smartphone))
#6(c)
table(df$Internet.Use, df$Smartphone)
#6(d)
sum(is.na(df$Internet.Use))
sum(is.na(df$Smartphone))
#6(e)
limited <- subset(df, df$Internet.Use == 1 | df$Smartphone == 1)
str(limited)
#6(f)
summary(limited)
#6(g)
mean(limited$Info.On.Internet)
#6(h)
sum(limited$Info.On.Internet == 0)
sum(limited$Info.On.Internet == 11)
#6(i)
mean(limited$Worry.About.Info, na.rm = TRUE)
#6(j)
mean(limited$Anonymity.Possible, na.rm = TRUE)
#6(k)
hist(limited$Age)
#or
library("ggplot2")
ggplot(limited)+geom_histogram(aes(Age),na.rm=T,binwidth=5,color="black",fill="lightblue")
#6(l)
max(table(limited$Info.On.Internet, limited$Age))
#6(m)
jitter(c(1,1,2,2,3)) #jitter adds some random zero mean noise to the vector
#6(n)
ggplot(limited,aes(Age,Info.On.Internet)) +geom_point(position="jitter",na.rm=T)
#6(o)
tapply(limited$Info.On.Internet, limited$Smartphone, mean)
#6(p)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm = TRUE)

#Qs7
data(swiss)
head(swiss)
?stars
?swiss
#7(a)
stars(swiss, key.loc=c(18,2))
#7(b)
stars(as.matrix(swiss[, c(2,3,5,6)]),location = as.matrix(swiss[, c(4,1)]),axes = T)
#7(c)
stars(as.matrix(swiss[,c(2,3,5,6)]), location = as.matrix(swiss[,c(4,1)]), axes = T, labels = NULL, len = 3, main = "Fertility against Education", xlab = "Education", ylab = "Fertility", draw.segments = TRUE, key.loc = c(80,35))

#Qs8
Parole <- read.csv("Parole(3).csv")
#Qs8(a)
table(Parole$Violator,Parole$Male)
#Qs8(b)
table(Parole$State,Parole$Crime)
#Qs8(c)
ggplot(data = Parole, aes(x = Age)) + geom_histogram()
#Qs8(d)
ggplot(data = Parole, aes(x = Age)) + geom_histogram(binwidth=5,closed=c("left"),center=17.5)
#Qs8(e)
ggplot(data = Parole, aes(x = Age)) + geom_histogram(binwidth=5,closed=c("left"),center=17.5,color=c("blue"))
#Qs8(f)
ggplot(data = Parole, aes(x = Age)) + geom_histogram(binwidth=5,closed=c("left"),center=17.5,color=c("blue"))+facet_grid(Male~.)
