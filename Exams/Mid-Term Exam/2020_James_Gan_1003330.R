library(ggplot2) #Standard Data Visualisation Plotting library
library(ggfortify) #Aids Plotting linear plots with ggplot2
library(psych) #Create a scatter plot matrix
library(factoextra) #Visualisation of the PCA eigen values
library(caTools) #Perform train-test split on dataframe
suppressMessages(library(ROCR)) #AUC-ROC Package
library(mlogit) #Multinomial Logistics Regression
library(leaps) #Subset selection


rm(list=ls())
#Question 1
df1 <- read.csv("UScrime(8).csv")
str(df1)
View(df1)
#1(a)
df1[which.max(df1$Crime),]
df1[min(df1$Ineq),]
#1(b)
df1_b <- subset(df1, df1$Wealth > 5000)
mean(df1_b$Crime)
df1_b2 <- subset(df1, df1$Wealth < 5000)
mean(df1_b2$Crime)
#1(d)
t.test(df1_b$Crime, df1_b2$Crime)
#1(e)
UStrain <- subset(df1[4:47, ], select=-States)
model1<- lm(Crime~Wealth, data=UStrain)
summary(model1)
#1(f)
UStest <- subset(df1[1:3, ], select=-States)
UStest[3,]
predict.lm(model1,newdata=UStest[3,],interval=c("confidence"),level=.95)
#1(g)
model2 <-lm(Crime~., data=UStrain)
summary(model2)
#1(h)
model3 <-lm(Crime~M+Ed+Po1+U2+Ineq+Prob, data=UStrain)
summary(model3)
#1(j)
model4 <- regsubsets(Crime~., UStrain, nvmax=17)
summary(model4)
plot(model4)
which.max(summary(model4)$adjr2)
summary(model4)$adjr2[which.max(summary(model4)$adjr2)]
#1(k)
which.max(summary(model4)$bic)
summary(model4)$bic[which.max(summary(model4)$bic)]
#1(l)
model_j <- lm(Crime~M+Ed+Po1+M.F+U1+U2+Wealth+Ineq+Prob, data=UStrain)
predtest_j <- predict(model_j,newdata=UStest)
sse_j<-sum((UStest$Crime)-predtest_j)^2
sse_j

model_k <- lm(Crime~., data=UStrain)
predtest_k <- predict(model_k,newdata=UStest)
sse_k<-sum((UStest$Crime)-predtest_k)^2
sse_k
#1(m)
sse_j>sse_k
#1(n)
df2 <- df1
rownames(df2) <- df2$States
df2 <- subset(df2, select=-c(States,Crime))
str(df2)
pca_output <- prcomp(df,scale=T)
summary(pca_output)
#1(o)
pca_output <- prcomp(df2,scale=T)
summary(pca_output)
pca_output$rotation
pca_output$x

fviz_eig(pca_output)
fviz_pca_biplot(pca_output, label = "var", addEllipses=TRUE, ellipse.level=0.95)

View(df1)



#Qs2
#2(a)
transport <- read.csv("transport(1).csv")
str(transport)
head(transport)
tapply(transport$CHOICE, transport$MODE, FUN=sum)
#2(c)
set.seed(200)
spl <- sample(210, 0.7*210)
training <- subset(transport, is.element(ID, spl))
#2(d)
tapply(training$CHOICE, training$MODE, FUN=sum)
48/(41+48+15+43)
#2(e)
data_transport <- mlogit.data(training,  # data.frame of data
                        choice = "MODE",  # column name of choice
                        shape = "long",  # wide means each row is an observation
                        # long if each row is an alternative
                        varying = c(3,9), # indices of varying columns for each alternative,
                        sep = "."  # not necessary but still good to be clear
)


