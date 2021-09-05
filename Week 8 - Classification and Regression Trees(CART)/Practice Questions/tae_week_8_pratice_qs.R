setwd("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Week 8 - Classification and Regression Trees(CART)/Practice Questions")

library(caTools) #Split train test data
library(ROCR) #ROC Curve
library(rpart) #CART
library(rattle) #CART Visualisation
library(RColorBrewer) #CART Visualisation
library(rpart.plot)  #CART Visualisation
library(ipred) #Bagging for decision trees only
library(randomForest)
library(tm) #Create DTM
library(SnowballC) #Stemming words
library(wordcloud)
library(e1071) #Naive Bayes Classifier

# QS 1
df <- read.csv("census.csv")

set.seed(2000)                                   # set seed for random sampling
spl <- sample.split(df$over50k,SplitRatio=0.6) # We use 70% of the data for training
train <- subset(df,spl==TRUE)           # training dataset
test <- subset(df,spl==FALSE)           # testing dataset

model1 <- glm(as.factor(over50k)~.,data=train,family="binomial")
names(which(coef(summary(model1))[,4] < 0.1)) #print out all predictors significant at the 10% significance level

p1 <- predict(model1,newdata=test,type="response")
table1b <- table(p1>=0.5,test$over50k)
sum(diag(table1b))/sum(table1b)

base_1 <- names(table(train$over50k)[which.max(table(train$over50k))])
unname(table(test$over50k)[base_1]/sum(nrow(test)))


#QS 3
supreme <- read.csv("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Week 8 - Classification and Regression Trees(CART)/Practice Questions/supreme(7).csv")

supreme$unCons <- as.integer(rowSums(supreme[,5:13]) == 9)
supreme$unLib <- as.integer(rowSums(supreme[,5:13]) == 0)
  
library(rpart)
library(rpart.plot)
tree_3_d <- rpart((as.factor(unCons)~petit + respon + 
                     circuit + unconst + lctdir + issue),
                  data = supreme)
prp(tree_3_d)