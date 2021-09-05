#Import relevant libraries
library(tidyverse)
library(tm)
library(textclean)
require(data.table)
library(caret) #Implement K Fold Cross Validation Method
library(rpart) #Implement CART
library(LiblineaR) #Implement Support Vector Machines
library(e1071)
library(ranger)
library(dplyr)
library(FCNN4R)
library(plyr)
library(nnet)
library(gbm)
library(caTools)
library(neuralnet)
library(factoextra)

# 1. Data Ingestion
rm(list=ls())
setwd("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Week 13 - Kaggle Comp")
df <- read.csv("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Week 13 - Kaggle Comp/cleaned_train_df.csv", stringsAsFactors=FALSE)
value <- 0.01*nrow(df)
df.subset <- df[1:value,]
#test.df <- read.csv("test.csv", stringsAsFactors=FALSE)


# 2. Data Pre-Processing

# #2a. Training Dataset
# CORPUS <- Corpus(VectorSource(df$tweet)) #Change the object data type to document
# CORPUS <- tm_map(CORPUS, content_transformer(tolower)) #lower case
# CORPUS <- tm_map(CORPUS, content_transformer(removePunctuation), ucp = F, char.keep="):(;/><") #Remove punctuation
# CORPUS <- tm_map(CORPUS,removeWords,c("{link}", "@mention")) #Remove my own specified set of words
# CORPUS <- tm_map(CORPUS, removeWords, stopwords("english")) #Remove Stopwords
# CORPUS <- tm_map(CORPUS, stemDocument, language = "english") #Stem Words
# CORPUS <- tm_map(CORPUS, stripWhitespace) #Strip white spaces front and back
# dtm <- DocumentTermMatrix(CORPUS)
# dtm <- removeSparseTerms(dtm,0.995)
# train.cleaned <- as.data.frame(as.matrix(dtm))
# colnames(train.cleaned) <- make.names(colnames(train.cleaned))
# #trained.cleaned <- scale(train.cleaned)
# train.cleaned$sentiment <- df$sentiment
# train.cleaned$sentiment <- factor(train.cleaned$sentiment)
# dim(train.cleaned)
# 
# 
# #2b. Testing Dataset
# CORPUS <- Corpus(VectorSource(test.df$tweet)) #Change the object data type to document
# CORPUS <- tm_map(CORPUS, content_transformer(tolower)) #lower case
# CORPUS <- tm_map(CORPUS, content_transformer(removePunctuation), ucp = F, char.keep="):(;/><") #Remove punctuation
# CORPUS <- tm_map(CORPUS,removeWords,c("{link}", "@mention")) #Remove my own specified set of words
# CORPUS <- tm_map(CORPUS, removeWords, stopwords("english")) #Remove Stopwords
# CORPUS <- tm_map(CORPUS, stemDocument, language = "english") #Stem Words
# CORPUS <- tm_map(CORPUS, stripWhitespace) #Strip white spaces front and back
# dtm2 <- DocumentTermMatrix(CORPUS, control = list(dictionary=Terms(dtm)))
# test.cleaned <- as.data.frame(as.matrix(dtm2))
# colnames(test.cleaned) <- make.names(colnames(test.cleaned))
# #test.cleaned <- scale(test.cleaned)
# dim(test.cleaned)




#Machine Learning Models

#Data Splitting
set.seed(123)
spl   <- sample.split(train.cleaned$sentiment,SplitRatio=0.7)
train <- subset(train.cleaned,spl==TRUE)
test  <- subset(train.cleaned,spl==FALSE)


#3.1 Naive Bayes Model - Did not work very well
# model1 <- naiveBayes(sentiment~.,data=train.cleaned)
# summary(model1)
# predict1 <- predict(model1,newdata=test.cleaned,type="class")
# write.csv(predict1,"/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Week 13 - Kaggle Comp", row.names = FALSE)

#3.2 Gradient Boosting Classifier
# model2 <- gbm(formula = sentiment~.,data = train,n.trees = 500,cv.folds = 1)
# pred2 <- predict(model2, n.trees = model2$n.trees, test)
# sum(diag(table(test$sentiment, pred2)))/sum(table(test$sentiment, pred2))

#3.3 XGBoost
# xgb.fit1 <- xgb.cv(data = train.cleaned,label = sentiment,nrounds = 1000,nfold = 5,objective = "multi:softmax", verbose = 0)
# pred2 <- predict(xgb.fit1, test)
# table(pred2, test$sentiment)

#3.4 Random Forests
# library(randomForest)
# model4 <- randomForest(sentiment~.,data=train.cleaned)
# predict4 <- predict(model4,newdata=test.cleaned,type="class")
# write.csv(predict4, "rf.csv")

#3.5 CART
# library(rpart)
# model5 <- rpart(sentiment~.,data=train.cleaned)
# predict5 <- predict(model5,newdata=test.cleaned,type="class")
# write.csv(predict5, "cart.csv")

#3.6 Neural Networks
# model6 <- neuralnet(sentiment~., train.cleaned, hidden = c(100, 10), linear.output = F, rep=1)
# output6 <- compute(model6,test.cleaned)
# output6 <- output6$net.result
# output6.df <- as.data.frame(as.matrix(output6, dimnames = list(NULL, c(1, 2, 3))))
# predict6 <- apply(output6.df,1,function(x) which(x==max(x)))
# write.csv(predict6, "nn_4layers.csv")

#3.7 K Nearest Neighbor
# library(class)
# train.labels <- train.cleaned$sentiment
# train.knn <- df <- subset(train.cleaned, select = -c(sentiment))
# dim(train.knn)
# dim(test.cleaned)
# predict7 <- knn(train = train.knn, test = test.cleaned, cl = train.labels , k=3)
# write.csv(predict7, "k nearest neighbour.csv")


#Mutli Class Logistics Regression

#Sentiment 1


