library(mlogit)
library(dfidx)
library(caTools)
library(tm)
library(wordcloud)
library(rpart)
library(rpart.plot)
library(SnowballC)
library(qdap)
library(pacman)
library(dplyr)
library(textstem)

rm(list=ls())
setwd("/Users/james/OneDrive - Singapore University of Technology and Design/SUTD/Year 3/Term 6/40.016 - The Analytics Edge/Week 13 - Kaggle Comp")
df <- read.csv("train.csv", stringsAsFactors=FALSE)
test.df <- read.csv("test.csv", stringsAsFactors=FALSE)


#Training Dataset
CORPUS <- Corpus(VectorSource(df$tweet)) #Change the object data type to document
#CORPUS<-tm_map(CORPUS, content_transformer(strip), char.keep="':)'")
CORPUS <- tm_map(CORPUS, removePunctuation) #Remove punctuation
CORPUS <- tm_map(CORPUS, content_transformer(tolower)) #lower case
CORPUS <- tm_map(CORPUS,removeNumbers)
CORPUS <- tm_map(CORPUS,removeWords,c("{link}", "@mention")) #Remove my own specified set of words
CORPUS <- tm_map(CORPUS, removeWords, stopwords("english")) #Remove Stopwords
CORPUS <- tm_map(CORPUS, stripWhitespace) #Strip white spaces front and back
for (i in 1:length(CORPUS)) {CORPUS [[i]][[1]]<-lemmatize_strings(CORPUS [[i]][[1]])} 
dtm1 <- DocumentTermMatrix(CORPUS)
dtm1 <- removeSparseTerms(dtm1,0.995)
train.cleaned <- as.data.frame(as.matrix(dtm1))
#trained.cleaned <- scale(train.cleaned)
train.cleaned$sentiment <- df$sentiment
train.cleaned$sentiment <- factor(train.cleaned$sentiment)
tweetCorpus <- VCorpus(VectorSource(df$tweet))
tweetCorpus<- tm_map(tweetCorpus, content_transformer(strip), char.keep=":)(/")
dtm <- DocumentTermMatrix(tweetCorpus, control = list(
  dictionary = c(":d" , ":)",":/",":(","(:","):"),
  wordLengths=c(-Inf,Inf), 
  tolower=FALSE
)
)
emoticononly <- as.data.frame(as.matrix(dtm))
total.df <- cbind(train.cleaned,emoticononly)
total.df <- total.df %>% rename("sadface" = ":(", "smilyface" = ":)", "mehface" = ":/", "excitedface" = ":d", "smilyface2" = "(:", "sadface2"= "):")
dim(total.df)


#Testing Dataset
CORPUS <- Corpus(VectorSource(test.df$tweet)) #Change the object data type to document
CORPUS <- tm_map(CORPUS, content_transformer(tolower)) #lower case
CORPUS <- tm_map(CORPUS, content_transformer(removePunctuation), ucp = F, char.keep="):(;/><") #Remove punctuation
CORPUS <- tm_map(CORPUS,removeNumbers)
CORPUS <- tm_map(CORPUS,removeWords,c("{link}", "@mention")) #Remove my own specified set of words
CORPUS <- tm_map(CORPUS, removeWords, stopwords("english")) #Remove Stopwords
CORPUS <- tm_map(CORPUS, stripWhitespace) #Strip white spaces front and back
for (i in 1:length(CORPUS)) {CORPUS [[i]][[1]]<-lemmatize_strings(CORPUS [[i]][[1]])} 
dtm2 <- DocumentTermMatrix(CORPUS, control = list(dictionary=Terms(dtm1)))
test.cleaned <- as.data.frame(as.matrix(dtm2))
tweetCorpustest <- VCorpus(VectorSource(test.df$tweet))
tweetCorpustest <- tm_map(tweetCorpustest, content_transformer(strip), char.keep=":)(/")
dtmemo <- DocumentTermMatrix(tweetCorpustest, control = list(
  dictionary = c(":d" ,":)",":/",":(","(:","):"),
  wordLengths=c(-Inf,Inf), 
  tolower=FALSE
)
)
emoticononlytest <- as.data.frame(as.matrix(dtmemo))
totaltest.df <- cbind(test.cleaned,emoticononlytest)
totaltest.df <- totaltest.df %>% rename("sadface" = ":(", "smilyface" = ":)", "mehface" = ":/", "excitedface" = ":d", "smilyface2" = "(:", "sadface2"= "):")

write.csv(totaltest.df, "cleaned_test_df.csv")
write.csv(total.df, "cleaned_train_df.csv")

#Model Building
# library(randomForest)
# dim(totaltest.df)
# dim(total.df)
# model4 <- randomForest(subset(total.df, select = -c(sentiment)), total.df$sentiment)
# predict4 <- predict(model4,newdata=totaltest.df,type="class")
# write.csv(predict4, "rf_less_variables.csv")

