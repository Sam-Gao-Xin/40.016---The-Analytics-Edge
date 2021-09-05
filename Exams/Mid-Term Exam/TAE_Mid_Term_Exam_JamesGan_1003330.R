library(ggplot2) #Standard Data Visualisation Plotting library
library(ggfortify) #Aids Plotting linear plots with ggplot2
library(psych) #Create a scatter plot matrix
library(factoextra) #Visualisation of the PCA eigen values
library(caTools) #Perform train-test split on dataframe
suppressMessages(library(ROCR)) #AUC-ROC Package
library(mlogit) #Multinomial Logistics Regression
library(leaps) #Subset selection

#Clean Environment
setdiff(ls(), ls(pattern = "SETUP"))
rm(list = setdiff(ls(), ls(pattern = "SETUP")))

                              # Week 1 - R Syntax

df <- read.csv("wine_italy.csv",sep=",",head=T)
str(df) #See the basic overview of the dataframe
sapply(Parole, class) #Look at all the data type 

#1.1 Dataframe Operations

#1.1a - Dataframe Statistics
table(df$column_name) #Value_count
sum(is.na(df$column_name)) #Count null value in that column
length(table(df$column_name)) #Count unique values in that column
names(table(df$column_name)) #Get all the unique key values in that column as strings
nrow(subset(df, column_name == 0 & column_name_2 > 10)) #Count no of rows that satisfies condition
nrow(df) #No. rows in dataframe
ncol(df) #No. cols in dataframe
colnames(df) #Get all the column names in the dataframe

#1.1b - Count unique values for each category
that_col <- df$col_name_1
df$count_col_per_catergory <- table(that_col)[as.character(that_col)]

#1.1c - More complex Dataframe calcualtions
mean(df$column_name) #Including count of na rows
mean(df$column_name, na.rm=TRUE) #Removing count of na rows
names(which.max(table(df$column_name_1))) #Name of row with max value
sum(df$column_name == 10) #Summation of values of column with condition
max(table(df$column_name_1, df$column_name_2)) #Largest number of rows that has same value for two columns
tapply(df$column_name, df$index_column_name, FUN=mean, na.rm=TRUE)  #Functions include: mean, median, min, max
mean(subset(wine$price91,wine$age91>=15)) #Find mean price of wine for wine age>=15
mean(subset(wine$price91,wine$hrain<mean(wine$hrain)&wine$tempdiff<mean(wine$tempdiff))) #Another more complex condition to find mean price of wine, where the rain and tempdiff is less than their averages
new_df <- subset(df, column_name == 0 & column_name_2 > 10) #Taking a subset of dataframe with condition
new_df<-subset(df,select=-c(column_name)) #Taking a subset of dataframe w/o certain columns

#1.1d - Count number of levels in factor class column
sapply(df[sapply(df, is.factor)], nlevels) #get the number of levels of each column

#1.3 Datatype Conversion
df$col_name <- as.factor(df$col_name)
df$col_name <- as.character(df$col_name)
df$col_name <- as.numeric(df$col_name)

#1.2 Dataframe Visualisation
hist(df$column_name) #Simple Histogram Plot
#or
library("ggplot2")
ggplot(df)+geom_histogram(aes(column_name),na.rm=T,binwidth=5,color="black",fill="lightblue")

plot(jitter(df$column_name_1), jitter(df$column_name_2)) #Preventing points from overlapping each other
#or
ggplot(df, aes(column_name_1, column_name_2)) +geom_point(position="jitter",na.rm=T)

#1.21 Star Plot
star(df, key.loc=c(18,2)) #the key.loc is the coordinate position of the legend

stars(as.matrix(swiss[,c(2,3,5,6)]), location = as.matrix(swiss[,c(4,1)]), axes = T, labels = NULL, len = 3, main = "Fertility against Education", xlab = "Education", ylab = "Fertility")

#1.22 Histogram Plot
ggplot(data = Parole, aes(x = Age)) + geom_histogram(binwidth=5,closed=c("left"),center=17.5,color=c("blue"))+facet_grid(Male~.) # Top-Bottom Plot
ggplot(data = Parole, aes(x = Age)) + geom_histogram(binwidth=5,closed=c("left"),center=17.5,color=c("blue"))+facet_grid(.~Male) # Adjacent Plot
ggplot(data = Parole, aes(x = Age, fill = as.factor(Male))) + geom_histogram(binwidth=5,closed=c("left"),center=17.5,color=c("blue")) #	Plot the category as values within histogram bar



                              # Week 2 - Linear Regression and PCA
#2.1 Data Ingestion
setwd("")
df<-read.csv("")
str(df)

#2.2 Data Pre-Processing
df$column_name <- as.numeric(as.character(df$column_name))   #Convert Column from character to numeral datatype

#2.3 Data Analysis
cor(df$independent_var_1,df$independent_var_2, use = "pairwise.complete.obs")^2

df1<-subset(df,select=-c(predictor_var)) #Calculate all pearson correlation for independent variable
str(df1)
cor(df1, use = "pairwise.complete.obs")^2

#2.4 Initial Data Visualisation
pairs.panels(df,ellipses = F, lm =T, breaks=10, hist.col="blue") #Plot Linear plots and Pearson Correlation Values

ggplot(df,aes(independent_var_1,predictor_var))+geom_point(na.rm=T)+geom_smooth(method="lm",na.rm=T,se=F) #Plot the scatter plot with linear regression line

#2.5 Linear Model
model1<- lm(predictor_var~independent_var_1 + independent_var_2, data=df)
summary(model1)

confint(model1, level = 0.99) #Calculating the confidence level for coefficients of regression

test_data <- subset(wine,vintage>=1982)
predtest <- predict(model1,newdata=test_data)

#2.6 Polynomial Linear Regression Model
modelpoly1 <- lm(predictor_var~poly(independent_var_1,3,raw=TRUE), data = df) #power 3 polynomial regression plot
summary(modelpoly1)

#2.7 Data Visualisation
autoplot(model1,label.size = 3) #Plot QQ, residual plots

ggplot(df)+geom_point(aes(independent_var_1,predictor_var))+geom_line(aes(independent_var_1,predicted_y_1),color="blue",size=2)+geom_line(aes(independent_var_2,predicted_y_2),color="red",linetype="solid",size=2) #Comparing Linear vs Polynomial Plot

#2.8 Prediction and SSE, SST and r^2
test_data <- subset(wine,vintage>=1982)
predtest <- predict(model1,newdata=test_data)

sse<-sum((test_data$predictor_var)-predtest)^2
sst<-sum((test_data$predictor_var - mean(test_data$predictor_var))^2)
testR2<- 1 - (sse/sst)
testR2

#2.9 Principal Component Analysis(PCA)
#Eigenvalues represent the total amount of variance that can be explained by a given principal component
#Eigenvector represent a weight of each eigenvalue

pca_output <- prcomp(df,scale=T) #method to calculate pca for the independent variable with scaling
summary(pca_output) #Outputs includes sd(aka eigenvalue), Proportion of variance, and cumulative proportion

pve<-pca_output$sdev^2/sum(pca_output$sdev^2) #Manual calculation of Proportion of Variance Explained(PVE)
cpve<-cumsum(pve) #Cumulative Proportion of PVE

#2.9a PCA Visualisation (Using factoextra library)
fviz_eig(pca_output) #Histogram plot of the eigenvalues
fviz_pca_biplot(pca_output, label = "var", habillage=dependent_var_name, addEllipses=TRUE, ellipse.level=0.95) #biplot to see the separation of the different labels on the principal components | We can see the axes of the datatset

plot(cpve.w,xlab="Principal components",type="l",ylim=c(0,1))+abline(h=0.8,col="red")+abline(v=5,col="blue") #Plot a linear graph of the cumulative PVE
plot(pr.wine,type="l",ylim=c(0,5),main="Scree plot")+abline(h=1,col="red") #Scree Plot: Line plot of the individual eigenvalues

#2.9b PCA Loading Values: Loading Values = EigenVectors*(EigenValues)**0.5 
pca_output$rotation[order(pca_output$rotation[,1],decreasing=T),1:2] # Order according to PC1 | The ratio indicate the indices of the PCA columns to consider
pca_output$rotation[order(pca_output$rotation[,2],decreasing=T),1:2] # Order according to PC2


                                                    # Week 3 - Logistics Regression
#3.1 Basic Logistics Regression Model
model_1 <- glm(dependent_var ~ independent_var, data = df, family = binomial)
summary(model_1)

#3.1a - Get probability with 1 case
coef_table_1 <- summary(model_1)$coefficients #Get the coefficient values
coef_table_1 <- coef_table_1[!(startsWith(rownames(coef_table_1), "col_name_1")  | startsWith(rownames(coef_table_1), "col_name_2")),] #Remove these column names if necessary
x_1 <- c(1, 2 ,40 ,30, 34, 45) #Data of that particular case
logodds_1 <- coef_table_1[,1] %*% x_1  #Matrix Multiplication
prob = exp(logodds_1)/ (1+exp(logodds_1)) #Attain our probability

#3.2 Analysis of single independent variable (for loop)
p_val <- c() #to save our p-values for later
model_list <- list()  # to contain all the models later
all_var <- c("Year", "RS", "RA", "W", "OBP", "SLG", "BA","RankSeason", "NumCompetitors", "League")#to train single-variable models
for (variable in all_var) {
  model <- glm(as.formula(paste0("predictor_var ~ ", independent_var)), data = df, family = binomial) #paste0 concatenate all elements without separator
  model_list[[variable]] <- model #save the trained model in the list
  # we are appending a named numeric variable, for reference later
  p_val <- c(p_val, setNames(summary(model)$coefficients[2, 4], variable))  
}
p_val #We want the p-value to be <0.05, which would mean that we reject the null hypothesis and the coefficient is significant
sig_vars <- names(p_val[p_val < 0.05]) #Save only the significant variables
sig_vars


#3.3 Analysis of Combination of 2 independent variable (for loop)
var_combns_1 <- combn(sig_vars, 2)  # combinations of 2 variables
aic_table_1 <- data.frame(var_1 = character(), var_2 = character(), aic = numeric())
for (idx in 1:choose(length(sig_vars), 2)) { #iterate through the pariwise combinations of variables
  var_comb <- var_combns_1[,idx] #get the combination of variables
  model <- glm(formula(paste0("predictor_var ~ ", paste0(var_comb, collapse = "+"))), data = df, family = binomial)
  # note that rbind is relatively slow but this is not too important
  aic_table_1 <- rbind(aic_table_1, data.frame(var1 = var_comb[1], var2 = var_comb[2], aic = summary(model)$aic))
}

aic_table_1


#3.4 Train-Test Split
library(caTools)  # not required by this point
split <- sample.split(df$predictor_variable, SplitRatio = 0.7) #We need to declare what column is the predictor variable in the dataframe. This function creates a true false column that maps to the index of every row
train <- subset(df, split == TRUE) #With the index set, we need to map the boolean values to the dataframe as such. Where True values belong to the train dataset.
test <- subset(df, split == FALSE)

# 3.5 Prediction in Logistics Regression
pred_2_h <- predict(model_2, newdata = test, type = "response")
max(pred_2_h) #Finding the max predicted probability

#3.5a Calculate sensitivity/true positive rate with probability threshold of 0.5. We need to calculate the .predict() first
pred_table_2 <- table((pred_2_h > 0.5), test$Violator)
pred_table_2[2,2]/sum(pred_table_2[,2])

#3.6 AUC-ROC
predrocr_2 <- prediction(pred_2_h, df$predictor_variable)
auc_2 <- performance(predrocr_2, measure = "auc")@y.values
auc_2

#3.7 Log likelihood value (Measure of goodness of fit -> Higher the value, the better the model)
logLik(model_3)

#3.8 Scoring/Profit from Confusion Matrix
#The values in the matrix are the score/profit when there's 1 unit in that particular container of the confusion matrix
sum(pred_table_3_d * matrix(c(0,-300,0,100), nrow = 2, ncol = 2))

#3.9 T-Test
#Null hypothesis is that they are independent of each other, reject null hypothesis when p-value is less than significance level
t.test(df$col_1[df$col_2==1],df$col_1[df$col_2==-1])


                                                  # Week 4 - Multinomial Logistics Regression
#4.1 Data preparation for mlogit model
dataheat <- mlogit.data(df,  # data.frame of data
                        choice = "dependent_col",  # column name of choice
                        shape = "wide",  # wide means each row is an observation
                        # long if each row is an alternative
                        varying = c(3:12), # indices of varying columns for each alternative,
                        sep = "."  # not necessary but still good to be clear
)

#4.2 mlogit model without an intercept or a reference level
modelQ1 <- mlogit(depvar ~ ic + oc - 1, dataheat)  # -1 means no intercept

#4.3 mlogit model with an intercept and a reference level
modelQ1_3 <- mlogit(depvar ~ ic + oc, data = dataheat, reflevel = "hp") #Normalising the constant for the alternative hp to 0

#4.4 Alternative-specific effects
modelQ1_6 <- mlogit(depvar ~ oc + ic | income, dataheat) #In this case, we look at the alternative-specific income effects  
summary(modelQ1_6)

#4.5 Mixed Logistics Model
#Reference and take code from Week4-5 Exercise Practice
data_2 <- mlogit.data(df_2, id.var = "id", choice = "choice", varying = vary_ind_2, shape = "wide", sep = "")
model_2_a <- mlogit(as.formula(paste0("choice ~ ", paste0(pred_vars_2, collapse = " + "), " - 1")), data = data_2, rpar = rpar_vec_2_a, panel = TRUE)
summary(model_2_a)


                                          # Week 5 - Sparsity and Model Selection
#5.1 Best Subset Selection
model2 <- regsubsets(Salary~., hitters, nvmax=19) #Changing the max number of features in the model, and value for 19 for all the feature variables, as the default value of nvmax is 8
summary(model2)
plot(model2)

#5.1a Finding the best subset selection that has highest adjusted r^2
which.max(summary(model2)$adjr2)

#5.2 Forward Stepwise Selection
model3<-regsubsets(Salary~.,data=hitters,nvmax=19,method="forward")
which.max(summary(model3)$adjr2)

#5.3 Backwward Stepwise Selection
model4<-regsubsets(Salary~.,data=hitters,nvmax=19,method="backward")
which.max(summary(model4)$adjr2)

#5.4 Predict regsubsets
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars =names(coefi)
  mat[,xvars]%*%coefi
}
pred<- predict.regsubsets(model_valset,hitters[valset,],id=10)

#5.5 K-Fold Cross Validation
k=10
folds=sample(1:k,nrow(hitters),replace =TRUE)
cv.errors =matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))




