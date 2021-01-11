library("VIM")
library(tidyverse)
library(class)
#loading
phishing<-read.csv("F://ENGG//III year//6th sem//DWM//Project//phishing.csv", TRUE, ",")
head(phishing)
summary(phishing)


#Setting Working Directory
setwd("F://ENGG//III year//6th sem//DWM//Project")

#Preprocessing
#Make Redirect NULL because more than half the values are 0(not known) and imputing makes no sense in this case
phishing$Redirect = NULL
#Convert to dataframe
df<-data.frame(phishing)
#Replace 0s with NA for imputing
df[df==0]<-NA

#MICE - for imputing values
library(mice)
#Table for count of missing values
md.pattern(df)
#Visualising missing values
aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#marginplot(df[c(4,11)]) - compare two coulmns for missing values
#Imputing the missing values
tempData <- mice(df,m=5,maxit=50,meth='pmm',seed=500)
train.complete<-complete(tempData,1)
#Check the number of missing values
sum(is.na(train.complete))
#Visualising missing values after imputation
aggr_plot <- aggr(train.complete, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train.complete), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Creating data partition into test and train set
train=createDataPartition(y=train.complete$Result, p=0.80, list=FALSE)
train_set=train.complete[train,]
test_set=train.complete[-train,]
#summary(tempData)
#print(typeof(tempData))
#print(typeof(train.complete))
#head(tempData)
#df1 <- data.frame(matrix(unlist(tempData), nrow=1122706, byrow=T))
#tempData
#df1<-data.frame(tempData)

library(caret)
require(rpart)

#Visualizing data

library(ggplot2)

#MODEL 1: KNN
train_set$Result <- as.character(train_set$Result)
train_set$Result <- as.factor(train_set$Result)
model_fit = train(Result~., method = "knn", data = train_set, trControl = trainControl(method = 'cv', number = 2, classProbs = FALSE));
print(model_fit);
plot(model_fit);

# Classify from our reserved test set.
testing_set_predict = predict(model_fit, newdata = test_set); 
# Verifying our model from the classifications.
table(testing_set_predict, test_set$Result);
mean(testing_set_predict==test_set$Result)

#MODEL 2: Random Forest
library(randomForest)
train_set$Result <- as.character(train_set$Result)
train_set$Result <- as.factor(train_set$Result)
rf = randomForest(Result~ ., ntree = 100,data = train_set)
plot(rf)
varImpPlot(rf,  sort = T,n.var=10,main="Top 10 - Variable Importance")
rf_result <- predict(rf ,test_set)
table(rf_result, test_set$Result)
mean(rf_result==test_set$Result)


#MODEL 3: SVM
require(e1071)
svm_model <- svm(Result~., data=train_set, type='C-classification', kernel='radial')
pred_test <-predict(svm_model,test_set)
mean(pred_test==test_set$Result)
table(pred_test, test_set$Result)
#plot(svm_model, test_set)

#MODEL 4: Boosted Logistic Regression
#trainControl for Boosted Logisitic Regression
fitControl <- trainControl(method = 'repeatedcv', repeats = 5,
                           number = 5, verboseIter = T)

# Run a Boosted logisitic regression over the training set
log.fit <- train(Result~.,  data = train_set, 
                 method = "LogitBoost", trControl = fitControl,
                 tuneLength = 5)

# Predict the testing target
log.predict <- predict(log.fit, test_set[,-31])
table(log.predict, test_set$Result)

mean(log.predict==test_set$Result)

#MODEL 5:Decision Tree
require(rpart)
library(rattle)	
library(rpart.plot)
tree= rpart(Result~., data=train_set, control=rpart.control(cp=.0005))
#Plotting
fancyRpartPlot(tree)

tree_pred=predict(tree, test_set, type='class')
#Accuracy
mean(tree_pred==test_set$Result)
#Confusion matrix
table(tree_pred, test_set$Result)

