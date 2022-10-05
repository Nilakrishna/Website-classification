#uploading dataset
library(readr)
df=read.csv("C:/Users/nilak/Downloads/website.csv")
View(df)

#dimension
dim(df)

str(df)
summary(df)

var_names = names(df)
var_names

library(dplyr)
#Exploring the target feature
table(df$Type)

library(ggplot2)
#PLOTTING THE TARGET FEATURE
png(file="target.png")
ggplot(df, aes(x = Type)) + geom_bar(color='blue',fill = "#FF6666")
dev.off()

#histogram 
library(purrr)
library(tidyr)
png(file="hist.png")
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
dev.off()

#boxplot
png(file="boxplot1.png")
boxplot(df$Type ~ df$DNS_QUERY_TIMES,data=df, xlab = "Type",ylab = "DNS QUERY TIMES",
                                                    main = "website data")


#preprocessing

#removing unwanted variables
df=df[-c(1,3:10,20)]
View(df)

#missing values
colSums(sapply(df,is.na))

#correlation matrix
df.cor = cor(df)
df.cor
png(file="corrplot.png")
library(corrplot)
corrplot(df.cor)
dev.off()


#split into test and train data
library(caTools)
set.seed(123) 
split=sample.split(df,SplitRatio = 0.8)
train_data<-subset(df,split==TRUE)
test_data<-subset(df,split==FALSE)
View(train_data)
View(test_data)

#MODELS

#multiple logistic regression
logistic<-glm(Type~.,data=train_data,family=binomial)
summary(logistic)
prob_pred = predict(logistic, type = 'response', newdata = test_data)

#confusion matrix
cm = table(ActualValue=test_data$Type, PredictedValue=prob_pred > 0.5)
cm

#accuracy
sum(diag(cm))/sum(cm)

library(rpart)
library(rpart.plot)

#decision tree model on train data
model_tree<-rpart(Type~.,data=train_data,method="class")
model_tree
png(file="tree.png")
rpart.plot(model_tree)
dev.off()

library(forecast)
#prediction on test data
pred_tree=predict(model_tree, test_data, type = 'class')
cm1 = table(ActualValue=test_data$Type, PredictedValue=pred_tree)
cm1
#accuracy
sum(diag(cm1))/sum(cm1)


#Random forest 
library(caret)
library(randomForest)


rf_model<-randomForest(as.factor(Type)~.,data=train_data,ntree=50,type="classification")
rf_model
str(rf_model)
png(file="rf.png")
plot(rf_model)
dev.off()


pred_rf=predict(rf_model,test_data,type="class")
levels(pred_rf)
levels(test_data$Type)
confusionMatrix(pred_rf,as.factor(test_data$Type))

png(file="rfimp.png")
varImpPlot(rf_model)
dev.off()
importance(rf_model)
#The Random Forest model  ranked SOURCE_APP_BYTES as the most important variable
#Random Forest has the highest accuracy so it is the best model