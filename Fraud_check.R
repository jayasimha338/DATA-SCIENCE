fraud <- read.csv('D:/WORK/RANDOM FORESTS/Fraud_check.csv')
View(fraud)
f <- fraud
f$Taxable.Income <- ifelse(f$Taxable.Income<=30000,'risky','good')
table(f$Taxable.Income)
f$Taxable.Income <- as.factor(f$Taxable.Income)
str(f)
# data partition
library(caret)
partition <- createDataPartition(f$Taxable.Income,p=0.70,list = F)
train <- f[partition,]
str(train)
test <- f[-partition,]
# Building a random forest model on training data 
library(randomForest)
model <- randomForest(Taxable.Income~.,data = train, na.action=na.roughfix,importance=T)
model$ntree
# Training accuracy 
mean(train$Taxable.Income==predict(model,train))# 90.97
#Predicting the test data
pred_test <- predict(model,test)
mean(pred_test==test$Taxable.Income)
#cross table
library(gmodels)
f_perf <- CrossTable(test$Taxable.Income,pred_test,
                     prop.chisq = F,prop.c = F,prop.r = F,
                     dnn = c('actual default','predict default'))
# Since the model accuracy is 78.21% 
confusionMatrix(pred_test,test$Taxable.Income)

