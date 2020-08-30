company <- read.csv('D:/WORK/RANDOM FORESTS/Company_Data.csv')
c <- company
summary(c$Sales)
c$Sales <- ifelse(c$Sales<=5,'low','high')
c$Sales <- as.factor(c$Sales)
str(c)
#data partiton
partition <- createDataPartition(c$Sales,p=0.75,list = F)
train <- c[partition,]
test <- c[-partition,]
# Building a random forest model on training data 
library(randomForest)
model <- randomForest(Sales~.,data = train,na.action=na.roughfix,importance=T)
model$ntree
# Training accuracy
mean(train$Sales==predict(model,train))#100
#Predicting the test data
pred_test <- predict(model,test)
mean(pred_test==test$Sales)
#cross table
library(gmodels)
c_perf <- CrossTable(test$Sales,pred_test,
                     prop.chisq = F,prop.c = F,prop.r = F,
                     dnn = c('actual default','predict default'))
# Since the model accuracy is 81.81% 