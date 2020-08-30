forest <- read.csv('D:/WORK/SUPPORT VECTOR MACHINES/forestfires.csv')
View(forest)
f <- forest
str(f)
# data partition
library(caret)
partition <-createDataPartition(f$size_category,p=0.70,list=F) 
train <- f[partition,]
test <- f[-partition,]
### Building model using different kernel tricks ###
library(kernlab)

# Vanilladot
model1 <- ksvm(size_category~.,data=train,kernel="vanilladot")
pred1 <- predict(model1,newdata=test)
mean(pred1==test$size_category) # 88.31%

# Rbfdot
model2 <- ksvm(size_category~.,data=train,kernel="rbfdot")
pred2 <- predict(model2,newdata=test)
mean(pred2==test$size_category) # 76.62%

# Besseldot
model3 <- ksvm(size_category~.,data=train,kernel="besseldot")
pred3 <- predict(model3,newdata=test)
mean(pred3==test$size_category) # 66.23%
# Polydot
model4 <- ksvm(size_category~.,data=train,kernel="polydot")
pred4 <- predict(model4,newdata=test)
mean(pred4==test$size_category) # 88.31%

# Splinedot
model5 <- ksvm(size_category~.,data=train,kernel="splinedot")
pred5 <- predict(model5,newdata=test)
mean(pred5==test$size_category) # 64.93%

# Anovadot
model6 <- ksvm(size_category~.,data=train,kernel="anovadot")
pred6 <- predict(model6,newdata=test)
mean(pred6==test$size_category) # 89.61%

# Tanhdot
model7 <- ksvm(size_category~.,data=train,kernel="tanhdot")
pred7 <- predict(model7,newdata=test)
mean(pred7==test$size_category) # 58.44%

# Laplacedot
model8 <- ksvm(size_category~.,data=train,kernel="laplacedot")
pred8 <- predict(model8,newdata=test)
mean(pred8==test$size_category) # 74.67%

# Hence from above anovadot kernel trick has high accuracy 89.61%
