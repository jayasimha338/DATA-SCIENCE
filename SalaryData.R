train <- read.csv('D:/WORK/SUPPORT VECTOR MACHINES/SalaryData_Train(1).csv')
test <- read.csv('D:/WORK/SUPPORT VECTOR MACHINES/SalaryData_Test(1).csv')
prop.table(table(train$Salary))
prop.table(table(test$Salary))
library(kernlab)
library(caret)
### Building model using different kernel tricks ###

# Vanilladot
model1 <- ksvm(Salary~.,data=train,kernel="vanilladot")
pred1 <- predict(model1,newdata=test)
mean(pred1==test$Salary) # 84.62

# Rbfdot
model2 <- ksvm(Salary~.,data=train,kernel="rbfdot")
pred2 <- predict(model2,newdata=test)
mean(pred2==test$Salary) # 85.43

# Besseldot
model3 <- ksvm(Salary~.,data=train,kernel="besseldot")
pred3 <- predict(model3,newdata=test)
mean(pred3==test$Salary) # 77.03

# Polydot
model4 <- ksvm(Salary~.,data=train,kernel="polydot")
pred4 <- predict(model4,newdata=test)
mean(pred4==test$Salary) # 84.62

# Splinedot
model5 <- ksvm(Salary~.,data=train,kernel="splinedot")
pred5 <- predict(model5,newdata=test)
mean(pred5==test$Salary) # 75.17

# Anovadot
model6 <- ksvm(Salary~.,data=train,kernel="anovadot")
pred6 <- predict(model6,newdata=test)
mean(pred6==test$Salary) # 78.26%

# Tanhdot
model7 <- ksvm(Salary~.,data=train,kernel="tanhdot")
pred7 <- predict(model7,newdata=test)
mean(pred7==test$Salary) # 66.38%

# Laplacedot
model8 <- ksvm(Salary~.,data=train,kernel="laplacedot")
pred8 <- predict(model8,newdata=test)
mean(pred8==test$Salary) # 85.24%
