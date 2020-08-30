profit <- read.csv('D:/WORK/NEURAL NETWORKS/50_Startups.csv')
str(profit)
table(profit$State)
# normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normp <- as.data.frame(lapply(profit[,-4],FUN = normalize))
summary(normp$Profit)
# data partition
library(caret)
partition <- createDataPartition(normp$Profit,p=.70,list = F)
train <- normp[partition,]
test <- normp[-partition,]
# Using multilayered feed forward nueral network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)
# Building model
pmodel <- neuralnet(Profit~.,data = train)
summary(pmodel)
plot(pmodel)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(pmodel,test[1:3])
model_results$net.result
pred_profit <- model_results$net.result
pred_profit
model_results$neurons
# to find correlation
cor(pred_profit,test$Profit)
plot(pred_profit,test$Profit)
#It has correlation about 0.9568
# New model
model_5<-neuralnet(Profit~.,data= train,hidden = c(5,3))
plot(model_5)
model_5_res<-compute(model_5,test[1:3])
pred_p_5<-model_5_res$net.result
cor(pred_p_5,test$Profit)
plot(pred_p_5,test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
# It has correlation about 0.9642
