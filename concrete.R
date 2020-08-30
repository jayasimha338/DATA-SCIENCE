concrete <- read.csv('D:/WORK/NEURAL NETWORKS/concrete.csv')
str(concrete)

# normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
norms <- as.data.frame(lapply(concrete,FUN = normalize))
summary(norms$strength)
# data partition
library(caret)
partition <- createDataPartition(norms$strength,p=.75,list = F)
train <- norms[partition,]
test <- norms[-partition,]
# Using multilayered feed forward nueral network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)
# Building model
smodel <- neuralnet(strength~.,data = train)
summary(smodel)
plot(smodel)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(smodel,test[1:8])
model_results$net.result
pred_strength <- model_results$net.result
pred_strength
model_results$neurons
# to find correlation
cor(pred_strength,test$strength)
plot(pred_strength,test$strength)
#It has correlation about 0.8312
# New model
model_5<-neuralnet(strength~.,data= train,hidden = c(4,3))
plot(model_5)
model_5_res<-compute(model_5,test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,test$Profit)
plot(pred_strn_5,test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
# It has correlation about 1
