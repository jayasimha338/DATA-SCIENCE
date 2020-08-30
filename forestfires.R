forest <- read.csv('D:/WORK/NEURAL NETWORKS/forestfires.csv')
str(forest)
table(forest$area)
range(forest$area)
# normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normf <- as.data.frame(lapply(forest[,-c(1,2,31)],FUN = normalize))
summary(normf$area)
# data partition
library(caret)
partition <- createDataPartition(normf$area,p=.75,list = F)
train <- normf[partition,]
test <- normf[-partition,]
# Using multilayered feed forward nueral network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)
# Building model
fmodel <- neuralnet(area~.,data = train)
summary(fmodel)
plot(fmodel)
# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(fmodel,test[,-9])
model_results$net.result
pred_area<- model_results$net.result
pred_area
model_results$neurons
# to find correlation
cor(pred_area,test$area)
plot(pred_area,test$area)
#It has correlation about 0.03863
# New model
model_5<-neuralnet(train$area~.,data= train,hidden = c(4,2))

plot(model_5)
model_5_res<-compute(model_5,test[,-9])
pred_area_5<-model_5_res$net.result
cor(pred_area_5,test$area)
plot(pred_area_5,test$area)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
# It has correlation about 0.9640
