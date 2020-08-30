library(neuralnet)
library(nnet)
library(caret)
forestfires_data <- read.csv("D:/WORK/NEURAL NETWORKS/forestfires.csv")
View(forestfires_data)
str(forestfires_data)
forestfires <- forestfires_data[,-c(1,2,31)]
View(forestfires)
str(forestfires)
##normalizating the data
normalize <- function(x){
  return(((x-min(x))/(max(x)-min(x))))
}
forestfires_norm <- as.data.frame(lapply(forestfires,FUN= normalize))
View(forestfires_norm)
## partition of train and test data
library(caret)
partition <- createDataPartition(forestfires_norm$area,p=.70,list=F)
norm_train <- forestfires_norm[partition,]
View(norm_train)
norm_test <- forestfires_norm[partition,]
View(norm_test)
###model buidling 1
model1 <- neuralnet(norm_train$area~.,data = norm_train)
plot(model1)
model1_results <- compute(model1,norm_test[,-9])
predict_area <- model1_results$net.result
cor(predict_area,norm_test$area)#13.73%
plot(predict_area,norm_test$area)
###model buidling 2
model2 <- neuralnet(norm_train$area~.,data = norm_train,hidden = 2)
plot(model2)
model2_results <- compute(model2,norm_test[,-9])
predict_area_2<- model2_results$net.result
cor(predict_area_2,norm_test$area)#90.82%
plot(predict_area_2,norm_test$area)
####model buidling 3
model3 <- neuralnet(norm_train$area~.,data = norm_train,hidden = 5)
plot(model3)
model3_results <- compute(model3,norm_test[,-9])
predict_area_3<- model3_results$net.result
cor(predict_area_3,norm_test$area)#96.21%
plot(predict_area_3,norm_test$area)
####model buidling 4-combition of hidden neurons
model4<- neuralnet(norm_train$area~.,data = norm_train,hidden =c(4,2))
plot(model4)
model4_results <- compute(model4,norm_test[,-9])
predict_area_4<- model4_results$net.result
cor(predict_area_4,norm_test$area)#94.89%
plot(predict_area_4,norm_test$area)

####model buidling 5-combition of hidden neurons
model5<- neuralnet(norm_train$area~.,data = norm_train,hidden =c(5,4))
plot(model5)
model5_results <- compute(model5,norm_test[,-9])
predict_area_5<- model5_results$net.result
cor(predict_area_5,norm_test$area)#97.81%
plot(predict_area_5,norm_test$area)


