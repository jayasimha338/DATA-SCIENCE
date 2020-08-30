library(readr)
Salary_Data <- read_csv("D:/WORK/SIMPLE LINEAR REGRESSION/Salary_Data.csv")
View(Salary_Data)
# Changing column names
View(Salary_Data)
colnames(Salary_Data)[1] <- "ye"
colnames(Salary_Data)[2] <- "s"
attach(Salary_Data)
View(Salary_Data)
## Simple Linear Regression ##
#Both x and y are contionous
#x-ye,y-s
# EDA
summary(Salary_Data)
# Scatter Plot = Linear
plot(ye,s)
# Correlation Coefficient
cor(ye,s)  # cor()> 0.85 and positive direction
# Model Bulding # MR^2 > 0.81, P-value < 0.05 so it is significance
slr <- lm(s~ye)
summary(slr)
predict <- predict(slr)
predict(slr,interval="predict")
#ggplot
library(ggplot2)
ggplot(data = Salary_Data,aes(x =ye,y = s)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Salary_Data, aes(x=ye, y=predict))
library(caret)
