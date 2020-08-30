library(readr)
emp_data <- read_csv("D:/WORK/SIMPLE LINEAR REGRESSION/emp_data.csv")
View(emp_data)
# Changing column names

colnames(emp_data)[1] <- "sh"
colnames(emp_data)[2] <- "cr"
attach(emp_data)
View(emp_data)
## Simple Linear Regression ##
#Both x and y are contionous
#x-sh,y-cr
# EDA
summary(emp_data)
# Scatter Plot 
plot(sh,cr)
# Correlation Coefficient
cor(emp_data$sh,emp_data$cr)  # cor()> 0.85 ,but it is in negative direction
# Model Bulding # MR^2 > 0.81, P-value < 0.05 so it is significance
slr <- lm(cr~sh)
summary(slr)
predict <- predict(slr)
predict(slr,interval="predict")
#ggplot
ggplot(data = emp_data,aes(x =sh,y = cr)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=sh, y=predict))
