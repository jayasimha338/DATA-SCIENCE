library(readr)
calories_consumed <- read_csv("D:/WORK/SIMPLE LINEAR REGRESSION/calories_consumed.csv")
View(calories_consumed)
# Changing column names
View(calories_consumed)
colnames(calories_consumed)[1] <- "wg"
colnames(calories_consumed)[2] <- "cc"
attach(calories_consumed)
View(calories_consumed)
## Simple Linear Regression ##
#Both x and y are contionous
#x-calories,y-weight
# EDA
summary(calories_consumed)
# Scatter Plot = Curvilinear in nature
plot(calories_consumed$cc,calories_consumed$wg)
# Correlation Coefficient
cor(calories_consumed$wg,calories_consumed$cc)  # cor()> 0.85 and positive direction

# Model Bulding # MR^2 > 0.81, P-value < 0.05 so it is significance
slr <- lm(wg~cc)
summary(slr)
predict <- predict(slr)
predict(slr,interval="predict")

#ggplot
ggplot(data = calories_consumed,aes(x =cc,y = wg)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=cc, y=predict))

