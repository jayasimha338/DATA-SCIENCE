library(readr)
delivery_time <- read_csv("D:/WORK/SIMPLE LINEAR REGRESSION/delivery_time.csv")
View(delivery_time)
# Changing column names
View(delivery_time)
colnames(delivery_time)[1] <- "dt"
colnames(delivery_time)[2] <- "st"
View(delivery_time)
## Simple Linear Regression ##
#Both x and y are contionous
#x-sorting time,y-delivery time
# EDA
summary(delivery_time)
# Scatter Plot 
plot(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`)
qqline(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`)
# Correlation Coefficient
cor(delivery_time)  
# Model Bulding 
slr <- lm(delivery_time$`Delivery Time`~delivery_time$`Sorting Time`)
summary(slr)
####
#Exponential Model

# x = st and y = log(dt)
attach(delivery_time)
View(delivery_time)
plot(st, log(dt))

cor(st, log(dt))


slr1 <- lm(log(dt) ~ st)

summary(slr1)
predict <- predict(slr1)
predict(slr1,interval="predict")
#ggplot for exponential model
ggplot(data = slr1,aes(x =st,y = log(dt))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = slr1, aes(x=st, y=predict))

#########
# Polynomial model with 2 degree (quadratic model)

plot(st, dt)
plot(st*st,dt)

cor(st*st,dt)

plot(st*st, log(dt))

cor(st, log(dt))
cor(st*st, log(dt))
attach(delivery_time)
# lm(


slr2 <- lm(log(dt) ~ st + I(st*st))

summary(slr2)
#####
#Polynomial model with 3 degree (quadratic model)

slr3<-lm(log(dt)~st + I(st*st) + I(st*st*st))
summary(slr3)
#####
#Logrithamic Model
slr4<- lm(dt ~ log(st))
summary(slr4)
#######


# From above all model bulding we can conclude thate exponential model is some what significance
#where P-value < 0.05,MR^2 = 0.7109 is moderate
