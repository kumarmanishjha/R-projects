setwd("C:\\Users\\MANISH\\Desktop\\Analyticsvidhya\\Fractal")

rm(list=ls(all=T))

library(data.table)
library(lubridate)
library(ggplot2)

train = fread("train.csv")
a = data.table(x = train$Number_Of_Sales, y =train$Price)
a = a[order(y)]
plot(a$x, a$y) #Inverse curve
par(mfrow = c(1,1))
b = data.table(x = train$Category_1, y =train$Number_Of_Sales)
b = b[order(x)]
b_1 = subset(b, x != 0)
plot(b_1$x, b_1$y)

train_sub1 = subset(train, Category_3 == 0)
plot(train_sub1$Price, train_sub1$Number_Of_Sales)
ggplot(train_sub1, aes(train_sub1$Price, train_sub1$Number_Of_Sales)) + geom_smooth(method = loess)
