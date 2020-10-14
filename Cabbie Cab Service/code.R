rm(list = ls(all=TRUE))
setwd("C:\\Users\\MANISH\\Desktop\\HackerEarth\\Cabbie Cab Service")

mydata = read.csv2("train.csv", sep = ',', header = TRUE, na.strings = "")
str(mydata)
summary(mydata)

#Preprocessing
#Removing columns with too many or very few missing values
mydata = mydata[-c(1,13)]
final_Data = mydata

#Handling missing values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(final_Data,1,pMiss)
apply(final_Data,2,pMiss)

library(dplyr)

final_Data = filter(final_Data, !is.na(pickup_longitude))
final_Data = filter(final_Data, !is.na(pickup_latitude))
final_Data = filter(final_Data, !is.na(dropoff_longitude))
final_Data = filter(final_Data, !is.na(dropoff_latitude))
final_Data = filter(final_Data, !is.na(new_user))
summary(final_Data)
location = final_Data[c(9,10)]

head(location)
######Feature Engineering######

##Clustering of latitude and longtitudes
location = na.omit(location)
str(location)
location$pickup_latitude = as.numeric(location$pickup_latitude)
location$pickup_longitude = as.numeric(location$pickup_longitude)

is.numeric(location$pickup_longitude)
location = scale(location)

gc()
wss <- 0
for (i in 1:40) {
  wss[i] <- sum(kmeans(location,centers=i, algorithm = "MacQueen")$withinss)
}

#Scree Plot
plot(1:40, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

##k = 14 looks good

loc_cluster <- kmeans(location, 14)
loc_cluster$centers
location = data.frame(location, loc_cluster$cluster)

#Adding to original dataset
#Adding cluster to final data
final_Data = data.frame(final_Data,cluster = location$loc_cluster.cluster)
summary(final_Data)

#Central Imputation
library(DMwR2)
final_Data =  centralImputation(final_Data)
summary(final_Data)
sum(is.na(final_Data))

###Feature engineering from date

#Converting to datetime
final_Data$pickup_datetime = as.POSIXlt(final_Data$pickup_datetime)
final_Data$dropoff_datetime = as.POSIXlt(final_Data$dropoff_datetime)

ride_time = final_Data$dropoff_datetime - final_Data$pickup_datetime
head(ride_time)
final_Data = data.frame(final_Data, ride_time)
pickup_time = unclass(final_Data$pickup_datetime)
head(pickup_time)
year = pickup_time$year
head(year)
month = pickup_time$mon
head(month)

tm1.lt <- as.POSIXlt("2013-07-24 23:55:26")
unclass(tm1.lt)
unlist(tm1.lt)
tm1.lt$sec


## Create 10 fold cross validation
library(caret)
folds =  createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
