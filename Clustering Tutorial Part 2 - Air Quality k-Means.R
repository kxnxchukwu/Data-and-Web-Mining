## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(tidy=TRUE,echo = TRUE)
# library(formatR)

setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 3 Clustering")
getwd()


## ------------------------------------------------------------------------
require("datasets")
data("airquality")
str(airquality)
head(airquality)

## ----PreProcess dataset--------------------------------------------------
##Remove extra attributes
##Let's remove "Day" attribute from the dataset.
airquality$Day<- NULL

head(airquality)
summary(airquality)

## ----Find and impute missing values. Normalize the dataset.--------------
##We will find predictors that have missing values. We then need to impute those missing values(NA) with their monthly average(just to keep it simple, there are other methods though!)
col1<- mapply(anyNA,airquality) # apply function anyNA() on all columns of airquality dataset
col1
##The output shows that only Ozone and Solar.R attributes have NA i.e. some missing value.

## ------------------------------------------------------------------------
for (i in 1:nrow(airquality)){
  if(is.na(airquality[i,"Ozone"])){
    airquality[i,"Ozone"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Ozone"],na.rm = TRUE)}
# Impute monthly mean in Solar.R
    if(is.na(airquality[i,"Solar.R"])){
    airquality[i,"Solar.R"]<- mean(airquality[which(airquality[,"Month"]==airquality[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
}

## ------------------------------------------------------------------------
#Normalize the predictor attributes so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))}
airquality[,1:4]<- normalize(airquality[,1:4]) 
# replace contents of dataset with normalized values

## ------------------------------------------------------------------------
head(airquality)


## ------------------------------------------------------------------------
result<- kmeans(airquality[c(1,2,3,4)],3) # apply k-means algorithm using first 4 attributes and with k=3(no. of required clusters)
result$size # gives no. of records in each cluster
result$center

## ------------------------------------------------------------------------
par(mfrow=c(1,2), mar=c(5,4,2,2))
plot(airquality[,1:2], col=result$cluster) # Plot to see how Ozone and Solar.R data points have been distributed in clusters
plot(airquality[,1:2], col=airquality$Month) 


## ------------------------------------------------------------------------
plot(airquality[,3:4], col=result$cluster) # Plot to see how Wind and Temp data points have been distributed in clusters
plot(airquality[,3:4], col=airquality$Month)
## ------------------------------------------------------------------------
plot(airquality[,], col=result$cluster) # Plot to see all attribute combinations

par(mfrow=c(1,1))

