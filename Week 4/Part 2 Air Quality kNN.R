## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(tidy=TRUE,echo = TRUE)

setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 4 Classification")



## ------------------------------------------------------------------------
require("class")
library(formatR)

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

## ---STEP 6---------------------------------------------------------------------
#Normalize the predictor attributes so that no particular attribute has more impact on clustering algorithm than others.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))}
airquality[,1:4]<- normalize(airquality[,1:4]) 
# replace contents of dataset with normalized values

## ------------------------------------------------------------------------
class<- data.frame("month"=airquality$Month)
names(class)= "Month"
airquality[,5]<- NULL #remove "Month" from airquality
head(airquality)

## ---STEP 7---------------------------------------------------------------------
set.seed(999) # required to reproduce the results
rnum<- sample(rep(1:153))
airquality<- airquality[rnum,] #randomize "airquality" dataset
class<- as.data.frame(class[rnum,]) #apply same randomization on "class" attribute

## ---STEP 8---------------------------------------------------------------------
airquality.train<- airquality[1:130,]
airquality.train.target<- class[1:130,]
airquality.test<- airquality[131:153,]
airquality.test.target<- class[131:153,]

## ---STEP 9---------------------------------------------------------------------
neigh<- round(sqrt(nrow(airquality)))+1 
# no. of neighbours are generally square root of total number of instances
model<- knn(train = airquality.train,  test = airquality.test, cl=airquality.train.target, k=neigh) 
# apply knn algorithm

## ---STEP 10---------------------------------------------------------------------
table(airquality.test.target, model)

table(airquality.test.target)
table(model)

## ------------------------------------------------------------------------
mean(airquality.test.target== model)

