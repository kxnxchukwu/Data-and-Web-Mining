## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

getwd()

setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 3 Clustering")

## ------------------------------------------------------------------------
require("datasets")
data("iris") # load Iris Dataset
str(iris) #view structure of dataset

## ------------------------------------------------------------------------
summary(iris) # View statistical information about the dataset

## ------------------------------------------------------------------------
iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new)
summary(iris.new)

## ------------------------------------------------------------------------
head(iris.class)

## ------------------------------------------------------------------------
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

## ------------------------------------------------------------------------
iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

## ------------------------------------------------------------------------
result<- kmeans(iris.new,3) #aplly k-means algorithm with no. of centroids(k)=3
result$size # gives no. of records in each cluster

## ------------------------------------------------------------------------
result$centers # gives value of cluster center datapoint value(3 centers for k=3)

## ------------------------------------------------------------------------
result$cluster #gives cluster vector showing the custer where each record falls

## ------------------------------------------------------------------------
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)
# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)
# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=result$cluster)
# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)

## ------------------------------------------------------------------------
table(result$cluster,iris.class)

par(mfrow=c(1,1))
