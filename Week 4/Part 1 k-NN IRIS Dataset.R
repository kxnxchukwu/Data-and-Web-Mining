## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

getwd()
setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 4 Classification")

## ----load data-----------------------------------------------------------
dfi <- data(iris) ##load data
head(iris) 

## ----Generate Random Number that is 90% of the total number of rows in the dataset----
set.seed(1234)
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 
 
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
 
##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)

## ----Extract training dataset--------------------------------------------

iris_train <- iris_norm[ran,] 

## ----Extract testing dataset---------------------------------------------
iris_test <- iris_norm[-ran,] 

## ----Extract 5th column for training and testing datasets----------------
iris_train_category <- iris[ran,5] 
 
##extract 5th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,5]

## ----Load package class(ification)--------------------------------------------------
library(class)

## ----Run knn function----------------------------------------------------
pr <- knn(iris_train,iris_test,cl=iris_train_category,k=13)

## To view how the test instances were classified
pr

## ----Create confusion matrix---------------------------------------------

tab <- table(pr,iris_test_category)
tab

## ----Function to calculate accuracy of predictions-----------------------
 accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
 accuracy(tab)


