#C5.0 Algorithm


getwd() 
setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 5 Decision Trees")

install.packages('C50')

library(C50)
data(iris)
np <- ceiling(0.1*nrow(iris))
np

set.seed(4)
test.index <- sample(1:nrow(iris), np)
#test.index <- sort(sample(1:nrow(iris), np))


iris.test <- iris[test.index, ]
iris.train <- iris[-test.index, ]

c <- C5.0Control(subset = FALSE,
                 bands = 0,
                 winnow = FALSE,
                 noGlobalPruning = FALSE,
                 CF = 0.25,
                 minCases = 2,
                 fuzzyThreshold = FALSE,
                 sample = 0,
                 seed = sample.int(4096, size = 1) -1L,
                 earlyStopping = TRUE
)
iris_treeModel <- C5.0(x = iris.train[, -5], y = iris.train$Species,control =c)
summary(iris_treeModel)

plot(iris_treeModel)

test.output <- predict(iris_treeModel, iris.test[, -5], type = "class")
test.output

n <- length(test.output)
number = 0
for ( i in 1:n){
  if(test.output[i] == iris.test[i, 5])
  {
    number=number+1}
}
test.accuracy = number/n*100
test.accuracy


