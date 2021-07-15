## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(tidy= TRUE, echo = TRUE)

getwd()
par(mfrow=c(1,1))
#install.packages("ggplot2",dependencies=TRUE)

## --STEP 2--tidy=TRUE, echo=TRUE------------------------------------------------
# Read data
data1 = read.csv("US Presidential Data.csv")
head(data1)
str(data1)

## --STEP 3----------------------------------------------------------------------
# load library
library(ggplot2)
library(caret)
library(e1071)

## ---STEP 4---------------------------------------------------------------------
# Transforming the dependent variable to a factor
data1$Win.Loss = as.factor(data1$Win.Loss)
str(data1)


## --STEP 5----------------------------------------------------------------------
#Partitioning the data into training and validation data
set.seed(101)
index = createDataPartition(data1$Win.Loss, p = 0.7, list = F )
train = data1[index,]
validation = data1[-index,]

## ---STEP 6---------------------------------------------------------------------
# Explore data
dim(train)
dim(validation)
names(train)
head(train)
head(validation)

## ---STEP 7---------------------------------------------------------------------
# Setting levels for both training and validation data
levels(train$Win.Loss) <- make.names(levels(factor(train$Win.Loss)))
levels(validation$Win.Loss) <- make.names(levels(factor(validation$Win.Loss)))

## ---STEP 8---------------------------------------------------------------------
# Setting up train controls
repeats = 3
numbers = 10
tunel = 10
set.seed(1234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

## ---STEP 9---------------------------------------------------------------------
model1 <- train(Win.Loss~. , data = train, method = "knn",
               preProcess = c("center","scale"),
               trControl = x,
               metric = "ROC",
               tuneLength = tunel)

# Summary of model
model1
plot(model1)

## ---STEP 10---------------------------------------------------------------------
# Validation

#install.packages("gplots")
#library(gplots)
valid_pred <- predict(model1,validation, type = "prob")
#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(valid_pred[,2],validation$Win.Loss)
# Calculating Area under Curve (AUC)
perf_val <- performance(pred_val,"auc")
View(perf_val)

# Plot AUC
perf_val1 <- performance(pred_val, "tpr", "fpr")
plot(perf_val1, col = "green", lwd = 1.5)
#Calculating KS statistics
ks <- max(attr(perf_val1, "y.values")[[1]] - (attr(perf_val1, "x.values")[[1]]))
ks

