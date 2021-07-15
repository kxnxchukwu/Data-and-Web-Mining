library(dplyr)

getwd()

setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 5 Decision Trees")

data_titanic <- read.csv('titanic_2020.csv',header=T,na.strings=c(""))

data_titanic$Survived = as.factor(data_titanic$Survived)

np <- ceiling(0.1*nrow(data_titanic))
np

set.seed(4)
test.index <- sample(1:nrow(data_titanic), np)
#test.index <- sort(sample(1:nrow(data_titanic), np))

data_test <- data_titanic[test.index, ]
data_train <- data_titanic[-test.index, ]


#Step 3 - Train the model
library(randomForest)
library(caret)
library(e1071)


#Step 4 - define controls
# Define the control
trControl <- trainControl(method = "cv", number = 10, search = "grid")


#Step 5 - build models with controls
set.seed(1234)
# Run the model


rf_default <- caret::train(Survived~.,
                    data = data_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl,
                    na.action=na.exclude)
# Print the results
print(rf_default)

#Step 6 - Search for best mtry
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- caret::train(Survived~.,
                 data = data_train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300,
                 na.action=na.exclude)
print(rf_mtry)

# Store best value of mtry
rf_mtry$bestTune$mtry

max(rf_mtry$results$Accuracy)

best_mtry<- rf_mtry$bestTune$mtry 
best_mtry

# Step 7 Search for best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 20)) {
  set.seed(1234)
  rf_maxnode <- caret::train(Survived~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300,
                      na.action=na.exclude)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

#Step 8 - search for best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- caret::train(Survived~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 15,
                       ntree = ntree,
                       na.action=na.exclude)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#Step 9 - display best settings for model
best_mtry
store_maxnode
store_maxtrees
summary(results_tree)


fit_rf <- caret::train(Survived~.,
                data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 600,
                maxnodes = 15,
                na.action=na.exclude)

#Step 10 evaluate model
prediction <- predict(fit_rf, data_test[,2:8])
confusionMatrix(prediction, data_test$Survived)



