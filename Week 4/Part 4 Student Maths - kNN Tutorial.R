## ----setup, include=FALSE, echo=TRUE-------------------------------------
knitr::opts_chunk$set(tidy=TRUE, echo = TRUE)
library(formatR)

getwd() 

setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 4 Classification")


## --STEP 1--echo=TRUE-----------------------------------------------------------
#read in the .csv file using the url() function
data <- read.table("student-mat.csv",sep=";",header=TRUE)

#change all variable names to lowercase
var.names.data <-tolower(colnames(data))
colnames(data) <- var.names.data
head(data)

## --STEP 2----------------------------------------------------------------------
install.packages("class", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("e1071", dependencies = TRUE)
install.packages("FNN", dependencies = TRUE)
install.packages("gmodels", dependencies = TRUE)
install.packages("psych", dependencies = TRUE)
install.packages("data.table", dependencies=TRUE)
install.packages("BiocManager", dependencies= TRUE)

## --STEP 3--echo=TRUE-----------------------------------------------------------
#libraries needed
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)
library(plyr)

## --STEP 4--echo=TRUE-----------------------------------------------------------
data_class <- data

## ----echo=TRUE-----------------------------------------------------------
# put outcome in its own object
mjob_outcome <- data_class %>% select(mjob)

# remove original variable from the data set
data_class <-  data_class %>% select(-mjob)
data_class$mjob <- NULL


## ---STEP 5---------------------------------------------------------------------
str(data_class)

## --STEP 6 --echo=TRUE Scale the variables-----------------------------------------------------------
data_class[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "dalc", "walc", "health", "absences", "g1", "g2", "g3")] <- scale(data_class[, c("age", "medu", "fedu", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "dalc", "walc", "health", "absences", "g1", "g2", "g3")])

head(data_class)

## --STEP 7--echo=TRUE-----------------------------------------------------------
str(data_class)

## ----echo=TRUE-----------------------------------------------------------
data_class$schoolsup <- ifelse(data_class$schoolsup == "yes", 1, 0)
data_class$famsup <- ifelse(data_class$famsup == "yes", 1, 0)
data_class$paid <- ifelse(data_class$paid == "yes", 1, 0)
data_class$activities <- ifelse(data_class$activities == "yes", 1, 0)
data_class$nursery <- ifelse(data_class$nursery == "yes", 1, 0)
data_class$higher <- ifelse(data_class$higher == "yes", 1, 0)
data_class$internet <- ifelse(data_class$internet == "yes", 1, 0)
data_class$romantic <- ifelse(data_class$romantic == "yes", 1, 0)

## ----echo=TRUE-----------------------------------------------------------
data_class$school <- dummy.code(data_class$school)
data_class$sex <- dummy.code(data_class$sex)
data_class$address <- dummy.code(data_class$address)
data_class$famsize <- dummy.code(data_class$famsize)
data_class$pstatus <- dummy.code(data_class$pstatus)

## ------------------------------------------------------------------------
fjob <- as.data.frame(dummy.code(data_class$fjob))
reason <- as.data.frame(dummy.code(data_class$reason))
guardian <- as.data.frame(dummy.code(data_class$guardian))

## ------------------------------------------------------------------------
fjob <- dplyr::rename(fjob, other_fjob = other)
fjob <- dplyr::rename(fjob, health_fjob = health)

reason <- dplyr::rename(reason, other_reason = other)

guardian <- dplyr::rename(guardian, other_guardian = other)


## ---STEP 8---------------------------------------------------------------------
data_class <- cbind(data_class, fjob, guardian, reason)

## ---STEP 9---------------------------------------------------------------------
data_class <- data_class %>% select(-one_of(c("fjob", "guardian", "reason")))

head(data_class)

##########################
## End of data Prep
##########################

## ------------------------------------------------------------------------
set.seed(1234) # set the seed to make the partition reproducible

# 75% of the sample size = 296
smp_size <- floor(0.75 * nrow(data_class))

train_ind <- sample(seq_len(nrow(data_class)), size = smp_size)

# creating test and training sets that contain all of the predictors
class_pred_train <- data_class[train_ind, ]
class_pred_test <- data_class[-train_ind, ]

## ------------------------------------------------------------------------
mjob_outcome_train <- mjob_outcome[train_ind, ]
mjob_outcome_test <- mjob_outcome[-train_ind, ]

## ----tidy=TRUE, results="asis"----sq root of 296 = 17---------------------------------------
mjob_pred_knn <- knn(train = class_pred_train, test = class_pred_test, cl = mjob_outcome_train, k=17)

## --STEP 11--results="asis"------------------------------------------------------
# put "mjob_outcome_test" in a data frame
mjob_outcome_test <- data.frame(mjob_outcome_test)

# merge "mjob_pred_knn" and "mjob_outcome_test" 
class_comparison <- data.frame(mjob_pred_knn, mjob_outcome_test)

# specify column names for "class_comparison"
names(class_comparison) <- c("PredictedMjob", "ObservedMjob")

# inspect "class_comparison" 
head(class_comparison)

## ----tidy=TRUE, results= "asis"------------------------------------------
# create table examining model accuracy
CrossTable(x = class_comparison$ObservedMjob, y = class_comparison$PredictedMjob, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)

## --STEP 13--tidy=TRUE, results="asis"-------------------------------------------
mjob_pred_caret <- train(class_pred_train, mjob_outcome_train, method = "knn", preProcess = c("center","scale"))

## ----tidy=TRUE, results="asis", echo=TRUE--------------------------------
mjob_pred_caret

## ----tidy=TRUE, results="asis", echo=TRUE--------------------------------
plot(mjob_pred_caret)

## ----tidy=TRUE, results="asis", echo=TRUE--------------------------------
library(BiocManager)
library(caret)
knnPredict <- predict(mjob_pred_caret, newdata = class_pred_test) 

## --STEP 14--tidy=TRUE, results="asis"-------------------------------------------
library(caret)
library(ggplot2)
library(data.table)
library(Matrix)
confusion_matrix <- confusionMatrix(table(knnPredict, mjob_outcome_test$mjob_outcome_test))

## ------------------------------------------------------------------------
confusion_matrix

