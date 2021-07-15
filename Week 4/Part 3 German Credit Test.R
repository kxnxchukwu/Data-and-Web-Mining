## ------------------------------------------------------------------------
knitr::opts_chunk$set(tidy=TRUE, echo = TRUE)
library(formatR)

install.packages("caret")
install.packages("e1071")
library(e1071)
getwd()

setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 4 Classification")

## ----setup, include=FALSE, tidy=TRUE, echo=TRUE, results='asis'----------
gc <- read.csv("german_credit.csv") 
# reading csv data files from defined directory as file has already downloaded and stored in the directory

## Taking back-up of the input file, in case the original data is required later
gc.bkup <- gc
head (gc) 


# To check top 6 values of all the variables in data set.)

## ------------------------------------------------------------------------
##Step-2 Preparing and exploring the data. Dataset variable details are available on this link (https://archive.ics.uci.edu/ml/datasets/Statlog+%28German+Credit+Data%29 )
##There are 20 attributes/features, so for the simplicity we will select relevant attributes which are as follows :
##Age (numeric)
##Sex (text: male, female)
##Job (numeric: 0 - unskilled and non-resident, 1 - unskilled and resident, 2 - skilled, 3 - highly skilled)
##Saving accounts (text - little, moderate, quite rich, rich)
##Credit amount (numeric, in DM)
##Duration (numeric, in month)
##Purpose (text: car, furniture/equipment, radio/TV, domestic appliances, repairs, education, business, vacation/others)
##Note: All the attributes' value are already converted to numeric and same data is available on the link as mentioned above.

str(gc)

## --STEP 3--tidy=TRUE, echo=TRUE, results='asis'--------------------------------
#Feature/Attribute selection

#The variable 'Creditability' is our target variable i.e. this variable will determine whether bank manager will approve a loan based on the 7 Attributes.

gc.subset <- gc[c('Creditability','Age..years.','Sex...Marital.Status','Occupation','Account.Balance','Credit.Amount','Length.of.current.employment','Purpose')]
head(gc.subset)
str(gc.subset)


## ----tidy=TRUE, echo=TRUE, results='asis'--------------------------------
#Data normalistion to avoid biasness as the value sclae of 'Credit.Amount'is in thousand whereas other attribute's value are in 2 digits or 1 digit.
library(class)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) } # creating a normalize function for easy convertion.

## --STEP 5--tidy=TRUE, echo=TRUE------------------------------------------------
gc.subset.n<- as.data.frame(lapply(gc.subset[,2:8], normalize)) # lapply creates list that is why it is converted to dataframe and it applies defined fundtion (which is 'normalize') to all the list values which is here column 2 to 8 as first column is target/response.

## ----echo=TRUE-----------------------------------------------------------
head(gc.subset.n)
str(gc.subset.n)


## --STEP 6--tidy=TRUE, echo=TRUE, results='asis'--------------------------------
#Now all attributes having value in the range 0 to 1 which is normalised data and 'Creditability' column has been removed as sample value starts form column 2.

#Creating Training and Test data set. Training data will be used to build model whereas test data will be used for validation and optimisation of model by tuning k value.

set.seed(123)  # To get the same random sample
dat.d <- sample(1:nrow(gc.subset.n),size=nrow(gc.subset.n)*0.7,replace = FALSE) #random selection of 70% data.

train.gc <- gc.subset[dat.d,] # 70% training data
test.gc <- gc.subset[-dat.d,] # remaining 30% test data

#Now creating seperate dataframe for 'Creditability' feature which is our target.
train.gc_labels <- gc.subset[dat.d,1]
test.gc_labels  <- gc.subset[-dat.d,1]   

## ----tidy=TRUE, echo=TRUE, results='asis'--------------------------------
#install.packages(class) # to install class packages as it carries kNN function
library(class)          # to call class package

NROW(train.gc_labels)   # to find the number of observation

## ----echo=TRUE-----------------------------------------------------------
#To identify optimum value of k, generally square root of total no of observations (700) which is 26.45 is taken, so will try with 26, 27 then will check for optimal value of k.

knn.26 <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=26)
knn.27 <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=27)

## ------------------------------------------------------------------------
## Let's calculate the proportion of correct classification for k = 26, 27 

ACC.26 <- 100 * sum(test.gc_labels == knn.26)/NROW(test.gc_labels)  # For knn = 26
ACC.27 <- 100 * sum(test.gc_labels == knn.27)/NROW(test.gc_labels)  # For knn = 27
ACC.26    #Accuracy is 68.67%

## ------------------------------------------------------------------------
table(knn.26 ,test.gc_labels)  # to check prediction against actual value in tabular form

## ------------------------------------------------------------------------
# 11 & 192 are the correct prediction against actual wheras 90 & 7 are wrong prediction against actual.
table(knn.27 ,test.gc_labels)  # to check prediction against actual value in tabular form

## --STEP 10----------------------------------------------------------------------
library(caret)
confusionMatrix(table(knn.26 ,test.gc_labels))

## --STEP 11----------------------------------------------------------------------
confusionMatrix(table(knn.27 ,test.gc_labels))

## --STEP 12--echo=TRUE-----------------------------------------------------------
i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:28){ 
    knn.mod <-  knn(train=train.gc, test=test.gc, cl=train.gc_labels, k=i)
    k.optm[i] <- 100 * sum(test.gc_labels == knn.mod)/NROW(test.gc_labels)
    k=i  
    cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

## ------------------------------------------------------------------------
# Maximum accuracy at k=17   

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value

