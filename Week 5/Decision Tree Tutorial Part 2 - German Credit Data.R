## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

getwd()
setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 5 Decision Trees")

## --STEP 1----------------------------------------------------------------------
#read data file
mydata=read.csv("german_credit.csv")

## ------------------------------------------------------------------------
summary(mydata)

## ------------------------------------------------------------------------
# Check attributes of data
str(mydata)

## --STEP 2----------------------------------------------------------------------
# Check number of rows and columns
dim(mydata)

## --STEP 3----------------------------------------------------------------------
# Make dependent variable as a factor (categorical)
mydata$Creditability = as.factor(mydata$Creditability)

## --STEP 4----------------------------------------------------------------------
# Split data into training (70%) and validation (30%)
dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
train<-mydata[dt,]
val<-mydata[-dt,] 
# Check number of rows in training data set
nrow(train)

## --STEP 5--results='hide'------------------------------------------------------
# To view dataset
edit(train)

## --STEP 6----------------------------------------------------------------------
# Decision Tree Model
library(rpart)
mtree <- rpart(Creditability~., data = train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))

## ------------------------------------------------------------------------
mtree

## --STEP 7----------------------------------------------------------------------
#Plot tree
plot(mtree)
text(mtree)

install.packages("rattle",dependencies = TRUE)
#Beautify tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#view1
prp(mtree, faclen = 0, cex = 0.8, extra = 1)

## ------------------------------------------------------------------------
#view2 - total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}

prp(mtree, faclen = 0, cex = 0.8, node.fun=tot_count)

## ------------------------------------------------------------------------
#view3- fancy Plot
library(rattle)
#library(gKt)
#rattle()
fancyRpartPlot(mtree)

## --STEP 8----------------------------------------------------------------------
printcp(mtree)
bestcp <- mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

mtree$cptable
bestcp

# Prune the tree using the best cp.
pruned <- prune(mtree, cp = bestcp)


## --Step 9----------------------------------------------------------------------
# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

## --STEP 10----------------------------------------------------------------------
# confusion matrix (training data)
conf.matrix <- table(train$Creditability, predict(pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

table(train$Creditability)


## --STEP 11----------------------------------------------------------------------
#Scoring
library(ROCR)
val1 = predict(pruned, val, type = "prob")
val1

head(val1)
head(val$Creditability)

#Storing Model Performance Scores
pred_val <-prediction(val1[,2],val$Creditability)

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
View(perf_val)

# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

# Plot the ROC curve
plot(perf_val, col = "red", lwd = 1.5)

#Calculating KS statistics
ks1.tree <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks1.tree

## --STEP 12----------------------------------------------------------------------
# Advanced Plot
prp(pruned, main="Beautiful Tree",
    extra=106, 
    nn=TRUE, 
    fallen.leaves=TRUE, 
    branch=.5, 
    faclen=0, 
    trace=1, 
    shadow.col="gray", 
    branch.lty=3, 
    split.cex=1.2, 
    split.prefix="is ", 
    split.suffix="?", 
    split.box.col="lightgray", 
    split.border.col="darkgray", 
    split.round=.5)

