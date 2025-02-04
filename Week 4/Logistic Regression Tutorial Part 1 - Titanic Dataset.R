## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
install.packages("Amelia", dependencies = TRUE)
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

## ------------------------------------------------------------------------
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

## ------------------------------------------------------------------------
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

## ------------------------------------------------------------------------
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

## ------------------------------------------------------------------------
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

## ------------------------------------------------------------------------
is.factor(data$Sex)
is.factor(data$Embarked)

## ------------------------------------------------------------------------
contrasts(data$Sex)
contrasts(data$Embarked)

## ------------------------------------------------------------------------
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

## ------------------------------------------------------------------------
train <- data[1:800,]
test <- data[801:889,]

## ------------------------------------------------------------------------
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

## ------------------------------------------------------------------------
summary(model)

## ------------------------------------------------------------------------
anova(model, test="Chisq")

## ------------------------------------------------------------------------
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

