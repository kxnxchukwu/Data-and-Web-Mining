#Case study: Forensic Glass Data

#Set random number seed
set.seed(1)

#Load the forensic glass data
library(MASS)
data(fgl)
?fgl 

# Load many required packages
library(rpart)
library(adabag)
library(randomForest)
library(partykit)

#Split data into training and test
N<-nrow(fgl)
indtrain<-sample(1:N,size=0.75*N)
indtrain<-sort(indtrain)
indtest<-setdiff(1:N,indtrain)

# Look at performance of classification trees
fit.r <- rpart(type~.,data=fgl,subset=indtrain)
plot(as.party(fit.r))
pred.r <- predict(fit.r,newdata=fgl,type="class")

# Test data
table(fgl$type[indtest],pred.r[indtest])
sum(fgl$type[indtest]==pred.r[indtest])/length(indtest)

# Training data
table(fgl$type[indtrain],pred.r[indtrain])
sum(fgl$type[indtrain]==pred.r[indtrain])/length(indtrain)



# Look at bagging classification trees....bagging model takes some time to generate
fit.b <- bagging(type~.,data=fgl[indtrain,])
pred.b <- predict(fit.b,newdata=fgl,type="class")$class

# Test data
table(fgl$type[indtest],pred.b[indtest])
sum(fgl$type[indtest]==pred.b[indtest])/length(indtest)

#Training data

table(fgl$type[indtrain],pred.b[indtrain])
sum(fgl$type[indtrain]==pred.b[indtrain])/length(indtrain)




# Look at random forest classifier
fit.rf <- randomForest(type~.,data=fgl,subset=indtrain)
pred.rf <- predict(fit.rf,newdata=fgl,type="class")

#Test data
table(fgl$type[indtest],pred.rf[indtest])
sum(fgl$type[indtest]==pred.rf[indtest])/length(indtest)

#Training data
table(fgl$type[indtrain],pred.rf[indtrain])
sum(fgl$type[indtrain]==pred.rf[indtrain])/length(indtrain)

