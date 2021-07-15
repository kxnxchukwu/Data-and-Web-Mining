#Data Mining Code (Association Rules)
#
#
# This set of notes shows how we can use association rule mining to analyze voting data. 
# The example uses a famous data from the US Congress where there sixteen key votes from 1984 are considered. 
# There are 435 members of congress and they are either Democrat or Republican.
# We can investigate if there are structures in the voting behaviour of the congress members.
# Further details on the data are available here: http://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records

#Turn on the arules package

library(arules)

#Read in data from the web

dat <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

# Give sensible column names to data.

colnames(dat) <- c("Party",paste("Vote",1:16,sep=""))

#Look at the data

dat

#Note that the ?'s are people being absent from the vote.
# We recode the ?'s as n's

dat[dat=="?"] <- "n"

# I will remove "party" from the data so that we only analyze votes.

dat<-dat[,-1]

# Let's make a binary version of the data (1="y" and 0="n")

datnew <- 1*(dat=="y")
colnames(datnew) <- paste("Yes",1:16,sep="")

#Getting the data into the transaction format that arules uses. We look at the data again.

votes <- as(datnew,"transactions")
inspect(votes)

# We mine all assocation rules with support greater than 0.4 and confidence greater than 0.8

fit<-apriori(votes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

# Note that we are treating the votes in an asymmetric manner. 
# It could be argued that treating "y" as the item of interest is arbitrary.
# We could instead move the focus to the "n"s. 

datnew <- 1*(dat=="n")
colnames(datnew)<-paste("No",1:16,sep="")

#Getting the data into the format that arules uses.

novotes <- as(datnew,"transactions")
inspect(novotes)

fit<-apriori(novotes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)
 
# It could be further argued that treating "n" or "y" is still arbitrary.
# We could construct the dataset with both "y" and "n" votes recorded. 

allvotes<-merge(votes,novotes)
inspect(allvotes)

fit<-apriori(allvotes,parameter=list(support=0.4,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

# We get many more rules so we may wish to consider changing the parameter settings. 
# In particular, we should increase the support threshold. 

fit<-apriori(allvotes,parameter=list(support=0.45,confidence=0.8))
fit<-sort(fit,by="support")
inspect(fit)

