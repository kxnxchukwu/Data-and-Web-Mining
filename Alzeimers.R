
# the BayesLCA data set - "Symptoms of Patients Suffering from Alzheimer's Syndrome"
install.packages("BayesLCA")
install.packages("arulesViz")
install.packages("gplots")
install.packages("fpc")
install.packages("data.table")

library(arules)
library(BayesLCA)
library(arulesViz)
data(Alzheimer)
Alzheimer[1:20,]

Alzheimer.trans <- as(as.matrix(Alzheimer),"transactions")
Alzheimer.trans

str(Alzheimer)

# This paper provides an excellent outline of the BayesLCA package:
# BayesLCA: An R Package for Bayesian Latent Class Analysis :- http://www.jstatsoft.org/v61/i13/paper

# The key task when finding association rules is to find all subsets of the items that occur with probability 
# greater than s, where s is the support threshold.

# A key result which is used in the a priori algorithm is:
# P(S1,S2) <= min{(S1),P(S2)}
# where S1 and S2 are subsets of items i.e. the Frechet upper bound.

# There are 3 main qualities of Association Rules
# Support : this is a measures how often the combination of items in A and B co-occur in the observations.
# Confidence : measures the probability of B occurring in a transaction given that A does.
# Lift : The ratio of the probability A and B occuring together against the probability of A or B occuring independently 
# i.e. Lift > 1 means the probability of A and B occuring together is greater than what we would expect if they were independent events

fitalz <- apriori(Alzheimer.trans,parameter=list(confidence=0.5,support=0.01, minlen=2,maxlen=5))
fitalz <- sort(fitalz,by="support")
inspect(fitalz)

# Visualisation

library(arulesViz)

# Scatter Plot
plot(fitalz, measure=c("support", "lift"), shading="confidence")

plot(fitalz, measure=c("support", "confidence"), shading="lift")


# Matrix Plot
subfit <- fitalz[quality(fitalz)$confidence > 0.8]
plot(subfit, method="matrix", measure="lift", control=list(reorder = "measure"))

inspect(subfit)

# ScatterPlot
plot(subfit, method="scatterplot")

# Graph Plot
plot(subfit, method="graph")

     
# Summary

# We have seen that the range of values that lift takes can depend on the minimum support (s) and confdence (c) 
# thresholds used.

