# Simple examples
#Load arules and Groceries data

library(arules)
data(Groceries)

#Items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=1,maxlen=1))
fit <- sort(fit,by="support")
inspect(fit)

#Pairs of items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

#Pairs of items with support greater than threshold (confidence threshold too)

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

# Triples of items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# Triples of items with support greater than threshold (confidence threshold too)

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# Quadruples of items with support greater than threshold

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=4,maxlen=4))
fit <- sort(fit,by="support")
inspect(fit)

