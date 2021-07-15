#Please change the folder path as  per your local machine
# File -> Open File -> Titanic Dataset....Yes

titanic_r <- titanic.raw

titanic_r[1:3,]
str(titanic_r)

table(titanic_r$Age,titanic_r$Survived)
require(arules)

# Visulization
require(arulesViz)

rule <- apriori(titanic_r[2:4], 
                # min support & confidence
                parameter=list(minlen=2, supp=0.001, conf=0.001),  
                appearance = list(default = "lhs", rhs=c("Survived=Yes","Survived=No")))

inspect(rule)

sort.rule <- sort(rule, by="lift")
inspect(sort.rule)


plot(sort.rule, method="graph", control=list(nodeCol="red", edgeCol="blue"))


rule1 <- apriori(titanic_r[2:4], 
                 # min support & confidence
                 parameter=list(minlen=2, supp=0.001, conf=0.001),  
                 appearance = list(lhs=c("Age=Child","Age=Adult"), rhs=c("Survived=Yes")))

inspect(rule1)

sort.rule1 <- sort(rule1, by="lift")
inspect(sort.rule1)

plot(sort.rule1)

plot(sort.rule1, method="graph", control=list(nodeCol="red", edgeCol="blue"))

plot(sort.rule1, method="grouped", control=list(col=2))
