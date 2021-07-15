## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

## ------------------------------------------------------------------------
summary(mydata)

## ------------------------------------------------------------------------
sapply(mydata, sd)

## ------------------------------------------------------------------------
## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

## ------------------------------------------------------------------------
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")

## ------------------------------------------------------------------------
summary(mylogit)

## ------------------------------------------------------------------------
## CIs using profiled log-likelihood
confint(mylogit)

## ------------------------------------------------------------------------
## CIs using standard errors
confint.default(mylogit)

## ------------------------------------------------------------------------
install.packages("aod", dependencies = TRUE)
library(aod)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

## ------------------------------------------------------------------------
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

## ------------------------------------------------------------------------
## odds ratios only
exp(coef(mylogit))

## ------------------------------------------------------------------------
## odds ratios only
exp(coef(mylogit))
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

