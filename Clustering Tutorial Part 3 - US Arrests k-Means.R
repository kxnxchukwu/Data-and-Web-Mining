## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
install.packages("factoextra",dependencies = TRUE)
## ------------------------------------------------------------------------
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

## ------------------------------------------------------------------------
df <- USArrests

## ------------------------------------------------------------------------
df <- na.omit(df)
head(df)
str(df)

## ------------------------------------------------------------------------
df <- scale(df)
head(df)

## ------------------------------------------------------------------------
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## ------------------------------------------------------------------------
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

## ------------------------------------------------------------------------
k2

## ------------------------------------------------------------------------
fviz_cluster(k2, data = df)


## ------------------------------------------------------------------------
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

##### Some extra plots Murder vs Assault
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(Murder, Assault, color = factor(cluster), label = state)) +
  geom_text()

##### Some extra plots UrbanPop vs Assault
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Assault, color = factor(cluster), label = state)) +
  geom_text()

##########
# Can combine the generated cluster label with the original data
##########
df2 = cbind(df,k2$cluster)

colnames(df2) = c("Murder", "Assault", "UrbanPop", "Rape", "Cluster")
df2

write.csv(df2, file='df2.csv')

## ------------------------------------------------------------------------
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

## ------------------------------------------------------------------------
set.seed(123)
par(mfrow=c(1,1))
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")

## ------------------------------------------------------------------------
set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")

## ------------------------------------------------------------------------
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

## ------------------------------------------------------------------------
fviz_cluster(final, data = df)


##########
# Can combine the generated cluster label with the original data
##########
df3 = cbind(df,final$cluster)

colnames(df3) = c("Murder", "Assault", "UrbanPop", "Rape", "Cluster")
df3
