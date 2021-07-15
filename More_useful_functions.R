# Intro to R - an overview of common things you need to be able to do
# Brian Buckley

# General
# To get the manual page for any command in R just use '?' before the command
# e.g.
?read.csv

# Where are you - use this a lot to make sure you load and save data to the right place
getwd() # stands for 'get working directory'

# Set the working directory to somewhere else - note for windows users R (and Python) 
# expect forward slashes - '/' rather than Microsoft back slashes - '\'
setwd("C:/Users/buckl/Downloads")
getwd() # check we have moved

# ******************* Built-in reference data sets
# We will use built-in reference data throughout the module
data()

# You can get details of a data set with the question mark
# e.g.
?barley

# Load the iris reference data set - note you don't assign to a variable
data(iris)

# Explore the data
str(iris) # This has 150 rows and 5 fields and is a data frame
head(iris, 10) # the top 10 rows
tail(iris, 10) # the last 10 rows

# Writing files - note if you have a correct getwd() you don't need the whole path
write.csv(iris, file="iris.csv") 

# Reading files
x <- read.csv("iris.csv", header=TRUE, stringsAsFactors=FALSE)

str(x) # same as str(iris) as expected

# You can also read/write Excel files, delimited text and special R binary files
# which are much smaller than CSV
# e.g.
saveRDS(iris, file="iris.rds") # for large data the RDS file is much smaller
x <- readRDS("iris.rds")
str(x) # yep it's cool

# You can also stream a whole R object to a binary file but that is advanced ;-)

# ******************* Basic Statistics

mean(iris$Sepal.Length) # mean
max(iris$Sepal.Length) # max
min(iris$Sepal.Length) # min
sd(iris$Sepal.Length) # standard deviation
cor(iris$Sepal.Length, iris$Sepal.Width) # correlation coefficient

rowMeans(iris[1,1:4]) # row 1, columns 1 to 4 since 5 is categorical
rowSums(iris[1,1:4])

colMeans(iris[1:150,1:4]) # column means for the numerical columns
colSums(iris[1:150,1:4])

summary(iris) # summary stats for the whole data
pairs(iris) # correlation plots for the whole data


# ******************* Data Manipulation

# Split a column into multiple columns
x <- matrix(iris$Sepal.Length, ncol=2) # you can use nrow= as well or together with ncol
x
x <- matrix(iris$Sepal.Length, ncol=3)
x
# BUT!
x <- matrix(iris$Sepal.Length, ncol=4) # error because a matrix has to be square

# split a row into columns
x <- matrix(iris[1, 1:4], nrow=2)

# ******************* Basic graphics

install.packages("ElemStatLearn")
library(ElemStatLearn)

# A basic scatter plot
plot(prostate$age, prostate$lcavol)

# Change the axis labels and add a title
plot(prostate$age, prostate$lcavol,
     xlab = 'Age',
     ylab = 'Log(cancer volume)',
     main = 'Scatter plot')

# Use e.g. paste or paste0 to add in objects
var_1 = 'age'
var_2 = 'lcavol'
plot(prostate[,var_1], prostate[,var_2],
     xlab = var_1,
     ylab = var_2,
     main = paste('Scatter plot of',var_1,'vs',var_2))

# Change the plotting type
plot(prostate[,var_1], prostate[,var_2],
     pch = 19)
plot(prostate[,var_1], prostate[,var_2],
     type = 'l') # Yuck
?pch # Look at different point types

# Changing colours
plot(prostate[,var_1], prostate[,var_2],
     pch = 19, col = 'blue')

# Transparency
plot(prostate[,var_1], prostate[,var_2],
     pch = 19, col = rgb(1, 0, 0, alpha = 0.2))

# Changing some options with par
par(mar = c(2, 2, 2, 2), las = 1) # Margins - see ?par for more
plot(prostate[,var_1], prostate[,var_2])
# Be careful - these options are persistent
par(mar = c(5, 4, 4, 2) + 0.1)

# Add to plots with points and lines
plot(prostate[,var_1], prostate[,var_2], type = 'n')
points(prostate[,var_1], prostate[,var_2], col='red')
lines(prostate[,var_1], prostate[,var_2], col='green')

# Add a legend
legend('topleft', legend = c('points', 'lines'),
       pch = c(1, -1),
       lty = c(-1, 1),
       col = c('red', 'green'))

# Histograms
hist(prostate$lweight)

# Better bins:
hist(prostate$lweight, breaks = 30)
hist(prostate$lweight, breaks = seq(2,5, by = 0.2))

# Better x axis
hist(prostate$lweight, breaks = seq(2,5, by = 0.2), xaxt = 'n')
axis(side = 1, at = seq(2, 5, by = 0.2))

# Bar charts (called bar plots)
table(prostate$gleason)
barplot(table(prostate$gleason)) # No axis labels
barplot(table(prostate$gleason), horiz = TRUE)

# Boxplots
boxplot(prostate$lpsa)

# Careful! - this does *not* give you what you might expect
boxplot(prostate$gleason, prostate$lpsa)
# Proper way
boxplot(prostate$lpsa ~ prostate$gleason) # this is a *formula*
# Lots of extra options regarding direction, style, shape, colour, etc. - read the manual

# ******************* Fancy graphics

# Extract just the training data
dat<-prostate[which(prostate$train == TRUE),][1:9]

# Use ggplot for nicer-looking plots
library(ggplot2)

# For convenience, create subset data for the specific plots
# The first is a density plot of lpsa as a factor of gleason level
df1 = data.frame(x=dat$lpsa, Gleason=as.factor(dat$gleason))

# The second is a density plot of lpsa as a factor of age (split into age-range) 
age_range = cut(dat$age, breaks=c(40,50,60,70,80))
df2 = data.frame(x=dat$lpsa, Age=as.factor(age_range))

# This is how ggplot creates multiple panels (it does not use base R par())
require(gridExtra)
plot1 <- ggplot(df1, aes(x=x, colour=Gleason)) + 
  geom_density() +
  scale_x_discrete(name="log(Prostate-specific Antigen)",
                   breaks=seq(-1,6,1), limits=c(-1,6)) +
  ggtitle("Log PSA as a factor of Gleason")

plot2 <- ggplot(df2, aes(x=x, colour=Age)) + 
  geom_density() +
  scale_x_discrete(name="log(Prostate-specific Antigen)",
                   breaks=seq(-1,6,1), limits=c(-1,6)) +
  scale_color_discrete(name="Age",
                       labels=c("40-50","50-60","60-70","70-80")) +
  ggtitle("Log PSA as a factor of Age")
grid.arrange(plot1, plot2, ncol=2) # 1 row with side-by-side plots

