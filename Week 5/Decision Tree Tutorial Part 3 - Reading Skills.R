# Load the party package. It will automatically load other
# dependent packages.

getwd()
setwd("C:/Users/User/Documents/NCI20-21_Sem3/DWM HDSDA_JAN/Lecture 5 Decision Trees")

#########
## STEP 1
#########

install.packages("party", dependencies = TRUE)
install.packages("TH.data", dependencies = TRUE)
library(party)

# Print some records from data set readingSkills.
print(head(readingSkills))
table(readingSkills$nativeSpeaker)

# Load the party package. It will automatically load other
# dependent packages.
library(party)

#########
## STEP 2
#########

dim(readingSkills)
# Create the input data frame.
input.dat <- readingSkills[c(1:105),]

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree.
output.tree <- ctree(
  nativeSpeaker ~ age + shoeSize + score, 
  data = input.dat)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()
