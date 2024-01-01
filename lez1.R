#Load raw data
train <- read.csv("train.csv", header = TRUE) #passengers titanic data
test <- read.csv("test.csv", header = TRUE)

#Add a "Survived" variable to the test set to allow for combining data sets
test.survided <- data.frame(Survived = rep("None",nrow(test)),test[,]) #add the variable column Survived to the left of test set
#data.frame function manages data/matrix so add columns or rows

#Combine datasets
data.combined <- rbind(train,test.survided) #Row combine, cbind is columns combines. takes test.survided row appens them to the end of trains set

#A bit about R data types (e.g factors)
str(data.combined) #Diplay the structure of the object, describe that variable

data.combined$Survived <- as.factor(data.combined$Survived)#idem qui sopra, al machine learning non piacciono le stringhe meglio come fattore
data.combined$Pclass <- as.factor(data.combined$Pclass) #Pclass is the Passenger class of the Titanic (1st ricco, 2nd, 3rd povero): Pclass variable turn into a factor

table(data.combined$Survived)#riassunto dei valori del parametro in questo caso la colonna Survived

table(data.combined$Pclass)


# Load up ggplot2 package to use for visualizations
library(ggplot2)


# Hypothesis - Rich folks survived at a higer rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
head(as.character(train$name))


# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])


# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)


# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]


# Check out males to see if pattern continues
males <- data.combined[which(data.combined$sex == "male"), ]
males[1:5,]


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)


# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")