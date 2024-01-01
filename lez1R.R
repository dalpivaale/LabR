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
train$pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + #praticamente col + aggiungi roba al plot, puoi anche assegnarlo ad una variabile e sommare ste robe alla variabile
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
head(as.character(train$Name))


# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name))) #unique funzione che si becca gli indici dove non si ripetono i valori: ottenendo un vettore senza ripetizioni


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "name"])


# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),] 


# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)


# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),] #si funzione che rileva dov'è presente la stringa "Miss." (string detect). quindi
misses[1:5,]


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mrses[1:5,]


# Check out males to see if pattern continues
males <- data.combined[which(data.combined$Sex == "male"), ] #which lo usi per beccarti gli indici del array che soddisfano una certa condizione, ossia per prenderti una porzione del dataset
males[1:5,]


# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.
extractTitle <- function(name) { #sta funzione matcha il name a esso passato come name
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) { #grep ricerca se il pattern "Miss" == a name, quando è soddisatta la condizione restiuisce "Miss" oppure verifica le condizioni qui di seguito
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
  titles <- c(titles, extractTitle(data.combined[i,"Name"])) #ad ogni riga si estrae il prefisso del nome e lo si somma a titles creando così la colonna titles
}
data.combined$title <- as.factor(titles)#aggiungiamo la colonna titles al dataset


# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) + #Plot delle prime 891 osservazioni ossia il train set, fa vedere la sopravvivenza in base alla classe e al title
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# What's the distribution of females to males across train & test?
table(data.combined$Sex)


# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +  #solito plot dei precedenti confrontando la sopravvivenza in base al sesso e alla classe del biglietto
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) + #aggiunge un'altra variabile al plot così da aggiungere altre informazioni
  geom_histogram(binwidth = 10) + #larghezza barre
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),] #in ques
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) + #interessante la condizione per pigliarsi i dati
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


# Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)


# Can we treat as a factor?
length(unique(data.combined$SibSp))


data.combined$SibSp <- as.factor(data.combined$sibsp)


# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
## SORT E SUBSET di una dataframe
sorted <- sort(dat$weight,decreasing =  FALSE)
sorted
sorted >= 70
n <- subset(dat,dat$weight > 75 & height > 170)

newdata <- subset(dat, age > 24 | age < 65, 
                  select=c(height, Weight)) #qua ti prende il subset con le sole colonne che vuoi

library(tidyverse)
n2<-arrange(dat, dat$weight) #sta funzione ordina il dataframe in ordine crescente a seconda di weigth ossia del peso delle persone


##COME USARE data.frame e table
df <- data.frame(player = c('AJ', 'Bob', 'Chad', 'Dan', 'Eric', 'Frank'),
                 position = c('A', 'B', 'B', 'B', 'B', 'A'),
                 points = c(1, 2, 2, 1, 0, 0))

#view data frame
df

player position points
1     AJ        A      1
2    Bob        B      2
3   Chad        B      2
4    Dan        B      1
5   Eric        B      0
6  Frank        A      0

#calculate frequency table for position variable
table(df$position) #tabella di frequenza

A B 
2 4

#calculate frequency table of proportions for position variable
prop.table(table(df$position))

A         B 
0.3333333 0.6666667

#calculate frequency table for position and points variable
table(df$position, df$points)

  0 1 2  #sta riga sono i points che il giocatore ha. Mentre la colonna A,B è la posizione del giocatore.
A 1 1 0
B 1 1 2

From the output we can observe:
  1 player in the data frame has a position of 'A' and 0 points
1 player in the data frame has a position of 'A' and 1 point
0 players in the data frame have a position of 'A' and 2 points
1 player in the data frame has a position of 'B' and 0 points
1 player in the data frame has a position of 'B' and 1 point
2 players in the data frame have a position of 'B' and 2 points

#only display two decimal places
options(digits=2) #si becca due decimali dopo la virgola

#calculate frequency table of proportions for position and points variable
prop.table(table(df$position, df$points))

0    1    2
A 0.17 0.17 0.00
B 0.17 0.17 0.33

##CHECKING FOR MISSING VALUE

data <- data.frame(x1 = c(NA, 5, 6, 8, 9),        
                   x2 = c(2, 4, NA, NA, 1),  
                   x3 = c(3,6,7,0,3),  
                   x4 = c("Hello", "value", 
                          NA, "geeksforgeeks", NA))  

data                     

# to find out the missing value 
which(is.na(data$x1)) #restitusce un vettore contenente le posizioni dei NA, su sta colonna c'è un NA i posizione 1 cioè il primo elemento
which(is.na(data$x2))  #qui in posizione  3 e 4
which(is.na(data$x3))  #qui non ci sono NA
which(is.na(data$x4))#su sta colonna NA in posizione 3 e 5

is.na(data$x1) #in.na restituisce un vettore contenente TRUE nella posizione in cui c'è il NA altrimenti FALSE
sum(is.na(data))# conta tutti i NA del Dataframe
mean(data$x1, na.rm = TRUE) #col secondo parametro a true non conta i NA

new_data <- data
new_data[is.na(new_data)] <- 99 #dove c'è NA ci sostituisco 99 (valore scelto a caso)
