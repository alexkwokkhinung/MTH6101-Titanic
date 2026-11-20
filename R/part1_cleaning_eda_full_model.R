source("R/00_setup.R")
#add code below

#Upload dataset to R

train <- read.csv("train.csv") # uploads csv table to r to use
str(train) # structure of the table each col data type etc
summary(train) # some stats from the table like number of NA's (All in Age)
nrow(train) # number of rows in the table train
summary(train$Age) # stats from the column Age only


#Clean dataset

train_clean <- train[complete.cases(train),] # table of cases without NA's
nrow(train_clean) # see how many rows in the cleaned table
nrow(train)-nrow(train_clean)==177 # CHECK if everything adds up
blanks <- colSums(1*(train_clean=="")) # finds the sum of 1's in each col where 1 is every time there is a blank cell since completed cases does not pick this up
blanks # how many blank cells there are per column 
new_clean <- train_clean[-which(train_clean$Embarked==""),] # new table that has removed the rows with blank entries from embarked inside, we know that there were 2
colSums(1*(new_clean==""))
nrow(new_clean)


#Compare clean dataset to original


sum(new_clean$Survived) # number of passengers that survived
table(new_clean$Survived)# number of people that survived(1) and number of people that died(0) fro clean 
table(train$Survived)# number of people that survived(1) and number of people that died(0) from original table
mean(new_clean$Survived) # proportion of people that survived in clean
mean(train$Survived)# proportion of people that survived in original set. can see it's slightly less than for clean


#How is survival associated with each predictor

tapply(new_clean$Survived, new_clean$Sex, mean) # table for survival rate by sex
tapply(new_clean$Survived, new_clean$SibSp, mean) # table for survival rate by SibSp
tapply(new_clean$Survived, new_clean$Pclass, mean) # table for survival rate by class
tapply(new_clean$Survived, new_clean$Embarked, mean) # table for survival rate by embarked
tapply(new_clean$Survived, new_clean$Parch, mean) # table for survival rate by Parch
tapply(new_clean$Age, new_clean$Survived, summary) # table for stats about age like mean median for survived and non-survived separately
tapply(new_clean$Fare, new_clean$Survived, summary) # table for stats about fare like mean median for survived and non-survived separately

barplot(tapply(new_clean$Survived, new_clean$Sex, mean), main="Survival rate by sex", ylab="Survival rate", xlab="Sex")
barplot(tapply(new_clean$Survived, new_clean$SibSp, mean), main="Survival rate by number of siblings or spouses a passenger has onboard", ylab="Survival rate", xlab="Number of siblings/spouse")
barplot(tapply(new_clean$Survived, new_clean$Pclass, mean), main="Survival rate by class", ylab="Survival rate", xlab="Class")
barplot(tapply(new_clean$Survived, new_clean$Embarked, mean), main="Survival rate by where passenger embarked", ylab="Survival rate", xlab="Port")
barplot(tapply(new_clean$Survived, new_clean$Parch, mean), main="Survival rate by the number of parents or children a passenger has onboard", ylab="Survival rate", xlab="Number of parents/children")

boxplot(Age~Survived, data=new_clean, main="Age by outcome", ylab="Age", xlab="Died/Survived")
boxplot(Fare~Survived, data=new_clean, main="Fare by outcome", ylab="Fare", xlab="Died/Survived")


#Create random stratified training and test sets

nc_survived <- which(new_clean$Survived==1)
nc_died <- which(new_clean$Survived==0)
set.seed(1) # fixed starting point for random number generator in r
training <- c(sample(nc_survived, round(0.8*length(nc_survived))), sample(nc_died, round(0.8*length(nc_died))))# training set. random set of rows from new_clean
test <- (1:nrow(new_clean))[-training] # test set
length(training)+length(test)==nrow(new_clean) # CHECK the number of rows of training and test is equal to the number of rows in new_clean 
intersect(training, test) # Checks there are no rows in both sets, should return an empty vector
anyDuplicated(training) # Check for repeated rows in training, should return 0
anyDuplicated(test) # Check for repeated rows in test, should return 0


#Check training and test sets are stratified

prop.table(table(new_clean$Survived))
prop.table(table(new_clean$Survived[training]))
prop.table(table(new_clean$Survived[test]))
#Checks each set to see if the proportion of passengers that have survived and dies is roughly the same


#Fit logistic model and check predictive performance

M1 <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=new_clean[training,], family="binomial") # Fits logistic model using the log of the odds (p/1-p) to training rows
P1 <- predict.glm(object=M1, newdata=new_clean[test, ], type="response") # computes z then returns a vector of predicted probabilities for each passenger (converts log-odds to probability) one per test row between 0 and 1. p is the probability that a passenger survives. 

summary(M1)
# Pr(>|z|) - smaller p val means that good evidence that predictor is related to survival.
# Predictors with +ve slope raises log odds which raises odds and probability f survival , -ve slope lowers probability of survival.
exp(M1$coefficients) # Holding all other variables fixed, each value is the factor the odds of survival are multiplied by for a one-unit increase so below 1 lowers the odds and above 1 raises them. the y-intercept is not interpreted because is no class "0" for Pclass. 
summary(P1) # stats like mean and median for the predicted probabilities
length(P1)==length(test) # CHECK length of P1 is equal to length of test set

threshold1 <- 1*(P1>0.5) # set a value for p which p has to be greater than for the passenger to be labelled Survived  
conf0.5 <- table(new_clean$Survived[test], threshold1) # counts how many passengers fall into each combination of what the model predicted and what actually happened
conf0.5
TN0.5 <- conf0.5[1,1] # true negative- the passenger died and the model said died
FP0.5 <- conf0.5[1,2] # false positive- the passenger died and the model said survived
FN0.5 <- conf0.5[2,1] # false negative- the passenger survived but the model said died
TP0.5 <- conf0.5[2,2] # true positive- the passenger survived and the model said survived
accuracy0.5 <- (TP0.5+TN0.5)/(TP0.5+TN0.5+FP0.5+FN0.5) # Correct guesses divided by all test passengers at threshold=0.5. Stores a single number between 0 and 1.
accuracy0.5
sensitivity0.5 <- TP0.5/(TP0.5+FN0.5)# Survivors the model caught divided by all actual survivors. Stores a single number between 0 and 1.
sensitivity0.5
specificity0.5 <- TN0.5/(TN0.5+FP0.5)# Deaths the model caught divided by all actual deaths. Stores a single number between 0 and 1.
specificity0.5
baseline_accuracy <- (TN0.5+FP0.5)/(TP0.5+TN0.5+FP0.5+FN0.5)# TN + FP is every passenger who actually died so this is the share of non-survivors in the test rows. That is the accuracy of a model that predicts "died" for everyone. Stores a single number between 0 and 1.
baseline_accuracy
baseline0.5>accuracy0.5 # Asks whether the model that says everyone dies beats the our model.
threshold2 <- 1*(P1>0.3)
conf0.3 <- table(new_clean$Survived[test], threshold2)
conf0.3
threshold3 <- 1*(P1>0.0)
conf0.0 <- table(new_clean$Survived[test], threshold3) # when the threshold is 0 every p is above the the cutoff so everyone is guessed to have survived.
conf0.0

#Plot ROC curve and check AUC

library(pROC) # Loads package for ROC 
roc_curve <- roc(response=new_clean$Survived[test], predictor=P1) 
plot(roc_curve) # plots ROC curve. y axis is rate of people that actually survived that model says survived (Sensitivity). x axis (from right to left) is the share of people who died that the model guessed as non-survivors (Specificity). each point in the curve is one threshold.
auc(roc_curve) # Area under roc curve. shows if we picked a survivor and a non-survivor from the test set at random whats the probability p (i.e. the probability of survival that the model gives) is greater for the survivor than the non-survivor.
