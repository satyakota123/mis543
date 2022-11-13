# Agarwala Sanchit
# MIS 545 Section 1
# Decision Tree

# Pre-execution --------------------------------------------------------
# By utilizing installed.packages() we can establish if the package is
# installed and if the package isn't in installed can be downloaded using
# install.packages().
# install.packages("tidyverse")
# install.packages("rpart")
# install.packages("rpart.plot")

# We can load/run the tidyverse/rpart/rpart.plot library using the
# library() command and check if the package is installed can be checked under 
# the packages tab on the right of the screen.
library(tidyverse)
library(rpart)
library(rpart.plot)

# Setting the working directory to your Lab08 folder
setwd(paste0("/Users/sanchitagarwala/Documents/FallSemester/MIS545/",
             "Deliverables/PostMid-Term/Project", sep=""))
# print(getwd())

# Reading the  munch_n941.csv into a tibble called restrail
restTrial <- read_csv(file = "munch_n941.csv",
                      col_types = "lniiffffilfiflfini",
                      col_names = TRUE)

# The print() command displays the restTrial in the console.
print(restTrial)

# The str() command displays the structure of restTrial in the console
str(restTrial)

# The summary() command displays the summary of restTrial in the console
summary(restTrial)

# Randomly splitting the dataset into restTrialTraining (75% of records) 
# and restTrialTesting (25% of records) using 370 as the random seed
set.seed(370)
sampleSet <- sample(nrow(restTrial),
                    round(nrow(restTrial) * 0.75),
                    replace = FALSE)
restTrialTraining <- restTrial[sampleSet, ]
restTrialTesting <- restTrial[ -sampleSet, ]

# Generate the decision tree model to predict FarmOwnership based on the other 
# variables in the dataset. Use 0.01 as the complexity parameter.
restTrialDecisionTreeModel0 <- rpart(formula = TryWithout ~.,
                                    method = "class",
                                    cp = 0.01,
                                    data = restTrialTraining)
restTrialDecisionTreeModel1 <- rpart(formula = TryWithout ~.,
                                     method = "class",
                                     cp = 0.1,
                                     data = restTrialTraining)
restTrialDecisionTreeModel2 <- rpart(formula = TryWithout ~.,
                                     method = "class",
                                     cp = 0.007,
                                     data = restTrialTraining)

# Display the decision tree visualization in R
rpart.plot(restTrialDecisionTreeModel0)
rpart.plot(restTrialDecisionTreeModel1)
rpart.plot(restTrialDecisionTreeModel2)

# Predicting the classes for each record in the testing dataset and storing them  
# in restTrialPrediction
restTrialPrediction0 <- predict(restTrialDecisionTreeModel0,
                               restTrialTesting,
                               type = "class")
restTrialPrediction1 <- predict(restTrialDecisionTreeModel1,
                                restTrialTesting,
                                type = "class")
restTrialPrediction2 <- predict(restTrialDecisionTreeModel2,
                                restTrialTesting,
                                type = "class")

# Displaying the restTrialPrediction on the console
print(restTrialPrediction0)
print(restTrialPrediction1)
print(restTrialPrediction2)

# Evaluating the model by forming a confusion matrix
restTrialConfusionMatrix0 <- table(restTrialTesting$TryWithout,
                                  restTrialPrediction0)
restTrialConfusionMatrix1 <- table(restTrialTesting$TryWithout,
                                   restTrialPrediction1)
restTrialConfusionMatrix2 <- table(restTrialTesting$TryWithout,
                                   restTrialPrediction2)

# Displaying the confusion matrix on the console
print(restTrialConfusionMatrix0)
print(restTrialConfusionMatrix1)
print(restTrialConfusionMatrix2)

# Calculating the model predictive accuracy and storing it into a variable 
# called predictiveAccuracy
predictiveAccuracy0 <- sum(diag(restTrialConfusionMatrix0)) /
  nrow(restTrialTesting)
predictiveAccuracy1 <- sum(diag(restTrialConfusionMatrix1)) /
  nrow(restTrialTesting)
predictiveAccuracy2 <- sum(diag(restTrialConfusionMatrix2)) /
  nrow(restTrialTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy0)
print(predictiveAccuracy1)
print(predictiveAccuracy2)
