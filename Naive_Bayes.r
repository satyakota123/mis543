# Install packages --------------------------------------------------------
install.packages("tidyverse")
install.packages("e1071")

# Load the packages
library(tidyverse)
library(e1071)

# set working directory
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Documents/Courses
      /Sem 3/MIS545 - Data Mining/MIS543Project")

# Reading the CSV file into an object called munch_n941 the read_csv() 
# function.
restTrialBinned <- read_csv(file = "munch_try_with_discount_binned.csv",
                         col_types = "lniifflililinii",
                         col_names = TRUE)

# Displaying the restTrialBinned tibble on the console 
# using the print() function
print(restTrialBinned)

# Displaying the structure of the restTrialBinned tibble 
# using the str() function
str(restTrialBinned)

# Displaying a summary of the restTrialBinned tibble 
# using the summary() function
summary(restTrialBinned)

# Setting the random seed to 591
set.seed(203)

# Randomly split the dataset into restTrialBinnedTraining (75% of records) and 
# restTrialBinnedTesting (25% of records)
sampleSetBinned <- sample(nrow(restTrialBinned),
                    round(nrow(restTrialBinned) * 0.75),
                    replace = FALSE)
restTrialBinnedTraining <- restTrialBinned[sampleSetBinned, ]
restTrialBinnedTesting <- restTrialBinned[ -sampleSetBinned, ]

# Generate naive bayes model
restTrialBinnedModel <- naiveBayes(formula = TryWith ~ .,
                            data = restTrialBinnedTraining,
                            laplace = 1)

# Build probabilities for records in the testing dataset and store them
restTrialBinnedProbability <- predict(restTrialModel,
                            restTrialBinnedTraining,
                            type = "raw")

# Print restTrialProbability
print(restTrialBinnedProbability)

# Predict classes for values
restTrialBinnedPrediction <- predict(restTrialModel,
                            restTrialBinnedTesting,
                            type = "class")

# display restTrialBinned Prediction
print(restTrialBinnedPrediction)

# Evaluate the model with confusion matrix
restTrialBinnedConfusion <- table(restTrialBinnedTesting$TryWith,
                            restTrialBinnedPrediction)

# Display confusion matrix
print(restTrialBinnedConfusion)

# calculate model's predictive accuracy
restTrialBinnedPredictiveAccuracy <- sum(diag(restTrialBinnedConfusion))/
  nrow(restTrialBinnedTesting)

# Display predictive accuracy
print(restTrialBinnedPredictiveAccuracy)