# Install packages --------------------------------------------------------
install.packages("tidyverse")
install.packages("e1071")

# Load the packages
library(tidyverse)
library(e1071)

# set working directory
setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Documents/Courses/Sem 3/MIS545 - Data Mining/MIS543Project")

# Reading the CSV file into an object called munch_n941 the read_csv() 
# function.
restTrial1 <- read_csv(file = "munch_n941.csv",
                      col_types = "lniiffffilfiflfini",
                      col_names = TRUE)

# Displaying the restTrial1 tibble on the console using the print() function
print(restTrial1)

# Displaying the structure of the restTrial1 tibble using the str() function
str(restTrial1)

# Displaying a summary of the restTrial1 tibble using the summary() function
summary(restTrial1)

# Setting the random seed to 591
set.seed(203)

# Randomly split the dataset into restTrial1Training (75% of records) and 
# restTrial1Testing (25% of records)
sampleSet1 <- sample(nrow(restTrial1),
                    round(nrow(restTrial1) * 0.75),
                    replace = FALSE)
restTrial1Training <- restTrial1[sampleSet1, ]
restTrial1Testing <- restTrial1[ -sampleSet1, ]

# Generate naive bayes model
restTrialModel <- naiveBayes(formula = TryWithout ~ .,
                            data = restTrial1Training,
                            laplace = 1)

# Build probabilities for records in the testing dataset and store them
restTrialProbability <- predict(restTrialModel,
                            restTrial1Training,
                            type = "raw")

# Print restTrialProbability
print(restTrialProbability)

# Predict classes for values
restTrialPrediction <- predict(restTrialModel,
                            restTrial1Testing,
                            type = "class")

# display restTrial Prediction
print(restTrialPrediction)

# Evaluate the model with confusion matrix
restTrialConfusion <- table(restTrial1Testing$TryWithout,
                            restTrialPrediction)

# Display confusion matrix
print(restTrialConfusion)

# calculate model's predictive accuracy
predictiveAccuracy <- sum(diag(restTrialConfusion))/nrow(restTrial1Testing)

# Display predictive accuracy
print(predictiveAccuracy)