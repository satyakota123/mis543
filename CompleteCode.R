# Trevor Volpe
# MIS 545 Section 01
# FILE NAME HERE
# CODE DESCRIPTION HERE

# Install required packages
# install.packages("tidyverse")
# install.packages("olsrr")
# install.packages("dummy")
# install.packages("corrplot")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("neuralnet")
# install.packages("e1071")

# Load appropriate packages
library(tidyverse)
library(olsrr)
library(dummy)
library(corrplot)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(e1071)

# set working directory
setwd('C:\\Users\\volpe\\Documents\\mis543')

# Read CSV file into a tibble and define column types
# l for logical
# n for numeric
# i for integers
# c for characters
# f for factors
# D for dates
# T for datetimes
restTrial <- read_csv(file = "munch_try_with_discount.csv",
                         col_types = "lniifflililinii",
                         col_names = TRUE)

# Reading the binned smoothed CSV file into an object called restTrialBinned using the read_csv() 
# function.
restTrialBinned <- read_csv(file = "munch_try_with_discount_binned.csv",
                         col_types = "lniifflililinii",
                         col_names = TRUE)

# Display summary of restTrial and restrial1
summary(restTrial)
summary(restTrialBinned)

# Drop NA Age 
restTrial <- restTrial %>%
  drop_na()

# Create a function called DisplayAllHistograms that take in a tibble parameter
# that will display a histogram for all numeric features in the tibble
displayAllHistograms <- function(tibbleDataSet) {
  tibbleDataSet %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}

# Call the displayAllHistgoram() functions using our restTrial tibble
displayAllHistograms(restTrial)

# normalize any non-normal features (age, income)
# drop income values of 0 because can't take log of 0
restTrial <- restTrial %>%
  filter(Income != 0)

restTrial <- restTrial %>% 
  mutate(LogIncome = log(Income))

restTrial <- restTrial %>% 
  mutate(LogAge = log(Age))

# Prep scaled tibble to only include continuous variables
restTrialScaled <- restTrial

# Scale and/or smooth data if required by the model
restTrialScaled <- restTrialScaled %>% 
  mutate(IncomeScaled = (Income - min(Income))/
           (max(Income)-min(Income))) %>%
  mutate(MinStarsScaled = (MinStars - min(MinStars))/
           (max(MinStars)-min(MinStars))) %>%
  mutate(MaxDistScaled = (MaxDist - min(MaxDist))/
           (max(MaxDist)-min(MaxDist))) %>%
  mutate(MaxPriceScaled = (MaxPrice - min(MaxPrice))/
           (max(MaxPrice)-min(MaxPrice))) %>%
  mutate(RestDistanceScaled = (RestDistance - min(RestDistance))/
           (max(RestDistance)-min(RestDistance))) %>%
  mutate(RestRatingScaled = (RestRating - min(RestRating))/
           (max(RestRating)-min(RestRating))) %>%
  mutate(RestPriceScaled = (RestPrice - min(RestPrice))/
           (max(RestPrice)-min(RestPrice))) %>%
  mutate(RestDiscountScaled = (RestDiscount - min(RestDiscount))/
           (max(RestDiscount)-min(RestDiscount)))

# Drop non-scaled data from scaled tibble
restTrialScaledComplete <- restTrialScaled %>%
  select(-Income, -MinStars, -MaxDist, -MaxPrice, -RestDistance,
         -RestRating, -RestPrice, -RestDiscount, -LogIncome, -LogAge)

# Drop non-normalized data after scaling has been done 
restTrial <- restTrial %>%
  select(-Age, -Income)

# Re-check distributions to see if normalization worked
displayAllHistograms(restTrial)

  
# NEED TO SCALE FOR NEURAL NETWORK  

# Discretize variables if necessary

# Convert restTrial into a tibble containing only categorical features
restTrialCat <- restTrial %>% 
                   select(TakeReferral, Frequency)

# Convert restTrialCat into a df containing only categorical features
restTrialCatDf <- data.frame(restTrialCat)

# Dummy code the position feature and turn the dataframe back into a tibble
restTrialCatDummy <- as_tibble(dummy(restTrialCatDf, int = TRUE))

# Rename dummy coded features
names(restTrialCatDummy) <- c("TakeRefSomeLike",
                              "TakeRefExtLike",
                              "TakeRefNeither",
                              "TakeRefExtUnlike",
                              "TakeRefSomeUnlike",
                              "FreqMonthly",
                              "FreqEveryFewMonth",
                              "FreqWeekly",
                              "FreqEveryCoupleWeek",
                              "FreqDaily",
                              "FreqNever")

# Combine restTrialCatDummy with other features in restTrial
# excluding TakeReferral, Frequency
restTrialComplete <- cbind(restTrialCatDummy, restTrial) %>%
  select(-TakeReferral,
         -Frequency)

# Remove categorical variables from restTrialScaledComplete 
restTrialScaledComplete <- restTrialScaledComplete %>%
  select(-TakeReferral,
         -Frequency)

# Add dummied categorical variables back to restTrialScaledComplete
restTrialScaledComplete <- cbind(restTrialCatDummy, restTrialScaledComplete)
  

# Remove referential variables for each categorical variable
restTrialComplete <- restTrialComplete %>% 
  select(-TakeRefNeither, -FreqNever)

restTrialScaledComplete <- restTrialScaledComplete %>% 
  select(-TakeRefNeither, -FreqNever)

# Data collection already controlled for outliars, randomly generated numbers,
# and income is capped at $500,000 (i.e. if someone made over $500,000 they
# only put $500,000)

# no new variables required

# Interesting query 1:
print(restTrialComplete %>%
        group_by(TryWith) %>%
        summarize(MeanRating = mean(RestRating)) %>%
        arrange(desc(MeanRating)),
      n = Inf)

# Interesting query 2:
print(restTrialComplete %>%
        group_by(TryWith) %>%
        summarize(MeanPrice = mean(RestPrice)) %>%
        arrange(desc(MeanPrice)),
      n = Inf)

# Interesting query 3: 
print(restTrialComplete %>%
        group_by(TryWith) %>%
        summarize(MeanDistance = mean(RestDistance)) %>%
        arrange(desc(MeanDistance)),
      n = Inf)

# Correlation plot of restTrial variables SHOULD THESE NOT BE DUMMIED?
restTrialCompleteNumeric <- restTrial %>% 
  keep(is.numeric)

corrplot(cor(restTrialCompleteNumeric),
         method = "number",
         type = "lower")

# Set randomization seed to a specified value for recreation purposes
set.seed(203)

# Randomly split data into 75% training and 25% testing sets
sampleSet <- sample(nrow(restTrialComplete),
                    round(nrow(restTrialComplete) * 0.75),
                    replace = FALSE)

# Randomly split binned data into 75% training and 25% testing sets
sampleSetBinned <- sample(nrow(restTrialBinned),
                    round(nrow(restTrialBinned) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into restTrialCompleteTraining and restTrialBinnedTraining
restTrialCompleteTraining <- restTrialComplete[sampleSet, ]
restTrialBinnedTraining <- restTrialBinned[sampleSetBinned, ]


# Put the records from the 25% sample into restTrialCompleteTesting and restTrialBinnedTesting
restTrialCompleteTesting <- restTrialComplete[-sampleSet, ]
restTrialBinnedTesting <- restTrialBinned[ -sampleSetBinned, ]

# Randomly split data into 75% training and 25% testing sets
sampleSet <- sample(nrow(restTrialScaledComplete),
                    round(nrow(restTrialScaledComplete) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into restTrialCompleteTraining
restTrialCompleteScaledTraining <- restTrialScaledComplete[sampleSet, ]

# Put the records from the 25% sample into restTrialCompleteTesting
restTrialCompleteScaledTesting <- restTrialScaledComplete[-sampleSet, ]

# Check for class imbalance of TryWith
print(summary(restTrialCompleteTraining$TryWith))

# Store magnitude of class imbalance of TryWith
classImbalanceMagnitude <- 200 / 68

# Create logistic regression which uses TryWith as dependent variable
restTrialModelLogistic <- glm(data = restTrialCompleteTraining,
                        family = binomial,
                        formula = TryWith ~ .)

# Display logistic regression model
print(summary(restTrialModelLogistic))

# Predict outcome of the test set using the restTrialModel
RestTrialPredictionLogistic <- predict(restTrialModelLogistic,
                               restTrialCompleteTesting)

# Set threshold for binary outcome at above 0.5 for 1, anything equal to or
# below 0.5 will be set to 0
RestTrialPredictionLogistic <-
  ifelse(RestTrialPredictionLogistic > 0.5, 1, 0)

# Generate a confusion matrix of predictions
restTrialConfusionMatrixLogistic <- table(restTrialCompleteTesting$TryWith,
                                          RestTrialPredictionLogistic)

# Display confusion matrix of predictions
print(restTrialConfusionMatrixLogistic)

# Calculate the false positive rate of predictions
restTrialConfusionMatrixLogistic[1, 2] /
  (restTrialConfusionMatrixLogistic[1, 2] +
     restTrialConfusionMatrixLogistic[1, 1])

# Calculate the false negative rate of predictions
restTrialConfusionMatrixLogistic[2, 1] /
  (restTrialConfusionMatrixLogistic[2, 1] +
     restTrialConfusionMatrixLogistic[2, 2])

# Calculate the prediction accuracy by dividing the number of true positives
# and true negatives by the total amount of predictions in the testing dataset
sum(diag(restTrialConfusionMatrixLogistic)) / nrow(restTrialCompleteTesting)

# Test for multicollinearity among explanatory variables ONLY WORKS ON LINEAR REGRESSION?
# ols_vif_tol(restTrialModel)

# KNN

# Naive Bayes
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

# Evaluate the Naive Bayes model with confusion matrix
restTrialBinnedConfusion <- table(restTrialBinnedTesting$TryWith,
                            restTrialBinnedPrediction)

# Display confusion matrix
print(restTrialBinnedConfusion)

# calculate Naive Bayes model's predictive accuracy
restTrialBinnedPredictiveAccuracy <- sum(diag(restTrialBinnedConfusion))/
  nrow(restTrialBinnedTesting)

# Display predictive accuracy
print(restTrialBinnedPredictiveAccuracy)

# Decision Tree
# Generate the decision tree model to predict restTrial based on the other 
# variables in the dataset. Use 0.01 as the complexity parameter.
restTrialDecisionTreeModel0 <- rpart(formula = TryWith ~.,
                                     method = "class",
                                     cp = 0.01,
                                     data = restTrialCompleteTraining)

restTrialDecisionTreeModel1 <- rpart(formula = TryWith ~.,
                                     method = "class",
                                     cp = 0.1,
                                     data = restTrialCompleteTraining)
restTrialDecisionTreeModel2 <- rpart(formula = TryWith ~.,
                                     method = "class",
                                     cp = 0.007,
                                     data = restTrialCompleteTraining)

# Display the decision tree visualization in R
rpart.plot(restTrialDecisionTreeModel0)
rpart.plot(restTrialDecisionTreeModel1)
rpart.plot(restTrialDecisionTreeModel2)

# Predicting the classes for each record in the testing dataset and storing them  
# in restTrialPrediction
restTrialPredictionDT0 <- predict(restTrialDecisionTreeModel0,
                                  restTrialCompleteTesting,
                                  type = "class")
restTrialPredictionDT1 <- predict(restTrialDecisionTreeModel1,
                                  restTrialCompleteTesting,
                                  type = "class")
restTrialPredictionDT2 <- predict(restTrialDecisionTreeModel2,
                                  restTrialCompleteTesting,
                                  type = "class")

# Displaying the restTrialPrediction on the console
print(restTrialPredictionDT0)
print(restTrialPredictionDT1)
print(restTrialPredictionDT2)

# Evaluating the model by forming a confusion matrix
restTrialConfusionMatrixDT0 <- table(restTrialCompleteTesting$TryWith,
                                     restTrialPredictionDT0)
restTrialConfusionMatrixDT1 <- table(restTrialCompleteTesting$TryWith,
                                   restTrialPredictionDT1)
restTrialConfusionMatrixDT2 <- table(restTrialCompleteTesting$TryWith,
                                   restTrialPredictionDT2)

# Displaying the confusion matrix on the console
print(restTrialConfusionMatrixDT0)
print(restTrialConfusionMatrixDT1)
print(restTrialConfusionMatrixDT2)

# Calculating the model predictive accuracy and storing it into a variable 
# called predictiveAccuracy
predictiveAccuracyDT0 <- sum(diag(restTrialConfusionMatrixDT0)) /
  nrow(restTrialCompleteTesting)
predictiveAccuracyDT1 <- sum(diag(restTrialConfusionMatrixDT1)) /
  nrow(restTrialCompleteTesting)
predictiveAccuracyDT2 <- sum(diag(restTrialConfusionMatrixDT2)) /
  nrow(restTrialCompleteTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracyDT0)
print(predictiveAccuracyDT1)
print(predictiveAccuracyDT2)

# Neural Network
# Generate the neural network model to predict TryWithout using IncomeScaled, 
# MinStarsScaled, MaxDistScaled, MaxPriceScaled, RestDistanceScaled, 
# RestRatingScaled and RestPriceScaled.. Use 3 hidden layers. Use "logistic" as
# the smoothing method and set linear.output to FALSE.
restTrialNeuralNet <- neuralnet(
  formula = TryWith ~ .,
  data = restTrialCompleteScaledTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)

# Display the neural network numeric results
print(restTrialNeuralNet$result.matrix)

# Visualize the neural network
plot(restTrialNeuralNet)

# Use restTrialNeuralNet to generate probabilities on the restTrialCompleteScaledTesting 
# data set and store this in restTrialNeuralNetProbability
restTrialNeuralNetProbability <- compute(restTrialNeuralNet,
                                         restTrialCompleteScaledTesting)

# Displaying the probabilities from the testing dataset on the console
print(restTrialNeuralNetProbability)

# Converting probability predictions into 0/1 predictions and store this into 
# restTrial1Prediction
restTrialNeuralNetPrediction <-
  ifelse(restTrialNeuralNetProbability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(restTrialNeuralNetPrediction)

# Evaluating the model by forming a confusion matrix
restTrialNeuralNetConfusionMatrix <- table(restTrialCompleteScaledTesting$TryWith,
                                           restTrialNeuralNetPrediction)

# Displaying the confusion matrix on the console
print(restTrialNeuralNetConfusionMatrix)

# Calculating the model predictive accuracy
predictiveAccuracyNeuralNet <- sum(diag(restTrialNeuralNetConfusionMatrix)) /
  nrow(restTrialCompleteScaledTesting)

# Displaying the predictive accuracy on the console
print(predictiveAccuracyNeuralNet)
