# Trevor Volpe
# MIS 545 Section 01
# FILE NAME HERE
# CODE DESCRIPTION HERE

# Install required packages
# install.packages("tidyverse")
# install.packages("olsrr")
# install.packages("dummy")
# install.packages("corrplot")

# Load appropriate packages
library(tidyverse)
library(olsrr)
library(dummy)
library(corrplot)

# set working directory
setwd(paste0("/Users/sanchitagarwala/Documents/FallSemester/MIS545/",
             "Deliverables/PostMid-Term/Project", sep=""))

# Read CSV file into a tibble and define column types
# l for logical
# n for numeric
# i for integers
# c for characters
# f for factors
# D for dates
# T for datetimes
restTrial <- read_csv(file = "munch_n941.csv",
                      col_types = "lniiffffilfiflfini",
                      col_names = TRUE)

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

# normalize any non-normal features DONT KNOW IF THIS IS NECESSARY

# Scale and/or smooth data if required by the model

# Discretize variables if necessary

# Convert restTrial into a tibble containing only categorical features
restTrialCat <- restTrial %>% 
  select(GiveReferral, TakeReferral, Frequency, Gender,
         Education, Married, Race)

# Convert restTrialCat into a df containing only categorical features
restTrialCatDf <- data.frame(restTrialCat)

# Dummy code the position feature and turn the dataframe back into a tibble
restTrialCatDummy <- as_tibble(dummy(restTrialCatDf, int = TRUE))

# Rename dummy coded features
names(restTrialCatDummy) <- c("GiveRefNeither",
                              "GiveRefSomeLike",
                              "GiveRefExtLike",
                              "GiveRefSomeUnlike",
                              "GiveRefExtUnlike",
                              "TakeRefSomeLike",
                              "TakeRefExtLike",
                              "TakeRefNeither",
                              "TakeRefExtUnlike",
                              "TakeRefSomeUnlike",
                              "FreqMonthly",
                              "FreqEveryFewMonth",
                              "FreqWeekly",
                              "FreqEveryCoupleWeek",
                              "FreqDaily",
                              "FreqNever",
                              "GenderFemale",
                              "GenderMale",
                              "EdSomeCollege",
                              "EdDoct",
                              "EdBach",
                              "EdMast",
                              "EdAssoc",
                              "EdHS",
                              "EdLessHS",
                              "EdProf",
                              "MaritalSingle",
                              "MaritalMarried",
                              "MaritalDivorced",
                              "MaritalWidowed",
                              "MaritalSeparated",
                              "RaceWhite",
                              "RaceAsian",
                              "RaceHisp",
                              "RaceAmIndian",
                              "RaceBlack",
                              "RaceOther",
                              "RacePacific")

# Combine restTrialCatDummy with other features in restTrialCat
# excluding GiveReferral, TakeReferral, Frequency, Gender,
# Education, Married, Race
restTrialComplete <- cbind(restTrialCatDummy, restTrial) %>%
  select(-GiveReferral,
         -TakeReferral,
         -Frequency,
         -Gender,
         -Education,
         -Married,
         -Race)

# Remove referential variables for each categorical variable
restTrialComplete <- restTrialComplete %>% 
  select(-GiveRefNeither, -TakeRefNeither, -FreqNever, -GenderFemale,
         -EdHS, -MaritalMarried, -RaceWhite)


# Data collection already controlled for outliars, randomly generated numbers,
# and income is capped at $500,000 (i.e. if someone made over $500,000 they
# only put $500,000)

# no new variables required

# Interesting query 1:
print(restTrialComplete %>%
        group_by(TryWithout) %>%
        summarize(MeanRating = mean(RestRating)) %>%
        arrange(desc(MeanRating)),
      n = Inf)

# Interesting query 2:
print(restTrialComplete %>%
        group_by(TryWithout) %>%
        summarize(MeanPrice = mean(RestPrice)) %>%
        arrange(desc(MeanPrice)),
      n = Inf)

# Interesting query 3: 
print(restTrialComplete %>%
        group_by(TryWithout) %>%
        summarize(MeanDistance = mean(RestDistance)) %>%
        arrange(desc(MeanDistance)),
      n = Inf)

# Check data for pairwise correlations SHOULD THESE NOT BE DUMMIED?
print(round(cor(restTrialComplete),2))

# Correlation plot of mobilePhone variables SHOULD THESE NOT BE DUMMIED?
corrplot(cor(restTrialComplete),
         method = "number",
         type = "lower")

# Set randomization seed to a specified value for recreation purposes
set.seed(203)

# Randomly split data into 75% training and 25% testing sets
sampleSet <- sample(nrow(restTrialComplete),
                    round(nrow(restTrialComplete) * 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into mobilePhoneTraining
restTrialCompleteTraining <- restTrialComplete[sampleSet, ]

# Put the records from the 25% sample into mobilePhoneTesting
restTrialCompleteTesting <- restTrialComplete[-sampleSet, ]

# Check for class imbalance of TryWithout
print(summary(restTrialCompleteTraining$TryWithout))

# Store magnitude of class imbalance of TryWithout
classImbalanceMagnitude <- 428 / 278

# Create logistic regression which uses TryWithout as dependent variable
restTrialModel <- glm(data = restTrialCompleteTraining,
                      family = binomial,
                      formula = TryWithout ~ .)

# Display logistic regression model
print(summary(restTrialModel))

# Predict outcome of the test set using the restTrialModel
RestTrialPrediction <- predict(restTrialModel,
                               restTrialCompleteTesting)

# Set threshold for binary outcome at above 0.5 for 1, anything equal to or
# below 0.5 will be set to 0
RestTrialPrediction <-
  ifelse(RestTrialPrediction > 0.5, 1, 0)

# Generate a confusion matrix of predictions
restTrialConfusionMatrix <- table(restTrialCompleteTesting$TryWithout,
                                  RestTrialPrediction)

# Display confusion matrix of predictions
print(restTrialConfusionMatrix)

# Calculate the false positive rate of predictions
restTrialConfusionMatrix[1, 2] /
  (restTrialConfusionMatrix[1, 2] +
     restTrialConfusionMatrix[1, 1])

# Calculate the false negative rate of predictions
restTrialConfusionMatrix[2, 1] /
  (restTrialConfusionMatrix[2, 1] +
     restTrialConfusionMatrix[2, 2])

# Calculate the prediction accuracy by dividing the number of true positives
# and true negatives by the total amount of predictions in the testing dataset
sum(diag(restTrialConfusionMatrix)) / nrow(restTrialCompleteTesting)

