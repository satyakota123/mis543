# Agarwala Sanchit
# MIS 545 Section 1
# Neural Network

# Pre-execution --------------------------------------------------------
# By utilizing installed.packages() we can establish if the package is
# installed and if the package isn't in installed can be downloaded using
# install.packages().
# install.packages("tidyverse")
# install.packages("neuralnet")

# We can load/run the tidyverse/neuralnet library using the library() command  
# and check if the package is installed can be checked under the packages tab on 
# the right of the screen.
library(tidyverse)
library(neuralnet)


# Setting the working directory to your Lab10 folder
setwd(paste0("/Users/sanchitagarwala/Documents/FallSemester/MIS545/",
             "Deliverables/PostMid-Term/Project", sep=""))
# print(getwd())

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

# Scaling the Income, MinStars, MaxDist, MaxPrice, RestDistance, RestRating 
# RestPrice and generating IncomeScaled, MinStarsScaled, MaxDistScaled, 
# MaxPriceScaled, RestDistanceScaled, RestRatingScaled and RestPriceScaled
restTrial1 <- restTrial %>%
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
           (max(RestPrice)-min(RestPrice)))

# Setting the random seed to 591
set.seed(591)

# Randomly split the dataset into restTrial1Training (75% of records) and 
# restTrial1Testing (25% of records)
sampleSet1 <- sample(nrow(restTrial1),
                    round(nrow(restTrial1) * 0.75),
                    replace = FALSE)
restTrial1Training <- restTrial1[sampleSet, ]
restTrial1Testing <- restTrial1[ -sampleSet, ]


# Generate the neural network model to predict TryWithout using IncomeScaled, 
# MinStarsScaled, MaxDistScaled, MaxPriceScaled, RestDistanceScaled, 
# RestRatingScaled and RestPriceScaled.. Use 3 hidden layers. Use "logistic" as
# the smoothing method and set linear.output to FALSE.
restTrial1NeuralNet <- neuralnet(
  formula = TryWithout ~ IncomeScaled + MinStarsScaled + MaxDistScaled + 
    MaxPriceScaled + RestDistanceScaled + RestRatingScaled + RestPriceScaled,
  data = restTrial1Training,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE
)

# Display the neural network numeric results
print(restTrial1NeuralNet$result.matrix)

# Visualize the neural network
plot(restTrial1NeuralNet)

# Use restTrial1NeuralNet to generate probabilities on the restTrial1Testing 
# data set and store this in restTrial1Probability
restTrial1Probability <- compute(restTrial1NeuralNet,
                                restTrial1Testing)

# Displaying the probabilities from the testing dataset on the console
print(restTrial1Probability)

# Converting probability predictions into 0/1 predictions and store this into 
# restTrial1Prediction
restTrial1Prediction <-
  ifelse(restTrial1Probability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(restTrial1Prediction)

# Evaluating the model by forming a confusion matrix
restTrial1ConfusionMatrix <- table(restTrial1Testing$TryWithout,
                                  restTrial1Prediction)

# Displaying the confusion matrix on the console
print(restTrial1ConfusionMatrix)

# Calculating the model predictive accuracy
predictiveAccuracy3 <- sum(diag(restTrial1ConfusionMatrix)) /
  nrow(restTrial1Testing)

# Displaying the predictive accuracy on the console
print(predictiveAccuracy3)
