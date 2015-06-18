library(caret)
library(mlbench)
library(pls)

data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)
## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)

training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
nrow(testing)


## Basic fit (using Partial Least Squares Discriminant Analysis from 'pls' package)
plsFit <- train(Class ~ ., data = training, method = "pls", preProc = c("center", "scale"))

## Using tuneLength = 15 --> set the candidate values of the tuning parameter at integers from 1 to 15
plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15,  preProc = c("center", "scale"))

## Set a method to resample the data before each fit
ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, trControl = ctrl, preProc = c("center", "scale"))

## Setting the performance function 
ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, trControl = ctrl, metric = "ROC", preProc = c("center", "scale"))
plsFit

## plot the results of the tuning
plot(plsFit)

## Use the best model to make predictions of new data
plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

## PRedict the probabilities for each category
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

## Calculate the confusion matrix (True Pos, True Neg, ...)
confusionMatrix(data = plsClasses, testing$Class)


