library(caret)
library(mlbench)
library(klaR)
library(MASS)
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

ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(123)

## Run PLS first for comparison purposes
plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, trControl = ctrl, metric = "ROC", preProc = c("center", "scale"))
plsFit
plot(plsFit)
plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)
confusionMatrix(data = plsClasses, testing$Class)

## Train Regularized Discriminant Analysis model
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
rdaFit <- train(Class ~ ., data = training, method = "rda", tuneGrid = rdaGrid, trControl = ctrl, metric = "ROC")
rdaFit
plot(rdaFit)
rdaClasses <- predict(rdaFit, newdata = testing)
str(rdaClasses)
rdaProbs <- predict(rdaFit, newdata = testing, type = "prob")
head(rdaProbs)
confusionMatrix(data = rdaClasses, testing$Class)

## Compare PLS model with RDA model
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

xyplot(resamps, what = "BlandAltman") 

diffs <- diff(resamps)
summary(diffs)