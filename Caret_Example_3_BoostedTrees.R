library(caret)
library(C50)
library(pROC)
data(churn)

str(churnTrain)

predictors <- names(churnTrain)[names(churnTrain) != "churn"]

allData <- rbind(churnTrain, churnTest)

set.seed(1)
inTrainingSet <- createDataPartition(allData$churn, p = .75, list = FALSE)
churnTrain <- allData[ inTrainingSet,]
churnTest <- allData[-inTrainingSet,]

## For this presentation, we will stick with the original split

numerics <- c("account_length", "total_day_calls", "total_night_calls")
## Determine means and sd's
procValues <- preProcess(churnTrain[,numerics], method = c("center", "scale", "YeoJohnson"))
## Use the predict methods to do the adjustments
trainScaled <- predict(procValues, churnTrain[,numerics])
testScaled <- predict(procValues, churnTest[,numerics])

procValues

## Use Boosted Trees model (previously Adaboost)
library(gbm)

# The gbm function does not accept factor response values so we
# will make a copy and modify the outcome variable

forGBM <- churnTrain
forGBM$churn <- ifelse(forGBM$churn == "yes", 1, 0)

# gbmFit <- gbm(formula = churn ~ ., distribution = "bernoulli", data = forGBM, n.trees = 2000, interaction.depth = 7, shrinkage = 0.01, verbose = TRUE)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode = 10)


gbmTune <- train(churn ~ ., data = churnTrain, method = "gbm", metric = "ROC", tuneGrid = grid, verbose = FALSE, trControl = ctrl)

p <- ggplot(gbmTune) + theme(legend.position = "top")
print(p)

gbmPred <- predict(gbmTune, churnTest)
str(gbmPred)

gbmProbs <- predict(gbmTune, churnTest, type = "prob")
str(gbmProbs)

confusionMatrix(gbmPred, churnTest$churn)


rocCurve <- roc(response = churnTest$churn,
                predictor = gbmProbs[, "yes"],
                levels = rev(levels(churnTest$churn)))
rocCurve


# Default metrics
## Regression: "RMSE" and "Rsquared"
## Classification: "Accuracy" and "Kappa"
## Custom: summaryFunction = <function name> --> should also modify metric to one of the arguments (see example in ?train)

# Models
# List of all models: http://topepo.github.io/caret/modelList.html

# Classification
# < 100K samples --> Linear SVC
        # if not working AND NOT text data --> kNN Classifier
                                      # if not working --> SVC Ensemble Classifiers
        # if not working AND text data --> Naive Bayes

# Regression
# <100K samples AND few important features --> ElasticNet Lasso
# <100K samples AND many important features --> RidgeRegression SVR
# >100K samples --> SGD Regressor

# Clustering
# known # of categories/clusters AND < 10K samples --> K means --> (not working) Spectral Clustering GMM
#                                    > 10K samples --> MiniBatch K means
# unknown # of categories/clusters --> MeanShift VBGMM

# Dimensionality reduction
# --> Randomized PCA --> (not working) AND <10K samples --> Isomap spectral embedding --> (not working) LLE
                                    #  AND >10K samples --> kerel approximation

# k-Nearest Neighbors: "kknn", from 'kknn', params:kmax,distance,kernel
# k-Nearest Neighbor: "knn", from 'base', params: k

# Support Vector Machine with Linear Kernel: "svmLinear", from 'kernlab', params: C

# Stochastc gradient boosting: "gbm", from 'gbm', params: n.trees, interaction.depth, shrinkage, n.minobsinnode