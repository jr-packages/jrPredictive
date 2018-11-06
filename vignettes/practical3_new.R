## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, results = "hide", fig.keep = "none")

## ------------------------------------------------------------------------
data(OJ, package = "ISLR")

## ---- message = FALSE----------------------------------------------------
library("caret")
library("jrPredictive")

## ---- eval = FALSE-------------------------------------------------------
#  par(mfrow = c(4, 5), mar = c(4, 0.5, 0.5, 0.5))
#  plot(Purchase ~ ., data = OJ)

## ------------------------------------------------------------------------
m1 = train(Purchase ~ PriceCH + PriceMM,
    data = OJ, method = "glm")

## ------------------------------------------------------------------------
mean(predict(m1) != OJ$Purchase)

## ------------------------------------------------------------------------
# with no model we essentially predict according to
# proportion of observations in data
probs = table(OJ$Purchase)/nrow(OJ)
preds = sample(levels(OJ$Purchase), prob = probs)
mean(preds != OJ$Purchase)

## ---- fig.cap = "Examining the decision boundary for orange juice brand purchases by price.", echo = TRUE, fig.keep="all"----
boundary_plot(m1,OJ$PriceCH, OJ$PriceMM, OJ$Purchase,
              xlab="Price CH", ylab="Price MM")

## ------------------------------------------------------------------------
# We now have a curved decision boundary.
# There are two regions of where we would predict MM, bottom left, and a tiny one up in the top right.

## ---- warning = FALSE----------------------------------------------------
mLM = train(Purchase ~ ., data = OJ, method = "glm")

## ------------------------------------------------------------------------
## YES!

## ---- echo = TRUE--------------------------------------------------------
warnings()

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  m_log$finalModel

## ---- echo = TRUE--------------------------------------------------------
?ISLR::OJ

## ---- echo = TRUE--------------------------------------------------------
OJsub = OJ[, !(colnames(OJ) %in% c("STORE", "SalePriceCH",
    "SalePriceMM", "PriceDiff", "ListPriceDiff"))]
OJsub$Store7 = as.numeric(OJsub$Store7) - 1
m_log = train(Purchase ~ ., data = OJsub, method = "glm")

## ---- echo = TRUE--------------------------------------------------------
remove = findLinearCombos(model.matrix(Purchase ~ ., data = OJ))

## ---- echo = TRUE--------------------------------------------------------
(badvar = colnames(OJ)[remove$remove])

## ---- echo = TRUE--------------------------------------------------------
OJsub = OJ[, -remove$remove]

## ------------------------------------------------------------------------
# the corrected model
remove = findLinearCombos(model.matrix(Purchase~., data = OJ))
(badvar = colnames(OJ)[remove$remove])

## ------------------------------------------------------------------------
OJsub = OJ[,-(remove$remove)]
mLM = train(Purchase~., data = OJsub, method = "glm")
mean(predict(mLM,OJsub) == OJsub$Purchase)

## ------------------------------------------------------------------------
## could use confusionMatrix
(cmLM = confusionMatrix(predict(mLM,OJsub),OJsub$Purchase))

## ------------------------------------------------------------------------
# or
sensitivity(predict(mLM,OJsub),OJsub$Purchase)

## ------------------------------------------------------------------------
specificity(predict(mLM,OJsub),OJsub$Purchase)

## ------------------------------------------------------------------------
#The model is fairly good at picking up both positive events, person buys CH, and negative events, MM.

## ---- echo = TRUE, message = FALSE, warning = FALSE, error=FALSE---------
library("pROC")

## ---- echo = TRUE--------------------------------------------------------
curve = roc(response = OJsub$Purchase,
  predictor = predict(m_log, type = "prob")[,"CH"])
## this makes CH the event of interest
plot(curve, legacy.axes = TRUE)

## ---- message = FALSE----------------------------------------------------
mKNN = train(Purchase~., data = OJsub, method = "knn")
mLDA = train(Purchase~., data = OJsub, method = "lda")
mQDA = train(Purchase~., data = OJsub, method = "qda")
cmKNN = confusionMatrix(predict(mKNN,OJsub),OJsub$Purchase)
cmLDA = confusionMatrix(predict(mLDA,OJsub),OJsub$Purchase)
cmQDA = confusionMatrix(predict(mQDA,OJsub),OJsub$Purchase)
(info = data.frame(Model = c("logistic","knn","lda","qda"),
           Accuracy = c(cmLM$overall["Accuracy"],
               cmKNN$overall["Accuracy"],
               cmLDA$overall["Accuracy"],
               cmQDA$overall["Accuracy"]),
           Sensitivity = c(cmLM$byClass["Sensitivity"],
               cmKNN$byClass["Sensitivity"],
               cmLDA$byClass["Sensitivity"],
               cmQDA$byClass["Sensitivity"]),
           Specificity = c(cmLM$byClass["Specificity"],
               cmKNN$byClass["Specificity"],
               cmLDA$byClass["Specificity"],
               cmQDA$byClass["Specificity"])))

## ------------------------------------------------------------------------
#Logistic regression and LDA have highest accuracy, QDA is poorest at classifying events, KNN gives most false positives

## ------------------------------------------------------------------------
#Accuracy increases at first with knn before then getting worse after a peak value of 9.
(mKNN2 = train(Purchase~., data = OJsub, method = "knn",
    tuneGrid = data.frame(k = 1:30)))

## ---- message=FALSE, warning = FALSE-------------------------------------
library("jrPredictive")
data(FuelEconomy, package = "AppliedPredictiveModeling")
regKNN = train(FE~., data = cars2010, method = "knn")
regLM = train(FE~., data = cars2010, method = "lm")
regKNN= validate(regKNN)
regLM = validate(regLM)
mark(regKNN)
mark(regLM)

## ------------------------------------------------------------------------
#The KNN regression model is not as good as the linear model at predicting the test set. It overestimates more at the lower end.

## ---- echo = TRUE--------------------------------------------------------
data(FuelEconomy, package = "AppliedPredictiveModeling")

## ------------------------------------------------------------------------
mKNN = train(FE ~ ., method = "knn", data = cars2010)

## ------------------------------------------------------------------------
# create a random sample to hold out
i = sample(nrow(cars2010), 100)
# set the train control object
tc = trainControl(method = "cv", number = 1,
    index = list(Fold1 = (1:nrow(cars2010))[-i]))
# fit the model using this train control object
mKNNvs = train(FE~., method = "knn", data = cars2010,
    trControl = tc)

## ------------------------------------------------------------------------
mKNNvs2 = train(FE~., method = "knn", data = cars2010,
     trControl = tc, tuneGrid = data.frame(k= 2:20))

## ------------------------------------------------------------------------
## With set.seed(1)
mKNNvs2$bestTune

## ------------------------------------------------------------------------
tc5fold = trainControl(method = "cv", number = 5)
tc10fold = trainControl(method = "cv", number = 10)
# use 50 boot strap estimates
tcboot = trainControl(method = "boot", number = 50)

## ------------------------------------------------------------------------
mKNNcv5 = train(FE~., data = cars2010, method = "knn",
    trControl = tc5fold, tuneGrid = data.frame(k = 2:20))

mKNNcv10 = train(FE~., data = cars2010, method = "knn",
    trControl = tc10fold, tuneGrid = data.frame(k = 2:20))

mKNNboot = train(FE~., data = cars2010, method = "knn",
    trControl = tcboot, tuneGrid = data.frame(k = 2:20))
mKNNcv5$bestTune
mKNNcv10$bestTune
mKNNboot$bestTune

## ------------------------------------------------------------------------
#The k-fold cross validation estimates and bootstrap estimates all
#yield the same conclusion, however it is different to when we used
#validation set approach earlier. We could plot the results
# from each on one plot to compare further:
plot(2:20, mKNNboot$results[,2], type = "l", ylab = "RMSE",
     xlab = "k", ylim = c(3,6.5))
lines(2:20, mKNNcv10$results[,2], col = "red")
lines(2:20, mKNNcv5$results[,2], col = "blue")
lines(2:20, mKNNvs2$results[,2], col = "green")

## ------------------------------------------------------------------------
#no see previous answer

## ----"cvresamp", fig.cap = "15 fold cross validation estimates of RMSE in a K nearest neighbours model against number of nearest neighbours.", echo=TRUE, fig.keep="all"----
tc = trainControl(method = "cv", number = 15, returnResamp = "all")
m = train(FE ~ ., data = cars2010, method = "knn", tuneGrid = data.frame(k = 1:15),
    trControl = tc)
boxplot(RMSE ~ k, data = m$resample)

## ---- echo = TRUE--------------------------------------------------------
m$time

## ------------------------------------------------------------------------
mKNNvs2$time$everything
mKNNcv5$time$everything
mKNNcv10$time$everything
mKNNboot$time$everything

#The validation set approach was quickest, however we must bear in mind that the conclusion here
#was different to the other cross validation approaches. The two k-fold cross validation estimates of RMSE and the bootstrap
#estimates all agreed with each other lending more weight to their conclusions. Plus we saw in the lectures that validation set
#approach was prone to highly variable estimates meaning we could get a different conclusion using a different hold out set.
#Either of the two k--fold cross validation methods would be preferable here.

## ---- echo = TRUE--------------------------------------------------------
data(Glass, package = "mlbench")

## ---- echo = TRUE--------------------------------------------------------
fourStats = function(data, lev = NULL, model = NULL){
    # This code will use the area under the ROC curve and the
    # sensitivity and specificity values from the built in
    # twoClassSummary function
    out = twoClassSummary(data, lev = levels(data$obs), model = NULL)
    # The best possible model has sensitivity of 1 and
    # specifity of 1. How far are we from that value?
    coords = matrix(c(1, 1, out["Spec"], out["Sens"]), ncol = 2,
        byrow = TRUE)
    # return the disctance measure together with the output
    # from two class summary
    c(Dist = dist(coords)[1], out)
}


## ---- echo = TRUE--------------------------------------------------------
data(Sonar, package = "mlbench")
mod = train(Class ~ ., data = Sonar,
              method = "knn",
              # Minimize the distance to the perfect model
              metric = "Dist",
              maximize = FALSE,
              tuneLength = 20,
              trControl =
    trainControl(method = "cv", classProbs = TRUE,
                     summaryFunction = fourStats))

## ---- fig.cap="Plot of the distance from a perfect classifier measured by sensitivity and specificity against tuning parameter for a k nearest neighbour model.", echo = TRUE, fig.keep="all"----
plot(mod)

## ------------------------------------------------------------------------
maxabsres = function(data, lev = NULL, model = NULL){
    m = max(abs(data$obs - data$pred))
    return(c("Max" = m))
}
# Test with pls regression
tccustom = trainControl(method = "cv",
                     summaryFunction = maxabsres)
mPLScustom = train(FE~., data = cars2010,
                   method = "pls",
               tuneGrid = data.frame(ncomp = 1:6),
               trControl = tccustom,
               metric = "Max", maximize = FALSE)
# success
# not to suggest this is a good choice of metric

