## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, results = "hide", fig.keep = "none")

## ---- echo = FALSE, message = FALSE--------------------------------------
library("caret")

## ------------------------------------------------------------------------
modInfo = getModelInfo(model = "lda", regex = FALSE)[[1]]
names(modInfo)

## ---- echo = TRUE--------------------------------------------------------
customModel = list(
  label = "custom",
  library = modInfo$library,
  type = modInfo$type,
  parameters = data.frame(
    parameter = c("threshold"),
    class = c("numeric"),
    label = c("Probability Cutoff")
  ),
  grid = function(x, y, len = NULL) {
     data.frame(threshold = seq(.01, .99, length = len))
     },
  loop = modInfo$loop,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (length(levels(y)) != 2)
       # we have only added the warning here
       stop("This works only for 2-class problems")
    lda(x, y, ...)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    class1p = predict(modelFit, newdata)$posterior[, 1]
  ## Raise the threshold for class #1 and a higher level of
  ## evidence is needed to call it class 1 so it should
  ## decrease sensitivity and increase specificity
    out = ifelse(
    class1p >= modelFit$tuneValue$threshold,
    modelFit$obsLevels[1],
    modelFit$obsLevels[2]
    )
    out
  },
  prob = modInfo$prob,
  predictors = modInfo$predictors,
  tags = modInfo$tags,
  levels = modInfo$levels,
  sort = modInfo$sort
  )

## ---- echo = TRUE, error = TRUE------------------------------------------
fourStats = function(data, lev = NULL, model = NULL) {
    out = twoClassSummary(data, lev = levels(data$obs), model = NULL)
    coords = matrix(c(1, 1, out["Spec"], out["Sens"]), ncol = 2,
        byrow = TRUE)
    c(Dist = dist(coords)[1], out)
}
set.seed(9)
data(Sonar, package = "mlbench")
mod = train(Class ~ ., data = Sonar, method = customModel,
    metric = "Dist", maximize = FALSE, tuneLength = 20, trControl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = fourStats))

## ---- echo = TRUE, fig.cap = "Using the standard caret plot functions with our custom model.", fig.keep="all", error = TRUE----
plot(mod)
plot(varImp)

