## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE, message = FALSE--------------------------------------
library("caret")
data(FuelEconomy, package = "AppliedPredictiveModeling")
set.seed(25)

## ---- echo = TRUE--------------------------------------------------------
data("FuelEconomy", package = "AppliedPredictiveModeling")

## ------------------------------------------------------------------------
mLM = train(FE~EngDispl+NumCyl+NumGears, method = "lm", data = cars2010)

## ------------------------------------------------------------------------
res = resid(mLM)
(trainRMSE = sqrt(mean(res*res)))

## ------------------------------------------------------------------------
## pick an index for samples
## floor just rounds down so we only try to sample a
## whole number
index = sample(nrow(cars2010),floor(nrow(cars2010)/2))
## set up a train control object
tcVS = trainControl(method = "cv", index = list(
    Fold1 = (1:nrow(cars2010))[-index]))
## train the model
mLMVS = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tcVS)

## ------------------------------------------------------------------------
# it's larger, often training error under estimates test error
getTrainPerf(mLMVS)
trainRMSE

## ------------------------------------------------------------------------
# set up train control objects
tcLOOCV = trainControl(method = "LOOCV")
tcKFOLD = trainControl(method = "cv", number = 10)
tcBOOT = trainControl(method = "boot")

# train the model
mLMLOOCV = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tcLOOCV)
mLMKFOLD = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tcKFOLD)
mLMBOOT = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tcBOOT)

## ------------------------------------------------------------------------
getTrainPerf(mLMVS)
getTrainPerf(mLMLOOCV)
getTrainPerf(mLMKFOLD)
getTrainPerf(mLMBOOT)
# all lower than validation set, we mentioned it tended to
# over estimate test error

## ------------------------------------------------------------------------
mLMVS$times$everything
mLMLOOCV$times$everything
mLMKFOLD$times$everything
mLMBOOT$times$everything

## ------------------------------------------------------------------------
# a number of trainControl objects 
tc2 = trainControl(method = "cv", number = 2)
tc5 = trainControl(method = "cv", number = 5)
tc10 = trainControl(method = "cv", number = 10)
tc15 = trainControl(method = "cv", number = 15)
tc20 = trainControl(method = "cv", number = 20)
# train the model using each
mLM2 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc2)
mLM5 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc5)
mLM10 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc10)
mLM15 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc15)
mLM20 = train(FE~EngDispl+NumCyl+NumGears, method = "lm",
    data = cars2010, trControl = tc20)
# use a data frame to store all of the relevant information
(info = data.frame("Folds" = c(2,5,10,15,20),
    "Time" = c(mLM2$times$everything[1],
        mLM5$times$everything[1],
        mLM10$times$everything[1],
        mLM15$times$everything[1],
        mLM20$times$everything[1]),
    "Estimate" = c(mLM2$results$RMSE,
                   mLM5$results$RMSE,
                   mLM10$results$RMSE,
                   mLM15$results$RMSE,
                   mLM20$results$RMSE)))

## ------------------------------------------------------------------------
# as there are more folds it takes longer to compute,
# not an issue with such a small model but something
# to consider on more complicated models.
# Estimates are going down as the number of folds increases.
# This is because for each held out fold we are using a greater
# proportion of the data in training so expect to get a better
# model.

## ---- echo = TRUE--------------------------------------------------------
## load the data in 
data(diabetes, package = "lars")
diabetesdata = cbind(diabetes$x,"y" = diabetes$y)

## ---- warning=FALSE------------------------------------------------------
modelformula = as.formula(paste("y~(.)^2 + ",
    paste0("I(",
           colnames(diabetesdata),"^2)",
          collapse = "+")
    ))
mLASSO = train(modelformula, data = diabetesdata,
    method = "lasso")
mRIDGE = train(modelformula, data = diabetesdata,
    method = "ridge")
mENET = train(modelformula, data = diabetesdata,
    method = "enet")

## ------------------------------------------------------------------------
# examine previous output then train over a finer grid near 
# the better end
mLASSOfine = train(modelformula,data = diabetesdata,
    method = "lasso", tuneGrid = data.frame(fraction = seq(0.1,0.5,by = 0.05)))
mLASSOfine$results

## ------------------------------------------------------------------------
# best still right down at the 0.1 end
mLASSOfiner = train(modelformula,data = diabetesdata,
    method = "lasso",
    tuneGrid = data.frame(fraction = seq(0.01,0.15,by = 0.01)))
mLASSOfiner$results

## ------------------------------------------------------------------------
# best is
mLASSOfiner$bestTune

## ------------------------------------------------------------------------
mRIDGEfine = train(modelformula,data = diabetesdata,
    method = "ridge",
    tuneGrid = data.frame(lambda = seq(0,0.1,by = 0.01)))
mRIDGEfine$results

## ------------------------------------------------------------------------
mRIDGEfiner = train(modelformula,data = diabetesdata,
    method = "ridge",
    tuneGrid = data.frame(lambda = seq(0.005,0.03,by = 0.001)))
mRIDGEfiner$results

## ------------------------------------------------------------------------
# the best one
mRIDGEfiner$bestTune

## ------------------------------------------------------------------------
mENETfine = train(modelformula, data = diabetesdata,
    method = "enet", tuneGrid = expand.grid(
                         lambda = c(0.001,0.01,0.1),
                         fraction = c(0.4,0.5,0.6)
    ))
mENETfine$results

## ------------------------------------------------------------------------
mENETfiner = train(modelformula, data = diabetesdata,
    method = "enet", tuneGrid = expand.grid(
                         lambda = seq(0.001,0.1,length.out = 10),
                         fraction = 0.5))
mENETfiner$results

## ------------------------------------------------------------------------
mENETfiner$bestTune

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  coef = predict(mLASSO$finalModel,
#    mode = "fraction",
#    s = mLASSO$bestTune$fraction,# which ever fraction was chosen as best
#    type = "coefficients"
#  )

## ---- eval = FALSE-------------------------------------------------------
#  # use predict to find the coefficients
#  coefLASSO = predict(mLASSOfiner$finalModel, mode = "fraction",
#          type = "coefficient", s = mLASSO$bestTune$fraction,
#          )
#  sum(coefLASSO$coefficients != 0)

## ------------------------------------------------------------------------
## [1] 57

## ---- eval = FALSE-------------------------------------------------------
#  coefENET= predict(mENETfiner$finalModel, mode = "fraction",
#          type = "coefficient", s = mENET$bestTune$fraction
#          )
#  sum(coefENET$coefficients != 0)

## ------------------------------------------------------------------------
## [1] 24

## ------------------------------------------------------------------------
mPCR = train(modelformula, data = diabetesdata, method = "pcr",
             tuneGrid = data.frame(ncomp = 1:7))
mPLS = train(modelformula, data = diabetesdata, method = "pls",
             tuneGrid = data.frame(ncomp= 1:7))
mPLS2 = train(modelformula, data = diabetesdata, method = "pls",
             tuneGrid = data.frame(ncomp= 5:15))
getTrainPerf(mLASSOfiner)
getTrainPerf(mRIDGEfiner)
getTrainPerf(mENETfiner)
getTrainPerf(mPCR)
getTrainPerf(mPLS2)

## ---- eval = FALSE-------------------------------------------------------
#  #The elastic net model has the lowest estimated test error, all are fairly similar. The elastic net model suggests only 21 non-zero
#  #coefficients out of all of those included in the model.

