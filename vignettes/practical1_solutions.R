## ---- setup, echo = FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  install.packages("drat")

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  drat::addRepo("rcourses")
#  install.packages("jrPredictive")

## ---- echo = TRUE--------------------------------------------------------
library("jrPredictive")

## ---- echo = TRUE--------------------------------------------------------
library("caret")

## ---- echo = TRUE--------------------------------------------------------
data(FuelEconomy, package = "AppliedPredictiveModeling")

## ------------------------------------------------------------------------
m1 = train(FE ~ EngDispl, method = "lm", data = cars2010)

## ------------------------------------------------------------------------
rstd = rstandard(m1$finalModel)
plot(fitted.values(m1$finalModel), rstd)

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  abline(h = c(-2, 0, 2), col = 2:3, lty = 2:1)

## ------------------------------------------------------------------------
# There definitely appears to be some trend in the
# residuals.  The curved shape indicates that we
# potentially require some transformation of variables.
# A squared term might help.

## ------------------------------------------------------------------------
plot(cars2010$FE, fitted.values(m1$finalModel), xlab = "FE",
    ylab = "Fitted values", xlim = c(10, 75), ylim = c(10,
        75))
abline(0, 1, col = 3, lty = 2)

## ------------------------------------------------------------------------
# We seem to slightly over estimate more often than not
# in the 25-35 range. For the upper end of the range we
# seem to always under estimate the true values.

## ------------------------------------------------------------------------
qqnorm(rstd)
qqline(rstd)
plot(cars2010$EngDispl, rstd)
abline(h = c(-2, 0, 2), col = 2:3, lty = 1:2)

## ------------------------------------------------------------------------
# We are struggling to justify the assumption of
# normality in the residuals here, all of the diagnostics
# indicate patterns remain in the residuals that are
# currently unexplained by the model.

## ------------------------------------------------------------------------
# We are struggling to justify the assumption of
# normality in the residuals here, all of the diagnostics
# indicate patterns remain in the residuals that are
# currently unexplained by the model

## ------------------------------------------------------------------------
m2 = train(FE ~ poly(EngDispl, 2, raw = TRUE), data = cars2010,
    method = "lm")

## ------------------------------------------------------------------------
# The residual diagnostics indicate a better fit now that
# the quadratic term has been included.

## ------------------------------------------------------------------------
# Perhaps the residuals more closely match the assumption
# of normality under this transformation. However we need
# to be careful about interpretation now as the response
# is on the log scale. Likewise for prediction we need to
# remember to undo the transformation.

## ------------------------------------------------------------------------
m3 = train(FE ~ EngDispl + NumCyl, data = cars2010, method = "lm")

## ---- echo = TRUE--------------------------------------------------------
## points = TRUE to also show the points
plot3d(m3, cars2010$EngDispl, cars2010$NumCyl, cars2010$FE,
    points = FALSE)

## ---- echo = TRUE--------------------------------------------------------
threejs::scatterplot3js(cars2010$EngDispl, cars2010$NumCyl,
    cars2010$FE, size = 0.5)

## ---- echo = TRUE--------------------------------------------------------
m4 = train(FE ~ EngDispl * NumCyl + I(NumCyl^5), data = cars2010,
    method = "lm")

