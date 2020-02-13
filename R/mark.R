#' A function for marking models during the predictive analytics course
#'
#' @param model A model resulting from a call to train
#' @return NULL
#' @importFrom stats lowess
#' @export
mark = function(model) {
  if (!inherits(model, "validated")) {
    stop("Make sure to validate your model before trying to mark it")
  }
  env = new.env()
  data(FuelEconomy, package = "AppliedPredictiveModeling", envir = env)
  cars2011 = env$cars2011
  pred = predict(model, cars2011)
  err = cars2011$FE - pred
  rmse = sqrt(mean(err * err))
  col1 = rgb(204, 74, 83, maxColorValue = 255)

  op = par(mfrow = c(1, 2), oma = c(0, 0, 1, 0),
           mar = c(3, 3, 2, 1),
           mgp = c(2, 0.4, 0), tck = -.01,
           cex.axis = 0.9, las = 1)
  on.exit(par(op))
  plot(cars2011$FE, pred, xlab = "Observed", ylab = "Fitted", col = "black",
       xlim = range(cars2011$FE),
       ylim = range(cars2011$FE),
       pch = 21, bg = "grey90", panel.first = grid())
  abline(0, 1)
  lines(lowess(cars2011$FE, pred), col = col1, lwd = 2.5, lty = 2)
  plot(cars2011$FE, err, xlab = "Observed", ylab = "Errors", col = "black",
       pch = 21, bg = "grey90", panel.first = grid())
  abline(h = 0)
  lines(lowess(cars2011$FE, err), col = col1, lwd = 2, lty = 2)
  mtext(text = paste("RMSE: ", round(rmse, 4)), outer = TRUE, line = -1)
}
