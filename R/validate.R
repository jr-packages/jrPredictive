#' A function for validating models for submission during the predictive analytics course
#'
#' @param model A model resulting from a call to train
#' @return A model of type train
#' @export
validate = function(model = NULL){
  if (is.null(model)) stop("No model provided.")
  if (!(inherits(model, "train"))) {
    stop("The model is not an object returned by train.")
  }

  ## Don't mess up global environment
  env = new.env()
  data(FuelEconomy, package = "AppliedPredictiveModeling", envir = env)
  cars2011 = env$cars2011

  test = tryCatch(predict(model, cars2011[1, ]), error = function(e) e)
  if (inherits(test, "error")){
    stop("Your model can not successfully predict the test evaluation.")
  }
  message("Success, mark your model using the mark function.")
  class(model) = c(class(model), "validated")
  model
}
