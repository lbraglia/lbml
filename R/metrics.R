#' mean accuracy of a classifier
#' 
#' @param y real value
#' @param yhat predicted value
#' 
#' @export
accuracy <- function(real, pred){
    correct <- real == pred
    mean(correct, na.rm = TRUE)
}


#' rmse of numerical predictions
#' 
#' @param y real value
#' @param yhat predicted value
#' 
#' @export
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
