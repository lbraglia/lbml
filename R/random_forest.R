#' @export
bagging <- function(train, test, formula = y ~ . - id){
    p <- ncol(train) - 1 - 1 # id excluded
    mod <- randomForest::randomForest(formula, data = train, mtry = p)
    predict(mod, newdata = test)
}
## bagging(median_impute(algo_train), median_impute(algo_test))



#' random forest template predictors (quntitative)
#'
#' @param train training dataset
#' @param test test dataset
#' @param formula formula used in the randomForest
#' @examples
#' random_forest(house_train, house_test)
#' @export
random_forest <- function(train, test, formula = y ~ . - id) {
    mod <- randomForest::randomForest(formula, data = train)
    predict(mod, newdata = test)
}


## ## old code for classification rfx
## random_forest <- function(train, test, formula = factor(y) ~ .){
##     mod <- randomForest::randomForest(formula, data = train)
##     preds <- predict(mod, newdata = test)
##     as.integer(preds) - 1
## }
## ## random_forest(median_impute(algo_train), median_impute(algo_test))


## #' create random forest
## #'
## #' @param formula_list a list of formulas to use in rf
## #' @examples
## #' formulas <- list("mod1" = y ~ .)
## #' rfs <- random_forest_maker(formulas)
## #' rfs[[1]](house_train, house_test)
## #' @export
## random_forest_maker <- function(formula_list = list (y ~ . - id)) {
##     res <- lapply(formula_list, function(f) {
##         function(train, test) random_forest(train, test, formula = f)
##     })
##     names(res) <- paste0("randomForest | ", names(res))
##     res
## }
