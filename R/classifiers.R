#' random forest template predictors (quntitative)
#'
#' @param train training dataset
#' @param test test dataset
#' @param formula formula used in the randomForest
#' @examples
#' logistic(house_train, house_test, y ~ Pclass + Age)
#' @export
logistic <- function(train, test, formula = y ~ . - id) {
    mod <- glm(formula, family = binomial, data = train)
    as.integer(predict(mod, newdata = test, type = "response") > 0.5)
}

#' create logistics models
#'
#' @param formula_list a list of formulas to use in rf
#' @examples
#' formulas <- list("mod1" = y ~ Pclass + Age,
#'                  "mod2" = y ~ Pclass + Age + SibSp + Parch + Embarked)
#' logistics <- logistic_maker(formulas)
#' logistics[[1]](titanic_train, titanic_test)
#' logistics[[2]](titanic_train, titanic_test)
#' @export
logistic_maker <- function(formula_list = list("all" = y ~ . - id)) {
    res <- lapply(formula_list, function(f) {
        function(train, test) logistic(train, test, formula = f)
    })
    names(res) <- paste0("logistic | ", names(res))
    res
}

#' Linear discriminant analysis predictor
#' 
#' @export
lda <- function(train, test, formula = y ~ . - id) {
    ## browser()
    mod <- MASS::lda(formula, data = train)
    preds <- predict(mod, newdata = test)$class
    ## predict.lda returns a "0"/"1" factor
    as.integer(preds) - 1
}

#' Naive bayes (gaussian) predictor
#' 
#' @examples
#' ## naive_bayes_g(algo_train, algo_test)# it's a NA problem
#' ## naive_bayes_g(median_impute(algo_train), median_impute(algo_test))
#' @export
naive_bayes_g <- function(train, test) {
    x_train <- train[names(train) %without% "y"]
    y_train <- as.factor(train$y)
    x_test <- test[names(test) %without% "y"]
    mod <- klaR::NaiveBayes(x = x_train, grouping = y_train)
    preds <- predict(mod, newdata = x_test)$class
    as.integer(preds) - 1
}


#' Naive bayes (kernel) predictor
#' 
#' @examples
#' # naive_bayes_k(median_impute(algo_train), median_impute(algo_test))
#' @export
naive_bayes_k <- function(train, test){
    x_train <- train[names(train) %without% "y"]
    y_train <- as.factor(train$y)
    x_test <- test[names(test) %without% "y"]
    mod <- klaR::NaiveBayes(x = x_train, grouping = y_train,
                            usekernel = TRUE)
    preds <- predict(mod, newdata = x_test)$class
    as.integer(preds) - 1
}


#' Support vector machine predictor
#' 
#' @examples
#' ## svm(algo_train, algo_test)
#' @export
svm <- function(train, test,
                formula = y ~ . - id,
                kernel = "radial",
                cost = 1){
    mod <- e1071::svm(formula = as.formula(formula),
                      data = train,
                      kernel = kernel,
                      cost = cost)
    test$y <- NULL
    predict(mod, newdata = test)
}

#' Support vector machines maker
#'
#' 
#' @examples
#' svms <- svm_maker("formula" = "y ~ . - id",
#'                   "kernel"  = c("linear", "radial", "polynomial"),
#'                   "cost" = 10^{-2:2})
#' ## svms[[1]](algo_train, algo_test)
#' ## svms[[9]](algo_train, algo_test)
#' @export
svm_maker <- function(...) {
    mods <- expand.grid(..., stringsAsFactors = FALSE)
    mods$desc <- apply(
        mods, 1,
        function(x) sprintf("svm | formula: %s, kernel: %s, cost: %s",
                            x["formula"],
                            x["kernel"],
                            x["cost"]
                            ))
    lapply(split(mods, mods$desc), function(x){
        function(train, test)
            svm(train, test,
                formula = x$formula,
                kernel = x$kernel,
                cost = x$cost)
    })
}
