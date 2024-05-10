#' @export
ridge <- function(train, test, formula = y ~ . - id){
    ## estrarre
    x_train <- model.matrix(formula, data = train)[, -1]
    y_train <- train$y
    ## espediente se no model matrix non fa
    test$y <- 1
    x_test <- model.matrix(formula, data = test)[, -1]
    ## stima
    mod <- glmnet::cv.glmnet(x = x_train, y = y_train, alpha = 0)
    best_lambda <- mod$lambda.min
    res <- predict(mod,
                   s = best_lambda,
                   newx = x_test,
                   x = x_train,
                   y = y_train)
    dim(res) <- NULL
    res
}

## ridge(algo_train, algo_test)

#' @export
lasso <- function(train, test, formula = y ~ . - id){
    ## estrarre
    x_train <- model.matrix(formula, data = train)[, -1]
    y_train <- train$y
    ## espediente se no model matrix non fa
    test$y <- 1
    x_test <- model.matrix(formula, data = test)[, -1]
    ## stima
    mod <- glmnet::cv.glmnet(x = x_train, y = y_train, alpha = 1)
    best_lambda <- mod$lambda.min
    res <- predict(mod,
                   s = best_lambda,
                   newx = x_test,
                   x = x_train,
                   y = y_train)
    dim(res) <- NULL
    res
}
