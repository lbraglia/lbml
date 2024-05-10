#' @export
xgb <- function(train, test,
                formula = y ~ . - id,
                objective = "reg:squarederror",  # For regression
                eval_metric = "rmse"             # Evaluation metric: Root Mean Squared Error
                ){
    ## browser()
    y_train <- train$y
    test$y <- 1
    y_test <- test$y
    x_train <- model.matrix(formula, data = train)[, -1]
    x_test <- model.matrix(formula, data = test)[, -1]
    ## Convert to DMatrix format
    train_data <- xgboost::xgb.DMatrix(data = as.matrix(x_train), label = y_train)
    test_data <- xgboost::xgb.DMatrix(data = as.matrix(x_test), label = y_test)
    ## set params, do estimate on train and obtain prediction on test
    params <- list(objective = objective, eval_metric = eval_metric)
    xgb_model <- xgboost::xgboost(params = params, data = train_data, nrounds = 100)
    predict(xgb_model, test_data)
}

