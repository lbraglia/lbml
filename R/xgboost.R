#' @export
xgb <- function(train, test){
    ## browser()
    target_column <- "y"
    predictors <- names(train) %without% c("id", "y")
    ## Convert data to DMatrix format
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train[, predictors]),
                                   label = train[, target_column])
    test$y <- 1
    dtest <- xgboost::xgb.DMatrix(data = as.matrix(test[, predictors]),
                                  label = test[, target_column])
    ## Set parameters for XGBoost
    params <- list(
        objective = "reg:squarederror",  # For regression
        eval_metric = "rmse"              # Evaluation metric: Root Mean Squared Error
    )
    ## Train the XGBoost model
    xgb_model <- xgboost::xgboost(params = params, data = dtrain, nrounds = 100)
    ## Make predictions on the test set
    predict(xgb_model, dtest)
}
