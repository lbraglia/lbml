random_forest <- function(train, test, formula = factor(y) ~ .){
    mod <- randomForest::randomForest(formula, data = train)
    preds <- predict(mod, newdata = test)
    as.integer(preds) - 1
}
## random_forest(median_impute(algo_train), median_impute(algo_test))



bagging <- function(train, test, formula = factor(y) ~ .){
    p <- ncol(train) - 1
    mod <- randomForest::randomForest(formula, data = train, mtry = p)
    preds <- predict(mod, newdata = test)
    as.integer(preds) - 1
}
## bagging(median_impute(algo_train), median_impute(algo_test))
