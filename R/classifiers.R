logistic_template <- function(train, test, formula) {
    mod <- glm(formula, family = binomial, data = train)
    as.integer(predict(mod, newdata = test, type = "response") > 0.5)
}

logistic_maker <- function(formulas) {
    res <- lapply(formulas, function(f) {
        function(train, test) logistic_template(train, test, formula = f)
    })
    names(res) <- paste0("logistic | ", names(res))
    res
}

formulas <- list("mod1" = y ~ Pclass + Age,
                 "mod2" = y ~ Pclass + Age + SibSp + Parch + Embarked)
logistics <- logistic_maker(formulas)
logistics[[1]](lbml::titanic_train, lbml::titanic_test)
logistics[[2]](lbml::titanic_train, lbml::titanic_test)


## -------------------------------------------------------------------------

lda_all <- function(train, test) {
    ## browser()
    mod <- MASS::lda(y ~ ., data = train)
    preds <- predict(mod, newdata = test)$class
    ## predict.lda returns a "0"/"1" factor
    as.integer(preds) - 1
}
## lda_all(algo_train, algo_test)

## -------------------------------------------------------------------------

naive_bayes_g <- function(train, test) {
    x_train <- train[names(train) %without% "y"]
    y_train <- as.factor(train$y)
    x_test <- test[names(test) %without% "y"]
    mod <- klaR::NaiveBayes(x = x_train, grouping = y_train)
    preds <- predict(mod, newdata = x_test)$class
    as.integer(preds) - 1
}
## naive_bayes_g(algo_train, algo_test)# it's a NA problem
## naive_bayes_g(median_impute(algo_train), median_impute(algo_test))

## -------------------------------------------------------------------------

naive_bayes_k <- function(train, test){
    x_train <- train[names(train) %without% "y"]
    y_train <- as.factor(train$y)
    x_test <- test[names(test) %without% "y"]
    mod <- klaR::NaiveBayes(x = x_train, grouping = y_train,
                            usekernel = TRUE)
    preds <- predict(mod, newdata = x_test)$class
    as.integer(preds) - 1
}
## naive_bayes_k(median_impute(algo_train), median_impute(algo_test))
## -------------------------------------------------------------------------
