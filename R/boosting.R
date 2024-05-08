#' boosting model template/single model
#' 
#' @param train train dataset
#' @param test  test dataset
#' @param formula gbm::gbm formula
#' @param n_trees gbm::gbm n.trees
#' @param distribution gbm::gbm distribution
#' 
#' @param ... boosting parameters
#' @export 
boosting <- function(train, test,
                     formula = y ~ . - id,
                     n_trees = 10,
                     distribution = c("gaussian", "bernoulli", "multinomial", "poisson", "coxph")){
    ## browser()
    distribution <- match.arg(distribution)
    mod <- gbm::gbm(formula = as.formula(formula),
                    data = train,
                    n.trees = n_trees,
                    distribution = distribution,
                    bag.fraction = 1)
    predict(mod, newdata = test, n.trees = n_trees,
            type = "response")
}

#' boosting model constructor
#
#' @param ... boosting parameters
#' @examples
#' boostings <- boosting_maker("formula" = "y ~ . - id",
#'                             "n_trees"  = c(10, seq(from = 25, to = 200, by = 25)),
#'                             "distribution" = "gaussian")
#' @export 
boosting_maker <- function(...) {
    mods <- expand.grid(..., stringsAsFactors = FALSE)
    mods$desc <- apply(mods, 1,
                       function(x) sprintf(
                                       "boosting | n_trees: %s, formula: %s, distribution: %s",
                                       x["n_trees"],
                                       x["formula"],
                                       x["distribution"]
                                   ))
    lapply(split(mods, mods$desc), function(x){
        function(train, test)
            boosting(train, test,
                     formula = x$formula,
                     n_trees = x$n_trees)
    })
}

