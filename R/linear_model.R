#' template for linear model predictor
#'
#' @param train training dataset
#' @param test test dataset
#' @param formula lm formula
#' @examples
#' formulas <- list("all variables" = y ~ . - id)
#' lms <- lm_maker(formulas)
#' @export
linear_model <- function(train, test, formula) {
    mod <- stats::lm(formula, data = train)
    predict(mod, newdata = test)
}


#' construct several predictor given formula
#' 
#' @param formulas_list list of formulas
#' @examples
#' formulas <- list("all variables" = y ~ . - id)
#' lms <- lm_maker(formulas)
#' @export
lm_maker <- function(formulas_list) {
    res <- lapply(formulas_list, function(f) {
        function(train, test) linear_model(train, test, formula = f)
    })
    names(res) <- paste0("lm | ", names(res))
    res
}
