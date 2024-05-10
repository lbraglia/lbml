covariates_sum <- function(df, ignore = c("y", "id")){
    paste(names(df) %without% ignore, collapse = " + ")
}

#' @export
step_aic <- function(train, test, model = stats::lm, ...){
    null_f <- y ~ 1
    full_f <- sprintf("y ~ %s", covariates_sum(train))
    scope <- list(upper = full_f[-2], lower = null_f[-2])
    null <- model(null_f, data = train, ...)
    ## mod_full <- lm(full_f, data = data)
    ## starting from the null model otherwise use mod_full
    step_mod <- MASS::stepAIC(null, scope = scope, direction = "both", trace = 0)
    ## extracted variables
    predict(step_mod, newdata = test)
}
