Mode <- function(x) names(which.max(table(x)))

impute_factor <- function(x){
    ## use the median if ordered or mode otherwise; return numeric
    ## variables unchanged as well
    if (is.factor(x)){
        nas <- is.na(x)
        if (any(nas)){
            if (is.ordered(x)){
                med <- quantile(x, probs = 0.5, na.rm = TRUE, type = 1)
                x[nas] <- as.character(med)
            } else {
                mod <- Mode(x)
                x[nas] <- mod
            }
        }
        x
    } else {
        x
    }
}

#' Impute univariately using the median
#'
#' @param df a data.frame with missing variables to be imputed
#' @param ignore character vector of ignored variables in imputation
#' @examples
#' head(airquality)
#' head(impute_median(airquality))
#' @export
impute_median <- function(df, ignore = c("y", "id")){
    not_imputed <- df[names(df) %in% ignore]
    imputed <- df[names(df) %nin% ignore]
    res <- predict(caret::preProcess(imputed, method = "medianImpute"), newdata = imputed)
    ## factors are not handled by the function above ..
    res <- as.data.frame(lapply(res, impute_factor))
    cbind(not_imputed, res)
}


#' Impute using bagged trees
#'
#' @param df a data.frame with missing variables to be imputed
#' @param ignore character vector of ignored variables in imputation
#' @examples
#' head(airquality)
#' head(bag_impute(airquality))
#' @export
impute_bag <- function(df, ignore = c("y", "id")){
    not_imputed <- df[names(df) %in% ignore]
    imputed <- df[names(df) %nin% ignore]
    res <- predict(caret::preProcess(imputed, method = "bagImpute"), newdata = imputed)
    cbind(not_imputed, res)
}


#' imputation using knn (it scales everything)
#'
#' @param df a data.frame with missing variables to be imputed
#' @param ignore character vector of ignored variables in imputation
#' @examples
#' head(airquality)
#' head(impute_knn(airquality))
#' @export
impute_knn <- function(df, ignore = c("y", "id")) {
    not_imputed <- df[names(df) %in% ignore]
    imputed <- df[names(df) %nin% ignore]
    res <- predict(caret::preProcess(imputed, method = "knnImpute"), newdata = imputed)
    cbind(not_imputed, res)
}


#' imputation using mice (one imputation)
#'
#' @param df a data.frame with missing variables to be imputed
#' @param ignore character vector of ignored variables in imputation
#' @examples
#' head(airquality)
#' head(impute_mice_single(airquality))
#' @export
impute_mice_single <- function(df, ignore = c("y", "id")){
    ## use mice defaults: do 5 sims, keep the first. random guys from
    ## internet recommends against making a single imputation
    ## (https://stackoverflow.com/questions/51370292) but doesnt
    ## convince me too much for our application. At the end go with
    ## defaults
    not_imputed <- df[names(df) %in% ignore]
    imputed <- df[names(df) %nin% ignore]
    res <- mice::complete(mice::mice(imputed, method = "pmm", m = 1), action = 1)
    cbind(not_imputed, res)
}


#' imputation using mice long version (nrow increases)
#'
#' @param df a data.frame with missing variables to be imputed
#' @param m the number of imputation to rbind
#' @param ignore character vector of ignored variables in imputation
#' @examples
#' dim(airquality)
#' imp <- impute_mice_long(airquality)
#' dim(imp)
#' @export
impute_mice_long <- function(df, m = 5, ignore = c("y", "id")){
    ## do 5 sim, take all (rbind like): that is testing dataframe
    ## number of rows is mutiplied by 5
    not_imputed <- df[names(df) %in% ignore]
    imputed <- df[names(df) %nin% ignore]
    res <- mice::complete(mice::mice(imputed, method = "pmm", m = m), action = "long")
    ## if there are omitted variables from imputation add them to the results
    if (ncol(not_imputed)){
        not_imputed_rep <- do.call(rbind, list(not_imputed)[rep(1, m)])
        res <- cbind(not_imputed_rep, res)
    }
    rm <- c(".imp", ".id")
    res[names(res) %without% rm]
}
