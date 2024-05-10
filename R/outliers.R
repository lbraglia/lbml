outliers_cap_iqr_worker <- function(x) {
    ## winsorize a single variable (only numeric variables,
    ## not factors, return unchanged)
    if (is.numeric(x)){
        iqr <- IQR(x, na.rm = TRUE)
        q1 <- quantile(x, probs = 0.25, na.rm = TRUE)
        q3 <- quantile(x, probs = 0.75, na.rm = TRUE)
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        ifelse(x < lower, lower, ifelse(x > upper, upper, x))
    } else {
        x
    }
}

#' cap the outliers using DescTools::Winsorize
#'
#' @param df a data.frame with possible outliers to be capped
#' @param ignore character vector of ignored variable in imputation
#' @examples
#' x <- airquality
#' x[1,1] <- 1e06
#' head(outliers_cap_iqr(x))
#' @export
outliers_cap_iqr <- function(df, ignore = c("y", "id")) {
    ## cap/winsorize a full dataset using the worker on each column
    searched <- names(df) %without% ignore
    df[searched] <- lapply(df[searched], outliers_cap_iqr_worker)
    df
}

## --------------------------------------------------------------------

outliers_cap_winsorize_worker <- function(x) {
    ## winsorize a single variable (only numeric variables,
    ## not factors, return unchanged)
    if (is.numeric(x)){
        DescTools::Winsorize(x, na.rm = TRUE)
    } else {
        x
    }
}

#' cap the outliers using DescTools::Winsorize
#'
#' @param df a data.frame with possible outliers to be capped
#' @param ignore character vector of ignored variable in imputation
#' @examples
#' x <- airquality
#' x[1,1] <- 1e06
#' head(outliers_cap_winsorize(x))
#' @export
outliers_cap_winsorize <- function(df, ignore = c("y", "id")) {
    ## cap/winsorize a full dataset using the worker on each column
    searched <- names(df) %without% ignore
    df[searched] <- lapply(df[searched], outliers_cap_winsorize_worker)
    df
}

## --------------------------------------------------------------------

outliers_identifier_z <- function(x) {
    ## identify outliers in a single numeric variable based on z scores
    if (is.numeric(x)){
        z <- scale(x)
        (!is.na(z)) & (abs(z) > 3)
    } else {
        rep(FALSE, length = length(x))
    }
}

#' trim observations with outliers based on z-scores
#'
#' @param df a data.frame with possible outliers to be capped
#' @param ignore character vector of ignored variable in imputation
#' @examples
#' x <- airquality
#' x[1,1] <- 1e06
#' head(outliers_trim_z(x))
#' @export
outliers_trim_z <- function(df, ignore = c("y", "id")){
    ## identify rows with at least an outliers on a single variable
    ## and remove them
    searched <- names(df) %without% ignore
    outlying_obs <- as.data.frame(lapply(df[searched], outliers_identifier_z))
    outlier_units <- rowSums(outlying_obs) >= 1
    df[!outlier_units, ]
}

## --------------------------------------------------------------------


outliers_identifier_iqr <- function(x) {
    ## identify outliers in a single numeric variable based on IQR
    if (is.numeric(x)){
        iqr <- IQR(x, na.rm = TRUE)
        q1 <- quantile(x, probs = 0.25, na.rm = TRUE)
        q3 <- quantile(x, probs = 0.75, na.rm = TRUE)
        (!is.na(x)) & (x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr)
    } else {
        rep(FALSE, length = length(x))
    }
}

#' trim observations with outliers based on IQR
#'
#' @param df a data.frame with possible outliers to be capped
#' @param ignore character vector of ignored variable in imputation
#' @examples
#' x <- airquality
#' x[1,1] <- 1e06
#' head(outliers_trim_iqr(x))
#' @export
outliers_trim_iqr <- function(df, ignore = c('y',"id")){
    ## same as outliers_trim_z but using outliers_identifier_iqr
    searched <- names(df) %without% ignore
    outlying_obs <- as.data.frame(lapply(df[searched], outliers_identifier_iqr))
    outlier_units <- rowSums(outlying_obs) >= 1
    df[!outlier_units, ]
}

## --------------------------------------------------------------------

#' Ignore the outliers
#'
#' it returns the dataframe unchanged
#' 
#' @param df a data.frame with possible outliers
#' @param ignore character vector of ignored variable in imputation
#' @export
outliers_none <- function(df, ignore = c("y", "id")) {
    df
}
