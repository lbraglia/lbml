#' standardize data frames
#' @param train train dataset
#' @param train train dataset
#' @param id_var id column in both the dataset
#' @param outcome_var outcome column in both the dataset
#' @param return_splitted return as splitted list of train and test
#' @export
standardize_df <- function(train, test,
                           id_var = 'Id',
                           outcome_var = 'SalePrice',
                           return_splitted = TRUE)
{
    ## browser()
    ## add NA to outcome in testing
    test[outcome_var] <- NA
    ## cbind with the same structure
    first <- c(id_var, outcome_var)
    remaining <- names(train) %without% first
    varseq <- c(first, remaining)
    all <- rbind(train[varseq], test[varseq])
    names(all)[1:2] <- c('id', 'y')
    ## split back in train and test
    source <- rep(c("train", "test"), c(nrow(train), nrow(test)))
    if (return_splitted) {
        split(all, factor(source))
    } else {
        all$source <- source
        all
    }
}
