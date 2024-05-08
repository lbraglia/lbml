## same_predictions <- function(sub1, sub2, sel_names = c("id", "y")){
##     sub1 <- sub1[sel_names]
##     sub2 <- sub2[sel_names]
##     names(sub1)[2] <- "a"
##     names(sub2)[2] <- "b"
##     if (nrow(sub1) != nrow(sub2))
##         stop("not the same dimension")
##     m <- merge(sub1, sub2)
##     if (nrow(m) != nrow(sub1) || nrow(m) != nrow(sub2))
##         stop("merge has different rows")
##     m$are_equal <- apply(m[-1], 1, function(x) x[1] == x[2])
##     list("comparison" = m,
##          "check" = all(m$are_equal),
##          "sums" = colSums(m[-1]))
## }
## same_predictions(best_method_submission, ens1)

## to optimize further iterations see which approaches
## yields a better cv accuracy
cv_acc_tree <- function(res_df){
    cv_acc_tree <- tree::tree(mean_cv_acc ~ missing_imputation
                              + outliers_handling + predictor_used,
                              data = res_df)
    plot(cv_acc_tree)
    text(cv_acc_tree, digits = 3, pretty = 500) # 500 to avoid text truncations
    invisible(cv_acc_tree)
}

## png(file = 'results/tree.png')
## cv_acc_tree(res1)
## dev.off()
