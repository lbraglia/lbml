#' @export
cv_rank_analyses <- function(datasets_functions = list(),
                             missing_imputation_functions = list(),
                             outliers_functions = list(),
                             predictors_functions = list(),
                             analyses = NULL,
                             metric_used = list(rmse, accuracy)[[1]],
                             order_res = c("increasing", "decreasing"),
                             cv_k = 10,
                             rng_seed = 1,
                             outdir = "submissions",
                             outfile = "analyses_performance.csv",
                             tg_bot = NULL)
{
    if (is.null(analyses)){
        analyses <- expand.grid(
            "dataset"            = names(datasets_functions),
            "missing_imputation" = names(missing_imputation_functions),
            "outliers_handling"  = names(outliers_functions),
            "predictor_used"     = names(predictors_functions))
    }
    analyses <- data.frame("id" = seq_len(nrow(analyses)), analyses)

    # return the crossvalidation stats for a single analysis 
    cv_performance <- function(x) {# row from analyses
        ## algorithm monitoring
        print(x)
        if (!is.null(tg_bot)){
            msg <- sprintf("Doing analysis: %s", paste(x, collapse = ", "))
            tg_bot$sendMessage(msg)
        }
        ## analysis setup: get functions for the analysis at hand
        dataset_label <- x["dataset"]
        dataset_f <- datasets_functions[[dataset_label]]
        missing_method_label <- x["missing_imputation"]
        missing_f <- missing_imputation_functions[[missing_method_label]]
        outliers_method_label <- x["outliers_handling"]
        outlier_f <- outliers_functions[[outliers_method_label]]
        predictor_method_label <- x["predictor_used"]
        predictor_f <- predictors_functions[[predictor_method_label]]
        ## missing and outlier handling: doit all in once starting from
        ## house_traing
        set.seed(rng_seed) # some imputations relies on sim (mice, bag)
        db_used  <- dataset_f() # here do not specify anything to use house$train
        clean_db <- missing_f(db_used)
        clean_db <- outlier_f(clean_db)
        ## CV evaluation of accuracy (the fold splitting is always the same
        ## in different methods)
        set.seed(rng_seed)
        fold_stat <- rep(NA, cv_k)
        n <- nrow(clean_db)
        fold <- sample(1:cv_k, n, replace = TRUE)
        for (f in 1:cv_k){
            ## split data in train and test starting from the clean db
            test_id   <- fold == f
            train_id  <- !test_id
            preds <- tryCatch(predictor_f(train = clean_db[train_id, ],
                                          test =  clean_db[test_id, ]),
                              error = function(x){
                                  cat("Error", x$message)
                                  rep(NA, length = sum(test_id))
                              })
            fold_stat[f] <- metric_used(clean_db[test_id, "y"], preds)
        }
        fold_stat
    }
    
    ## Doing analysis
    options(width = 100)
    cat("Provided analyses:\n")
    print(analyses, row.names = FALSE)
    if (!is.null(tg_bot)){
        msg <- sprintf("Starting analyses (n = %d)", nrow(analyses))
        tg_bot$sendMessage()
    }
    cv_stats <- apply(analyses, 1, cv_performance)
    if (!is.null(tg_bot))
        tg_bot$sendMessage("CV performance analyses done.")
    ## add mean cv stat (accuracy rmse) per method as first row
    ## if (keep_foldstat){
    ##     cv_stats <- rbind(colMeans(cv_stats), cv_stats)
    ##     stat_names <- c("mean_cv_stat",
    ##                    paste0("cv_stat_f", seq_len(nrow(cv_stats) - 1)))
    ##     rownames(cv_stats) <- stat_names
    ##     res <- cbind(analyses_df, t(cv_stats))
    ## } else {
    ##     res <- cbind(analyses_df, "mean_cv_stat" = colMeans(cv_stats))
    ## }
    ## please ... you will ignore it
    res <- cbind(analyses, "mean_cv_stat" = colMeans(cv_stats))
    ## ordering, rank and return
    order_res <- match.arg(order_res)
    row_order <- order(res$mean_cv_stat,
                       decreasing = order_res == "decreasing")
    res <- res[row_order, ]
    res$rank <- seq_len(nrow(res))
    ## dump results to a file
    if (! dir.exists(outdir)) dir.create(outdir)
    path <- sprintf("%s/%s.csv", outdir, outfile)
    write.csv(res, file = outfile, row.names = FALSE)
    ## return
    res
}
