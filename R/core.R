#' @export
rank_analyses <- function(analysis_functions = NULL,
                          analyses = NULL,
                          metric_used = list(rmse, accuracy)[[1]],
                          order_res = c("increasing", "decreasing"),
                          cv_k = 5,
                          rng_seed = 1,
                          outdir = "results",
                          outfile = sprintf("%s_analyses_performance.csv", format(Sys.time(),'%y%m%d_%H%M%S')),
                          tg_bot = NULL)
{
    datasets_functions            <- analysis_functions$datasets
    missing_imputation_functions  <- analysis_functions$missing_imputation
    outliers_functions            <- analysis_functions$outliers
    predictors_functions          <- analysis_functions$predictors 
    if (is.null(analyses)){
        analyses <- expand.grid(
            "dataset"            = names(datasets_functions),
            "missing_imputation" = names(missing_imputation_functions),
            "outliers_handling"  = names(outliers_functions),
            "predictor_used"     = names(predictors_functions))
    }
    analyses$id <- seq_len(nrow(analyses))

    ## return the crossvalidation stats for a single analysis 
    cv_performance <- function(x) {# row from analyses
        ## extract info of the analysis
        id <- x["id"]
        dataset_label <- x["dataset"]
        missing_method_label <- x["missing_imputation"]
        outliers_method_label <- x["outliers_handling"]
        predictor_method_label <- x["predictor_used"]
        ## monitoring
        msg <- sprintf("Doing #%s | dataset: %s, NA: %s, outliers: %s, predictor: %s",
                       id,
                       dataset_label,
                       missing_method_label,
                       outliers_method_label,
                       predictor_method_label)
        cat(msg, "\n")
        if (!is.null(tg_bot)){
            tg_bot$sendMessage(msg)
        }
        ## analysis setup: get functions for the analysis at hand
        dataset_f <- datasets_functions[[dataset_label]]
        missing_f <- missing_imputation_functions[[missing_method_label]]
        outlier_f <- outliers_functions[[outliers_method_label]]
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
        tg_bot$sendMessage(msg)
    }
    cv_stats <- apply(analyses, 1, cv_performance)
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
    rownames(res) <- NULL # so rowname is the rank as well which now
                          # becomes more important and handy to carry
                          # over
    res$rank <- seq_len(nrow(res))
    ## dump results to a file
    if (!is.null(outfile)){
        if (! dir.exists(outdir)) dir.create(outdir)
        path <- sprintf("%s/%s", outdir, outfile)
        write.csv(res, file = path, row.names = FALSE)
        cat("Results saved in ", outfile, ".\n")
        if (!is.null(tg_bot)){
            tg_bot$sendDocument(path)
        }
    }
    res
}


#' @export
make_predictions <- function(perf_df = NULL,
                             analysis_functions = NULL,
                             train = NULL,
                             test = NULL,
                             rng_seed = 1)
{
    datasets_functions            <- analysis_functions$datasets
    missing_imputation_functions  <- analysis_functions$missing_imputation
    outliers_functions            <- analysis_functions$outliers
    predictors_functions          <- analysis_functions$predictors
    ## make an unique final dataset
    final_db <- rbind(train, test)
    worker <- function(x){#x is a row of perf_df
        ## browser()
        ## analysis setup: get functions for the analysis at hand
        id <- x["id"]
        dataset_label <- x["dataset"]
        missing_method_label <- x["missing_imputation"]
        outliers_method_label <- x["outliers_handling"]
        predictor_method_label <- x["predictor_used"]
        msg <- sprintf("Doing #%s | dataset: %s, NA: %s, outliers: %s, predictor: %s",
                       id,
                       dataset_label,
                       missing_method_label,
                       outliers_method_label,
                       predictor_method_label)
        cat(msg, "\n")
        ## extract functions
        dataset_f <- datasets_functions[[dataset_label]]
        missing_f <- missing_imputation_functions[[missing_method_label]]
        outlier_f <- outliers_functions[[outliers_method_label]]
        predictor_f <- predictors_functions[[predictor_method_label]]
        ## choose the type of dataset
        db_used  <- dataset_f(final_db)
        ## imputation
        set.seed(rng_seed)
        imputed_db <- missing_f(db_used)
        ## outliers handling
        clean_db <- outlier_f(imputed_db)
        ## splitting
        outcome_is_na <- split(clean_db, f = is.na(clean_db$y))
        final_test <- outcome_is_na[["TRUE"]]
        final_train <- outcome_is_na[["FALSE"]]
        ## model evaluation (on train) and prediction (on test)
        set.seed(rng_seed)
        final_test$y <- predictor_f(final_train, final_test)
        res <- final_test[c("id", "y")]
        ## in case there are multiple predictions per id (think mice
        ## long) take the mean
        res <- setNames(aggregate(res$y, by = list(res$id), mean), c("id", "y"))
        if (nrow(res) != nrow(test))
            warning("returned preds don't have the provided number of rows")
        res
    }
    apply(perf_df, 1, worker)
}
