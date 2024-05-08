export_submission <- function(submission_df,
                              fname = "submission",
                              output_names = c("Id","SalePrice")){
    subm <- setNames(submission_df[c("id", "y")], output_names)
    write.csv(subm,
              file  = sprintf("submissions/%s.csv", fname),
              row.names = FALSE)
}
