#' Export kaggle competition submission
#'
#' @param df data.frame exported
#' @param output_names column names in the export file
#' @param input_names column names to consider in the df data.frame
#' @param directory where to export
#' @param outfile file name
#' @param tg_bot telegram::TGBot object already set up with default
#'     chat_id where the file will be posted as well
#' @export
export_submission <- function(df,
                              output_names = c("Id","SalePrice"),
                              input_names = c("id", "y"),
                              outdir = "submissions",
                              outfile = "submission",
                              tg_bot = NULL)
{
    ## path and directory setup
    if (! dir.exists(outdir)) dir.create(outdir)
    path <- sprintf("%s/%s.csv", outdir, outfile)
    ## export
    subm <- setNames(df[input_names], output_names)
    write.csv(subm, file = path, row.names = FALSE)
    ## telegram courtesy
    if (!is.null(tg_bot)){
        tg_bot$sendDocument(path)
    }
}
