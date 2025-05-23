#' Export epmfd results to disk
#'
#' @param object  One of: `epmfd_scaled`, `epmfd_misfit`, `epmfd_clean`.
#' @param dir     Target directory. Default = current working directory.
#' @param prefix  File prefix. If `NULL`, object class is used.
#' @param format  `"csv"` (default) or `"xlsx"`.  Requires \pkg{openxlsx}
#'   when `"xlsx"` is chosen.
#' @param save_rds Logical.  Also save the object as `<prefix>.rds`.
#' @return Invisible path(s) of the written file(s).
#'
#' @examples
#' \dontrun{
#' export_epmfd(clean_obj, dir = "results", prefix = "study1")
#' }
#' @export
export_epmfd <- function(object,
                         dir      = ".",
                         prefix   = NULL,
                         format   = c("csv", "xlsx"),
                         save_rds = TRUE) {

  format <- match.arg(format)
  if (is.null(prefix))
    prefix <- class(object)[1]

  fs::dir_create(dir)

  write_tbl <- function(data, name) {
    path <- fs::path(dir, paste0(prefix, "_", name, ".", format))
    if (format == "csv") {
      readr::write_csv(data, path)
    } else {
      if (!requireNamespace("openxlsx", quietly = TRUE))
        stop("Package 'openxlsx' required for xlsx export.", call. = FALSE)
      openxlsx::write.xlsx(data, path)
    }
    path
  }

  paths <- character()

  if (inherits(object, "epmfd_clean")) {
    paths <- c(paths, write_tbl(object$clean_data, "clean"))
    if (!is.null(object$misfit))
      paths <- c(paths, write_tbl(object$misfit$table, "misfit"))
  } else if (inherits(object, "epmfd_misfit")) {
    paths <- c(paths, write_tbl(object$table, "misfit"))
  } else if (inherits(object, "epmfd_scaled")) {
    paths <- c(paths, write_tbl(
      data.frame(Item = c(object$kept, object$removed),
                 Status = c(rep("Kept", length(object$kept)),
                            rep("Removed", length(object$removed)))),
      "scale"))
  } else {
    stop("Unsupported object class.")
  }

  # optional RDS
  if (save_rds) {
    rds_path <- fs::path(dir, paste0(prefix, ".rds"))
    saveRDS(object, rds_path)
    paths <- c(paths, rds_path)
  }

  invisible(paths)
}
