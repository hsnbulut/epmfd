#' Export epmfd objects to disk
#'
#' `export_epmfd()` writes commonly used tables from `epmfd_*` objects to
#' CSV / Excel / SPSS files, and (optionally) saves the object itself as an RDS.
#'
#' @param object  One of: `epmfd_scaled`, `epmfd_misfit`, `epmfd_clean`.
#' @param dir     Target directory. Default is the current working directory (`"."`).
#'                The directory is created if it does not exist.
#' @param prefix  File name prefix (without extension). If `NULL`, the first
#'                class name of `object` is used (e.g., `"epmfd_clean"`).
#' @param format  Output format; one of `"csv"` (default), `"xlsx"`, or `"sav"`.
#'   \itemize{
#'     \item `"csv"`: written via \pkg{readr} (\code{readr::write_csv()}).
#'     \item `"xlsx"`: requires \pkg{openxlsx} (\code{openxlsx::write.xlsx()}).
#'     \item `"sav"`: SPSS format; requires \pkg{haven} (\code{haven::write_sav()}).
#'   }
#' @param save_rds Logical; if `TRUE`, also saves the `object` as `<prefix>.rds`
#'                 in `dir` via \code{saveRDS()}.
#' @param include_misfit Logical; if `TRUE`, writes misfit tables when available
#'                 (see Details). Default = `FALSE`.
#'
#' @returns (Invisibly) a character vector of file paths that were written.
#'
#' @details
#' What gets written depends on the object class:
#' \itemize{
#'   \item \strong{`epmfd_clean`}: always writes the cleaned person-by-item data
#'         (`<prefix>_clean.<ext>`). If \code{include_misfit = TRUE} and a misfit
#'         object is attached, also writes the misfit table (`<prefix>_misfit.<ext>`).
#'   \item \strong{`epmfd_misfit`}: if \code{include_misfit = TRUE}, writes the
#'         misfit table (`<prefix>_misfit.<ext>`).
#'   \item \strong{`epmfd_scaled`}: writes a one-column-per-item summary indicating
#'         which items were kept or removed (`<prefix>_scale.<ext>`).
#' }
#'
#' When `format = "sav"`, logical columns are converted to labelled factors
#' (`FALSE`/`TRUE`) for SPSS compatibility. Writing `.sav` does not support list
#' columns; the function aborts if such columns are present.
#'
#' @section File naming:
#' Files are named `<prefix>_<name>.<format>` and placed under `dir`. For example:
#' `study1_clean.csv`, `study1_misfit.xlsx`, or `study1_scale.sav`.
#'
#' @seealso [saveRDS()], \pkg{readr}, \pkg{openxlsx}, \pkg{haven}
#'
#' @examples
#' \dontrun{
#' # Cleaned object: write cleaned data (CSV) and also save RDS
#' export_epmfd(clean_obj, dir = "results", prefix = "study1",
#'              format = "csv", save_rds = TRUE)
#'
#' # Misfit object: write misfit table to SPSS
#' export_epmfd(misfit_obj, dir = "out", format = "sav", include_misfit = TRUE)
#'
#' # Scaled object: write kept/removed item status as Excel
#' export_epmfd(scaled_obj, dir = "out", prefix = "scaleA", format = "xlsx")
#' }
#'
#' @export
export_epmfd <- function(object,
                         dir            = ".",
                         prefix         = NULL,
                         format         = c("csv", "xlsx", "sav"),
                         save_rds       = FALSE,
                         include_misfit = FALSE) {

  format <- match.arg(format)
  if (is.null(prefix)) prefix <- class(object)[1]
  fs::dir_create(dir)

  write_tbl <- function(data, name) {
    path <- fs::path(dir, paste0(prefix, "_", name, ".", format))
    data <- as.data.frame(data)

    if (format == "csv") {
      readr::write_csv(data, path)

    } else if (format == "xlsx") {
      if (!requireNamespace("openxlsx", quietly = TRUE))
        stop("Package 'openxlsx' required for xlsx export.", call. = FALSE)
      openxlsx::write.xlsx(data, path)

    } else if (format == "sav") {
      if (!requireNamespace("haven", quietly = TRUE))
        stop("Package 'haven' required for SPSS (.sav) export.", call. = FALSE)

      # convert logicals to labelled factors for SPSS
      is_logical <- vapply(data, is.logical, logical(1))
      if (any(is_logical)) {
        data[is_logical] <- lapply(data[is_logical], function(x)
          factor(x, levels = c(FALSE, TRUE), labels = c("FALSE", "TRUE")))
      }
      has_bad_list <- vapply(data, function(x) is.list(x) && !is.data.frame(x), logical(1))
      if (any(has_bad_list)) {
        stop("Cannot write .sav with list-columns: ",
             paste(names(data)[has_bad_list], collapse = ", "), call. = FALSE)
      }

      haven::write_sav(data, path)
    }

    path
  }

  paths <- character(0)

  if (inherits(object, "epmfd_clean")) {
    # always write clean data
    paths <- c(paths, write_tbl(object$clean_data, "clean"))

    # only write misfit if requested
    if (include_misfit && !is.null(object$misfit)) {
      paths <- c(paths, write_tbl(object$misfit$table, "misfit"))
    }

  } else if (inherits(object, "epmfd_misfit")) {
    if (include_misfit) {
      paths <- c(paths, write_tbl(object$table, "misfit"))
    }

  } else if (inherits(object, "epmfd_scaled")) {
    scale_df <- data.frame(
      Item   = c(object$kept, object$removed),
      Status = c(rep("Kept", length(object$kept)),
                 rep("Removed", length(object$removed))),
      stringsAsFactors = FALSE
    )
    paths <- c(paths, write_tbl(scale_df, "scale"))

  } else {
    stop("Unsupported object class.")
  }

  if (save_rds) {
    rds_path <- fs::path(dir, paste0(prefix, ".rds"))
    saveRDS(object, rds_path)
    paths <- c(paths, rds_path)
  }

  invisible(paths)
}
