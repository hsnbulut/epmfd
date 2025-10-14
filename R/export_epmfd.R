#' Export epmfd objects to disk
#'
#' `export_epmfd()` writes commonly used tables from `epmfd_*` objects to
#' CSV / Excel / SPSS files, and (optionally) saves the object itself as an RDS.
#'
#' @param object  One of: `epmfd_scaled`, `epmfd_misfit`, `epmfd_clean`.
#' @param dir     Target directory. If `NULL` (default), **no files are written**;
#'                instead, the function returns the tables as a named list.
#'                If provided, the directory must exist or will be created.
#' @param prefix  File name prefix (without extension). If `NULL`, the first
#'                class name of `object` is used (e.g., `"epmfd_clean"`).
#' @param format  Output format; one of `"csv"` (default), `"xlsx"`, or `"sav"`.
#'   \itemize{
#'     \item `"csv"`: written via \pkg{readr} (\code{readr::write_csv()}).
#'     \item `"xlsx"`: requires \pkg{openxlsx} (\code{openxlsx::write.xlsx()}).
#'     \item `"sav"`: SPSS format; requires \pkg{haven} (\code{haven::write_sav()}).
#'   }
#' @param save_rds Logical; if `TRUE` and `dir` is provided, also saves the `object`
#'                 as `<prefix>.rds` in `dir` via \code{saveRDS()}.
#' @param include_misfit Logical; if `TRUE`, writes/returns misfit tables when available
#'                 (see Details). Default = `FALSE`.
#'
#' @return If `dir` is `NULL`, a **named list** containing the tables that would be
#'   written (e.g., `clean`, `misfit`, `scale`). If `dir` is non-`NULL`, (invisibly)
#'   a character vector of file paths that were written.
#'
#' @details
#' What is produced depends on the object class:
#' \itemize{
#'   \item \strong{`epmfd_clean`}: cleaned person-by-item data (`clean`);
#'         if \code{include_misfit = TRUE} and a misfit object is attached, also `misfit`.
#'   \item \strong{`epmfd_misfit`}: if \code{include_misfit = TRUE}, `misfit`.
#'   \item \strong{`epmfd_scaled`}: item status summary (`scale`).
#' }
#'
#' When `format = "sav"`, logical columns are converted to labelled factors
#' (`FALSE`/`TRUE`) for SPSS compatibility. Writing `.sav` does not support list
#' columns; the function aborts if such columns are present.
#'
#' @section File naming (when `dir` is provided):
#' Files are named `<prefix>_<name>.<format>` under `dir`. For example:
#' `study1_clean.csv`, `study1_misfit.xlsx`, or `study1_scale.sav`.
#'
#' @seealso [saveRDS()], \pkg{readr}, \pkg{openxlsx}, \pkg{haven}
#'
#' @examplesIf FALSE
#' \donttest{
#'   # Minimal toy objects created inside the example ----
#'   set.seed(1)
#'   toy_clean <- data.frame(
#'     I1 = sample(0:1, 6, TRUE),
#'     I2 = sample(0:1, 6, TRUE)
#'   )
#'   toy_misfit <- data.frame(
#'     person = 1:6, Gpn = runif(6), U3p = runif(6)
#'   )
#'
#'   clean_obj <- structure(
#'     list(clean_data = toy_clean,
#'          misfit     = list(table = toy_misfit)),
#'     class = "epmfd_clean"
#'   )
#'
#'   misfit_obj <- structure(
#'     list(table = toy_misfit, method = "mokken"),
#'     class = "epmfd_misfit"
#'   )
#'
#'   scaled_obj <- structure(
#'     list(kept = c("I1", "I2"), removed = character()),
#'     class = "epmfd_scaled"
#'   )
#'
#'   # 1) No writing: return list
#'   lst <- export_epmfd(clean_obj, dir = NULL, include_misfit = TRUE)
#'   str(lst)
#'
#'   # 2) Write to a temporary directory (CRAN policy)
#'   tmpdir <- tempdir()
#'   export_epmfd(clean_obj,  dir = tmpdir, prefix = "study1", format = "csv",
#'                save_rds = TRUE)
#'
#'   # Optional formats guarded by Suggests (run only if installed)
#'   if (requireNamespace("haven", quietly = TRUE)) {
#'     export_epmfd(misfit_obj, dir = tmpdir, format = "sav",
#'                  include_misfit = TRUE)
#'   }
#'   if (requireNamespace("openxlsx", quietly = TRUE)) {
#'     export_epmfd(scaled_obj, dir = tmpdir, prefix = "scaleA",
#'                  format = "xlsx")
#'   }
#' }
#' @export
export_epmfd <- function(object,
                         dir            = NULL,
                         prefix         = NULL,
                         format         = c("csv", "xlsx", "sav"),
                         save_rds       = FALSE,
                         include_misfit = FALSE) {

  format <- match.arg(format)
  if (is.null(prefix)) prefix <- class(object)[1]

  # Yardımcı: tabloyu dosyaya yaz veya döndür
  write_tbl <- function(data, name) {
    data <- as.data.frame(data)

    # Eğer dir verilmemişse, yazma — sadece listeye koymak için döndür.
    if (is.null(dir)) return(structure(list(name = name, data = data), class = "epmfd_export_unit"))

    # Dir verilmişse, mevcut değilse oluştur (kullanıcı bilinciyle çağırdığı için OK)
    fs::dir_create(dir)
    path <- fs::path(dir, paste0(prefix, "_", name, ".", format))

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

  # Çıktı biriktiriciler
  written_paths <- character(0)
  return_units  <- list()

  if (inherits(object, "epmfd_clean")) {
    # always produce clean data
    res <- write_tbl(object$clean_data, "clean")
    if (is.null(dir)) return_units[["clean"]] <- res$data else written_paths <- c(written_paths, res)

    # only misfit if requested & available
    if (include_misfit && !is.null(object$misfit)) {
      res <- write_tbl(object$misfit$table, "misfit")
      if (is.null(dir)) return_units[["misfit"]] <- res$data else written_paths <- c(written_paths, res)
    }

  } else if (inherits(object, "epmfd_misfit")) {
    if (include_misfit) {
      res <- write_tbl(object$table, "misfit")
      if (is.null(dir)) return_units[["misfit"]] <- res$data else written_paths <- c(written_paths, res)
    }

  } else if (inherits(object, "epmfd_scaled")) {
    scale_df <- data.frame(
      Item   = c(object$kept, object$removed),
      Status = c(rep("Kept", length(object$kept)),
                 rep("Removed", length(object$removed))),
      stringsAsFactors = FALSE
    )
    res <- write_tbl(scale_df, "scale")
    if (is.null(dir)) return_units[["scale"]] <- res$data else written_paths <- c(written_paths, res)

  } else {
    stop("Unsupported object class.")
  }

  # RDS yalnızca dir verilmişse yazılır (aksi halde yazma yok)
  if (isTRUE(save_rds) && !is.null(dir)) {
    fs::dir_create(dir)
    rds_path <- fs::path(dir, paste0(prefix, ".rds"))
    saveRDS(object, rds_path)
    written_paths <- c(written_paths, rds_path)
  }

  # dir yoksa: tabloları döndür
  if (is.null(dir)) return(return_units)

  # dir varsa: yazılan yolları görünmez döndür
  invisible(written_paths)
}
