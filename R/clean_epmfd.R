#' Remove misfitting persons from an epmfd_misfit object
#'
#' `clean_epmfd()` removes individuals flagged as misfitting according to
#' a chosen rule and returns a cleaned dataset that can be directly passed
#' to [scale_epmfd()].
#'
#' @section Criterion:
#' - `"union"`: A person is removed if **at least one** statistic (e.g., Gnp, U3p, lpz)
#'   flags them as misfitting. This is stricter.
#' - `"intersection"`: A person is removed only if **all** statistics flag them
#'   as misfitting. This is more lenient.
#'
#' @param misfit An `epmfd_misfit` object returned by [misfit_epmfd()].
#' @param criterion Character string, either `"union"` (default) or `"intersection"`.
#'
#' @returns An `epmfd_clean` list with the following elements:
#' \itemize{
#'   \item `raw`: An `epmfd_raw` object containing only the retained persons and
#'         items, directly usable in [scale_epmfd()].
#'   \item `clean_data`: The cleaned raw data frame (persons × kept items).
#'   \item `n_removed`: Number of persons removed.
#'   \item `criterion`: The applied decision rule.
#'   \item `misfit`: The original `epmfd_misfit` object.
#' }
#'
#' @details
#' The function relies on the columns in \code{misfit$table}:
#' \itemize{
#'   \item `misfit_any`: Indicates whether at least one statistic flagged a person.
#'   \item `stats`: Names of the statistics used (e.g., `"Gnp"`, `"U3p"`, `"lpz"`).
#' }
#' Persons are retained based on the selected criterion and only the
#' kept items from \code{misfit$scaled$kept} are preserved.
#'
#' @seealso [misfit_epmfd()], [scale_epmfd()]
#'
#' @examples
#' \dontrun{
#' # Assume 'mf' is the result of misfit_epmfd()
#'
#' # Remove anyone flagged by at least one statistic
#' cl1 <- clean_epmfd(mf, criterion = "union")
#' cl1$n_removed
#' scale_obj <- scale_epmfd(cl1$raw)
#'
#' # Remove only those flagged by all statistics
#' cl2 <- clean_epmfd(mf, criterion = "intersection")
#'
#' head(cl1$clean_data)
#' }
#'
#' @export
clean_epmfd <- function(misfit,
                        criterion = c("union", "intersection")) {

  stopifnot(inherits(misfit, "epmfd_misfit"))
  criterion <- match.arg(criterion)

  tbl   <- misfit$table
  stats <- misfit$stats

  ## ------------------------------------------------------------------
  ## 1  – Determine misfits
  ## ------------------------------------------------------------------
  fit_flag <- if (criterion == "union") {
    !tbl$misfit_any
  } else {                              # intersection
    rowSums(!tbl[stats]) == length(stats)
  }

  n_removed <- sum(!fit_flag)

  ## ------------------------------------------------------------------
  ## 2  – Clean data (only kept items)
  ## ------------------------------------------------------------------
  kept_items <- misfit$scaled$kept
  raw_orig   <- misfit$scaled$raw

  clean_data <- raw_orig$data[fit_flag, kept_items, drop = FALSE]
  clean_id   <- raw_orig$id[fit_flag]

    K <- raw_orig$K

  ## ------------------------------------------------------------------
  ## 3  – Create New epmfd_raw object
  ## ------------------------------------------------------------------
  raw_clean <- list(
    data = clean_data,
    id   = clean_id,
    K    = K
  )
  class(raw_clean) <- c("epmfd_raw", "list")

  ## ------------------------------------------------------------------
  ## 4  – epmfd_clean object
  ## ------------------------------------------------------------------
  out <- list(
    raw        = raw_clean,
    clean_data = clean_data,
    n_removed  = n_removed,
    criterion  = criterion,
    misfit     = misfit
  )
  class(out) <- c("epmfd_clean", "list")
  return(out)
}
