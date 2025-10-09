#' Remove misfitting persons from an epmfd_misfit object
#'
#' `clean_epmfd()` removes individuals flagged as misfitting according to
#' a chosen decision rule and returns a cleaned dataset that can be passed
#' directly to [scale_epmfd()].
#'
#' @section Criterion:
#' - `"union"` (default): A person is removed if **at least one** statistic
#'   (e.g., Gnp, U3p, lpz) flags them as misfitting. This is stricter.
#' - `"intersection"`: A person is removed **only if all** statistics flag them
#'   as misfitting. This is more lenient.
#'
#' @param misfit An `epmfd_misfit` object returned by [misfit_epmfd()].
#' @param criterion Character string, either `"union"` (default) or `"intersection"`.
#'
#' @return An `epmfd_clean` list with:
#' \itemize{
#'   \item `raw`: An `epmfd_raw` object containing only the retained persons and
#'         items, directly usable in [scale_epmfd()].
#'   \item `clean_data`: The cleaned raw data frame (persons × kept items).
#'   \item `n_removed`: Number of persons removed.
#'   \item `criterion`: The applied decision rule.
#'   \item `misfit`: The original `epmfd_misfit` object (as provided).
#' }
#'
#' @details
#' The function uses logical misfit indicators stored in \code{misfit$table},
#' including:
#' \itemize{
#'   \item `misfit_any`: \code{TRUE} if at least one statistic flagged the person.
#'   \item Statistic-specific columns (e.g., `Gnp`, `U3p`, `lpz`) indicating
#'         per-statistic misfit decisions.
#' }
#' The set of statistics actually considered is taken from \code{misfit$stats}.
#' Under the `"intersection"` rule, a person is removed only if **all** of those
#' statistics are \code{TRUE}. Internally, \code{rowSums(..., na.rm = TRUE)} is
#' used so that \code{NA} values do not force removal (i.e., \code{NA} behaves
#' as “not flagged” in the intersection count).
#'
#' Only items listed in \code{misfit$scaled$kept} are retained in the output.
#' Person identifiers from the original raw object are preserved for the kept rows.
#'
#' @seealso [misfit_epmfd()], [scale_epmfd()]
#'
#' @examplesIf FALSE
#' \donttest{
#'   # Example usage (not run during checks):
#'   # Assume 'mf' is created by:
#'   #   raw <- as_epmfd_raw(your_data)
#'   #   sc  <- scale_epmfd(raw, method = "mokken")
#'   #   mf  <- misfit_epmfd(sc, stats = "auto")
#'   #
#'   # Remove anyone flagged by at least one statistic (union):
#'   # cl1 <- clean_epmfd(mf, criterion = "union")
#'   # head(cl1$clean_data)
#'   #
#'   # Or require all selected stats (intersection):
#'   # cl2 <- clean_epmfd(mf, criterion = "intersection")
#' }
#' @export
clean_epmfd <- function(misfit,
                        criterion = c("union", "intersection")) {

  stopifnot(inherits(misfit, "epmfd_misfit"))
  criterion <- match.arg(criterion)

  tbl   <- misfit$table
  stats <- misfit$stats

  # Safety check: ensure all requested statistic columns exist
  if (!all(stats %in% colnames(tbl))) {
    stop("Some requested statistics are not present in misfit$table: ",
         paste(setdiff(stats, colnames(tbl)), collapse = ", "))
  }

  # Extract relevant statistic columns
  stat_mat <- as.data.frame(tbl[, stats, drop = FALSE])

  ## ---------------------------------------------------------------
  ## 1 – Determine which persons are retained
  ## ---------------------------------------------------------------
  if (criterion == "union") {
    # UNION: Remove person if at least one statistic flagged them
    # => Keep if misfit_any == FALSE
    fit_flag <- !tbl$misfit_any
  } else {
    # INTERSECTION: Remove person only if all statistics flagged them
    # => Keep if not all are TRUE
    all_flagged <- rowSums(stat_mat, na.rm = TRUE) == length(stats)
    fit_flag <- !all_flagged
  }

  n_removed <- sum(!fit_flag, na.rm = TRUE)

  ## ---------------------------------------------------------------
  ## 2 – Clean data (only retained persons and kept items)
  ## ---------------------------------------------------------------
  kept_items <- misfit$scaled$kept
  raw_orig   <- misfit$scaled$raw

  clean_data <- raw_orig$data[fit_flag, kept_items, drop = FALSE]
  clean_id   <- raw_orig$id[fit_flag]
  K          <- raw_orig$K

  ## ---------------------------------------------------------------
  ## 3 – Create new epmfd_raw object
  ## ---------------------------------------------------------------
  raw_clean <- list(
    data = clean_data,
    id   = clean_id,
    K    = K
  )
  class(raw_clean) <- c("epmfd_raw", "list")

  ## ---------------------------------------------------------------
  ## 4 – Construct the final epmfd_clean object
  ## ---------------------------------------------------------------
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
