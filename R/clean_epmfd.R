#' Remove misfitting persons from the data
#'
#' @param object    An `epmfd_misfit` object created by [misfit_epmfd()].
#' @param criterion Character. If `"intersection"`, a person is removed only
#'   when they are flagged as misfitting by **all** selected statistics;
#'   if `"union"`, a person is removed when flagged by **any** of the selected
#'   statistics.
#' @param keep_table Logical. If `FALSE`, drops the misfit table from the output
#'   to save memory.
#'
#' @return An object of class `epmfd_clean` containing:
#' \itemize{
#'   \item \code{clean_data}: the item matrix after removing persons
#'   \item \code{removed_id}: IDs of the removed persons
#'   \item \code{n_removed}:  number of removed persons
#'   \item \code{criterion}:  cleaning rule used
#'   \item \code{misfit}:     original `epmfd_misfit` object (optional)
#' }
#' @export
clean_epmfd <- function(object,
                        criterion  = c("intersection", "union"),
                        keep_table = TRUE) {

  if (!inherits(object, "epmfd_misfit"))
    stop("Input must be an 'epmfd_misfit' object.", call. = FALSE)

  criterion <- match.arg(criterion)
  tbl       <- object$table

  # --- build logical vector -------------------------------------------------
  remove_flag <- if (criterion == "union") {
    tbl$misfit_any
  } else {                              # intersection
    apply(tbl[object$stats], 1, all)
  }

  removed_id <- tbl$id[remove_flag]
  kept_rows  <- !object$scaled$raw$id %in% removed_id
  clean_dat  <- object$scaled$raw$data[kept_rows, , drop = FALSE]

  out <- list(
    clean_data = clean_dat,
    removed_id = removed_id,
    n_removed  = length(removed_id),
    criterion  = criterion,
    misfit     = if (keep_table) object else NULL
  )
  class(out) <- c("epmfd_clean", class(out))
  out
}

# Silence R CMD check about NSE variable 'id'
if (getRversion() >= "2.15.1") {
  utils::globalVariables("id")
}
