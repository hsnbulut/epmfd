#' Print method for `epmfd_misfit` objects
#'
#' @param x   An object of class `epmfd_misfit`.
#' @param ... Further arguments passed to [utils::head()].
#' @export
print.epmfd_misfit <- function(x, ...) {

  cat("========  epmfd_misfit  ========\n")
  cat("Statistics used :", paste(x$stats, collapse = ", "), "\n")
  cat("Alpha           :", x$alpha, "\n")
  cat("Total persons   :", nrow(x$table), "\n")
  cat("Any misfit (%)  :", round(100 * mean(x$table$misfit_any), 1), "\n\n")

  utils::head(x$table, ...) |>
    print(row.names = FALSE)
  invisible(x)
}

#' Summary method for `epmfd_clean` objects
#'
#' @param object An object of class `epmfd_clean`.
#' @param ...    Further arguments (ignored).
#' @return       Invisibly returns a named list with summary numbers.
#' @export
summary.epmfd_clean <- function(object, ...) {

  total <- nrow(object$clean_data) + object$n_removed
  ratio <- if (total > 0) object$n_removed / total else 0

  cat("========  epmfd_clean  ========\n")
  cat("Cleaning rule  :", object$criterion, "\n")
  cat("Total persons  :", total, "\n")
  cat("Removed        :", object$n_removed,
      sprintf(" (%.1f%%)", 100 * ratio), "\n")
  cat("Remaining      :", nrow(object$clean_data), "\n")

  invisible(
    list(
      total      = total,
      removed    = object$n_removed,
      remaining  = nrow(object$clean_data),
      criterion  = object$criterion
    )
  )
}
