#' Generic plotting for *epmfd* objects
#'
#' Produces an exploratory plot that best fits the supplied object type.
#'
#' \strong{epmfd_scaled}  – Bar plot of kept vs. removed items and,
#' where possible, a histogram of the item-quality statistic
#'   (\\eqn{a} for MIRT, \\eqn{H_i} for Mokken) with its cut-off line.
#' \strong{epmfd_misfit}  – Bar plot of misfit counts per statistic and
#' overall misfit ratio.
#' \strong{epmfd_clean}   – Bar plot showing persons removed vs. remaining.
#'
#' If the \pkg{patchwork} package is available, paired plots are combined
#' vertically; otherwise the function returns a list of individual
#' \code{ggplot} objects.
#'
#' @param x   An object of class \code{epmfd_scaled}, \code{epmfd_misfit},
#'            or \code{epmfd_clean}.
#' @param ... Additional layers / aesthetics passed to \code{ggplot2} geoms.
#' @return    A \code{ggplot} object, or a \code{patchwork} object when
#'            the package is installed, or a list of plots otherwise.
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text geom_histogram
#'   geom_vline geom_hline labs theme_minimal
#' @export
plot.epmfd.epmfd_scaled  <- function(x, ...) { ... }
plot.epmfd.epmfd_misfit  <- function(x, ...) { ... }
plot.epmfd.epmfd_clean   <- function(x, ...) { ... }

# ----------------------------------------------------------------------
# 1) epmfd_scaled -------------------------------------------------------
# ----------------------------------------------------------------------
#' @export
plot.epmfd.epmfd_scaled <- function(x, ...) {

  # --- bar: kept vs removed -------------------------------------------
  df_bar <- data.frame(
    Status = factor(c("Kept", "Removed"), levels = c("Kept", "Removed")),
    Count  = c(length(x$kept), length(x$removed))
  )

  p_bar <- ggplot2::ggplot(df_bar, ggplot2::aes(Status, Count, fill = Status)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.2) +
    ggplot2::labs(title = "Item retention summary",
                  x = NULL, y = "Number of items") +
    ggplot2::theme_minimal()

  # --- histogram of a or Hi -------------------------------------------
  if (x$method == "mirt") {
    a_vals <- mirt::coef(x$model, IRTpars = TRUE, simplify = TRUE)$items[ , "a"]
    p_hist <- ggplot2::ggplot(data.frame(a = a_vals),
                              ggplot2::aes(a)) +
      ggplot2::geom_histogram(bins = 20, ...) +
      ggplot2::geom_vline(xintercept = x$a_thr, linetype = 2) +
      ggplot2::labs(title = "Discrimination (a) distribution",
                    x = "a", y = "Frequency") +
      ggplot2::theme_minimal()
  } else {  # mokken
    p_hist <- ggplot2::ggplot(data.frame(Hi = x$Hi),
                              ggplot2::aes(Hi)) +
      ggplot2::geom_histogram(bins = 20, ...) +
      ggplot2::geom_vline(xintercept = x$H_thr, linetype = 2) +
      ggplot2::labs(title = "Scalability (H_i) distribution",
                    x = "H_i", y = "Frequency") +
      ggplot2::theme_minimal()
  }

  # --- combine or return list -----------------------------------------
  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(p_bar / p_hist)   # patchwork syntax
  } else {
    return(list(summary = p_bar, quality = p_hist))
  }
}

# ----------------------------------------------------------------------
# 2) epmfd_misfit -------------------------------------------------------
# ----------------------------------------------------------------------
#' @export
plot.epmfd.epmfd_misfit <- function(x, ...) {

  # bar per statistic
  stat_counts <- sapply(x$stats, function(s) sum(x$table[[s]]))
  df_stat <- data.frame(Statistic = x$stats,
                        Misfit    = stat_counts)

  p_stat <- ggplot2::ggplot(df_stat,
                            ggplot2::aes(Statistic, Misfit, fill = Statistic)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::labs(title = "Misfit counts per statistic",
                  x = NULL, y = "Number of persons") +
    ggplot2::theme_minimal()

  # overall ratio
  ratio <- mean(x$table$misfit_any)
  df_ratio <- data.frame(Status = c("Misfit", "Fit"),
                         Count  = c(sum(x$table$misfit_any),
                                    sum(!x$table$misfit_any)))

  p_ratio <- ggplot2::ggplot(df_ratio,
                             ggplot2::aes(Status, Count, fill = Status)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.2) +
    ggplot2::labs(
      title = sprintf("Overall misfit ratio = %.1f%%", 100 * ratio),
      x = NULL, y = "Number of persons") +
    ggplot2::theme_minimal()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_stat / p_ratio
  } else {
    list(by_stat = p_stat, overall = p_ratio)
  }
}

# ----------------------------------------------------------------------
# 3) epmfd_clean --------------------------------------------------------
# ----------------------------------------------------------------------
#' @export
plot.epmfd.epmfd_clean <- function(x, ...) {

  df <- data.frame(Group = c("Remaining", "Removed"),
                   Count = c(nrow(x$clean_data), x$n_removed))

  ggplot2::ggplot(df, ggplot2::aes(Group, Count, fill = Group)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.2) +
    ggplot2::labs(title = "Persons before vs after cleaning",
                  x = NULL, y = "Number of persons") +
    ggplot2::theme_minimal()
}
