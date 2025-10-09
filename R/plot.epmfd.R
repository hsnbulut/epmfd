#' Plot methods for epmfd objects
#'
#' Quick visual summaries for three object classes:
#' \itemize{
#'   \item \strong{epmfd_scaled}: Item-level retention summary (Kept vs Removed)
#'         and a quality-statistic histogram (either discrimination \code{a} for
#'         \emph{mirt} or scalability \code{H_i} for \emph{mokken}).
#'   \item \strong{epmfd_misfit}: Bar plot of misfit counts per statistic and a
#'         global bar summarizing overall misfit ratio.
#'   \item \strong{epmfd_clean}: Bar plot comparing remaining vs removed persons.
#' }
#'
#' If the \pkg{patchwork} package is installed, paired plots are stacked
#' vertically and returned as a single \emph{patchwork} object; otherwise a
#' list of two \code{ggplot2} objects is returned.
#'
#' @section Dependencies:
#' These methods use \pkg{ggplot2}. For \code{epmfd_scaled} objects fitted with
#' \emph{mirt}, the method accesses model coefficients via \pkg{mirt} if that
#' package is installed (it is not required for \emph{mokken}). Stacking
#' multiple plots uses \pkg{patchwork} when available.
#'
#' @param x   An \code{epmfd_scaled}, \code{epmfd_misfit}, or \code{epmfd_clean}
#'            object.
#' @param ... Additional aesthetics or layers forwarded to the underlying
#'            \pkg{ggplot2} geoms (e.g., \code{alpha}, \code{linewidth}).
#'
#' @return A single \code{ggplot2} object, a \code{patchwork} object (if
#'         available), or a list of \code{ggplot2} objects—depending on the
#'         class and whether combined layout is possible.
#'
#' @seealso \code{\link{plot_misfit}} for 2D/3D scatter visualizations of
#' person-level misfit, and \code{\link{misfit_epmfd}} / \code{\link{clean_epmfd}}
#' for producing the inputs to these plots.
#'
#' @examplesIf FALSE
#' \donttest{
#' # Scaled object
#' p_scaled <- plot(scaled_obj)               # item retention + quality histogram
#'
#' # Misfit object
#' p_mf <- plot(misfit_obj)                   # per-statistic counts + overall ratio
#'
#' # Cleaned object
#' p_cl <- plot(clean_obj)                    # remaining vs removed persons
#'
#' # Add ggplot2 options through '...'
#' plot(misfit_obj, alpha = 0.8)
#' }
#'
#' @name plot_epmfd
#' @importFrom ggplot2 ggplot aes geom_col geom_text geom_histogram
#'   geom_vline labs theme_minimal
NULL
# --------------------------------------------------------------------------- #

#=========================== epmfd_scaled =====================================
#' @rdname plot_epmfd
#' @method plot epmfd_scaled
#' @export
plot.epmfd_scaled <- function(x, ...) {

  df_bar <- data.frame(
    Status = factor(c("Kept", "Removed"), levels = c("Kept", "Removed")),
    Count  = c(length(x$kept), length(x$removed))
  )

  p_bar <- ggplot2::ggplot(df_bar, ggplot2::aes(Status, Count, fill = Status)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::geom_text(ggplot2::aes(label = Count)) +
    ggplot2::labs(title = "Item retention summary",
                  x = NULL, y = "Number of items") +
    ggplot2::theme_minimal()

  if (x$method == "mirt") {
    a_vals <- mirt::coef(x$model, IRTpars = TRUE, simplify = TRUE)$items[, "a"]
    p_hist <- ggplot2::ggplot(data.frame(a = a_vals),
                              ggplot2::aes(a)) +
      ggplot2::geom_histogram(bins = 20, ...) +
      ggplot2::geom_vline(xintercept = x$a_thr, linetype = 2) +
      ggplot2::labs(title = "Discrimination (a) distribution",
                    x = "a", y = "Frequency") +
      ggplot2::theme_minimal()
  } else {
    p_hist <- ggplot2::ggplot() +
      ggplot2::geom_histogram(
        ggplot2::aes(x = x$Hi),
        bins = 20,
        ...
      ) +
      ggplot2::geom_vline(xintercept = x$H_thr, linetype = 2) +
      ggplot2::labs(
        title = "Scalability (H_i) distribution",
        x = "H_i", y = "Frequency"
      ) +
      ggplot2::theme_minimal()
  }

  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(p_bar / p_hist)
  } else {
    return(list(summary = p_bar, quality = p_hist))
  }
}

#=========================== epmfd_misfit =====================================
#' @rdname plot_epmfd
#' @method plot epmfd_misfit
#' @export
plot.epmfd_misfit <- function(x, ...) {

  stat_counts <- sapply(x$stats, function(s) sum(x$table[[s]]))
  df_stat  <- data.frame(Statistic = x$stats, Misfit = stat_counts)

  p_stat <- ggplot2::ggplot(df_stat,
                            ggplot2::aes(Statistic, Misfit, fill = Statistic)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::labs(title = "Misfit counts per statistic",
                  x = NULL, y = "Number of persons") +
    ggplot2::theme_minimal()

  ratio <- mean(x$table$misfit_any)
  df_ratio <- data.frame(Status = c("Misfit", "Fit"),
                         Count  = c(sum(x$table$misfit_any),
                                    sum(!x$table$misfit_any)))

  p_ratio <- ggplot2::ggplot(df_ratio,
                             ggplot2::aes(Status, Count, fill = Status)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::geom_text(ggplot2::aes(label = Count)) +
    ggplot2::labs(title = sprintf("Overall misfit ratio = %.1f%%",
                                  100 * ratio),
                  x = NULL, y = "Number of persons") +
    ggplot2::theme_minimal()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_stat / p_ratio
  } else {
    list(by_stat = p_stat, overall = p_ratio)
  }
}

#=========================== epmfd_clean ======================================
#' @rdname plot_epmfd
#' @method plot epmfd_clean
#' @export
plot.epmfd_clean <- function(x, ...) {

  df <- data.frame(Group = c("Remaining", "Removed"),
                   Count = c(nrow(x$clean_data), x$n_removed))

  ggplot2::ggplot(df, ggplot2::aes(Group, Count, fill = Group)) +
    ggplot2::geom_col(show.legend = FALSE, ...) +
    ggplot2::geom_text(ggplot2::aes(label = Count)) +
    ggplot2::labs(title = "Persons before vs after cleaning",
                  x = NULL, y = "Number of persons") +
    ggplot2::theme_minimal()
}
