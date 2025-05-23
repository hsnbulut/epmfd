#' Scatter plot of two person-fit statistics with cut-off lines
#'
#' @description
#' Displays respondents in the plane defined by two misfit statistics.
#' Points that exceed both cut-off values (upper-right quadrant) are the
#' most problematic persons.
#'
#' @param object An `epmfd_misfit` object, or an `epmfd_clean` object that
#'   still contains the original misfit results.
#'
#' @param x_stat Character, **one of** `"lpz"`, `"Gp"`, `"Gpn"`, `"U3p"`.
#'   Statistic to be shown on the *x*-axis.
#'   Default is `"lpz"`.
#'
#' @param y_stat Character, **one of** `"lpz"`, `"Gp"`, `"Gpn"`, `"U3p"`,
#'   **different from** `x_stat`.  Statistic to be shown on the *y*-axis.
#'   Default is `"Gp"`.
#'
#' @param alpha  Numeric in (0, 1).  If `x_cut` or `y_cut` is `NULL`,
#'   cut-off values are set to `qnorm(1 - alpha)`.
#'
#' @param x_cut,y_cut Numeric cut-off values.  When supplied they override
#'   the `alpha`-based defaults.
#'
#' @param ... Additional aesthetics or layers passed to
#'   [ggplot2::geom_point()].
#'
#' @return A `ggplot` object that can be further customised.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_vline labs
#'   theme_minimal
#' @importFrom stats qnorm
#' @export

plot_misfit_scatter <- function(object,
                                x_stat = "lpz",
                                y_stat = "Gp",
                                alpha  = 0.05,
                                x_cut  = NULL,
                                y_cut  = NULL,
                                ...) {

  ## --- retrieve misfit scores -------------------------------------------
  if (inherits(object, "epmfd_clean"))
    object <- object$misfit
  if (!inherits(object, "epmfd_misfit"))
    stop("`object` must be of class 'epmfd_misfit' or 'epmfd_clean'.")

  sc <- object$scores
  if (!(x_stat %in% names(sc) && y_stat %in% names(sc)))
    stop("Statistics not found in stored scores.")

  df <- data.frame(
    id = object$table$id,
    x  = sc[[x_stat]],
    y  = sc[[y_stat]]
  )

  ## --- cut-off values ----------------------------------------------------
  if (is.null(x_cut)) x_cut <- stats::qnorm(1 - alpha)
  if (is.null(y_cut)) y_cut <- stats::qnorm(1 - alpha)

  ## --- build plot --------------------------------------------------------
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(alpha = .8, ...) +
    ggplot2::geom_hline(yintercept = y_cut, linetype = 2) +
    ggplot2::geom_vline(xintercept = x_cut, linetype = 2) +
    ggplot2::labs(
      title = sprintf("%s vs %s (alpha = %.2f)", x_stat, y_stat, alpha),
      subtitle = sprintf("Cut-offs: %.2f | %.2f", x_cut, y_cut),
      x = x_stat, y = y_stat
    )+
    ggplot2::theme_minimal()
}

## Silence R CMD check for NSE vars
if (getRversion() >= "2.15.1")
  utils::globalVariables(c("x", "y"))
