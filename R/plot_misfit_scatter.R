#' Scatter / bubble plot for epmfd_misfit
#'
#' @inheritParams plot_misfit_scatter
#' @param x_stat,y_stat,z_stat Character names of statistics in
#'   \code{misfit$scores}.  If all three are \code{NULL}, the first three
#'   available statistics are chosen automatically.
#'
#' @export
plot_misfit_scatter <- function(object,
                                x_stat = NULL,
                                y_stat = NULL,
                                z_stat = NULL,
                                bubble = TRUE,
                                alpha  = 0.05,
                                x_cut  = NULL,
                                y_cut  = NULL,
                                z_cut  = NULL,
                                label_ids = FALSE,
                                ...) {

  ## 1 ─ object & scores ------------------------------------------------------
  if (inherits(object, "epmfd_clean"))
    object <- object$misfit
  stopifnot(inherits(object, "epmfd_misfit"))

  sc <- object$scores
  avail <- names(sc)

  ## 2 ─ otomatik stat seçimi -------------------------------------------------
  if (is.null(x_stat) && is.null(y_stat) && is.null(z_stat)) {
    if (length(avail) < 2)
      stop("At least two statistics required.")
    x_stat <- avail[1]
    y_stat <- avail[2]
    if (length(avail) >= 3) z_stat <- avail[3]
    bubble <- !is.null(z_stat)   # otomatik bubble
  }

  needed <- c(x_stat, y_stat, z_stat)
  needed <- needed[!is.null(needed)]
  if (!all(needed %in% avail))
    stop("Requested statistics not found in stored scores.")

  ## 3 ─ Data frame -----------------------------------------------------------
  df <- data.frame(
    id = object$table$id,
    x  = sc[[x_stat]],
    y  = sc[[y_stat]]
  )
  if (!is.null(z_stat))
    df$z <- sc[[z_stat]]

  ## 4 ─ Cut-offs -------------------------------------------------------------
  if (is.null(x_cut)) x_cut <- stats::qnorm(1 - alpha)
  if (is.null(y_cut)) y_cut <- stats::qnorm(1 - alpha)
  if (!is.null(z_stat) && is.null(z_cut))
    z_cut <- stats::qnorm(1 - alpha)

  ## 5 ─ Colour & size vars ---------------------------------------------------
  if (!is.null(z_stat)) {
    df$colour_flag <- ifelse(df$z > z_cut, "misfit", "fit")
    colours <- c(fit = "#2c7bb6", misfit = "#d73027")
  }

  ## 6 ─ ggplot ---------------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y))

  if (!is.null(z_stat) && bubble) {
    # Balon grafiği  (boyut = |z|, renk = fit/misfit)
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(size = abs(z), colour = colour_flag), ...) +
      ggplot2::scale_colour_manual(
        values = colours,
        name   = sprintf("%s (cut-off: %.2f)", z_stat, z_cut),
        labels = c(fit = paste0("≤ ", format(z_cut, digits = 3)),
                   misfit = paste0("> ", format(z_cut, digits = 3)))) +
      ggplot2::scale_size(guide = "none")          # boyut legend’i gizle

  } else if (!is.null(z_stat)) {
    # Renkli scatter (sabit nokta boyutu)
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(colour = colour_flag), size = 3, ...) +
      ggplot2::scale_colour_manual(
        values = colours,
        name   = sprintf("%s (cut-off: %.2f)", z_stat, z_cut),
        labels = c(fit = paste0("≤ ", format(z_cut, digits = 3)),
                   misfit = paste0("> ", format(z_cut, digits = 3))))
  } else {
    # Klasik scatter (tek renk)
    p <- p + ggplot2::geom_point(size = 3, ...)
  }

  p <- p +
    ggplot2::geom_hline(yintercept = y_cut,  linetype = 2) +
    ggplot2::geom_vline(xintercept = x_cut, linetype = 2) +
    ggplot2::labs(
      title    = sprintf("%s vs %s (alpha = %.2f)", y_stat, x_stat, alpha),
      subtitle = sprintf("Cut-offs: %.2f | %.2f", x_cut, y_cut),
      x = x_stat, y = y_stat) +
    ggplot2::theme_minimal()



  ## 7 ─ ID labels ------------------------------------------------------------
  if (label_ids) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = id), size = 3, min.segment.length = 0)
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = id), vjust = -0.6, size = 3)
    }
  }

  return(p)
}
