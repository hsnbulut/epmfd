#' Plot person misfit in 2D/3D using stored thresholds
#'
#' `plot_misfit()` visualizes person-level misfit statistics stored in an
#' `epmfd_misfit` object. It supports:
#' \itemize{
#'   \item \strong{2D}: scatter plots for all pairwise combinations of the
#'         selected statistics (or a single 2D plot if exactly two are given).
#'         Points are coloured by the joint exceedance logic (see `any`).
#'         Axis titles are the statistic names; the main title shows the cut-offs
#'         in parentheses, and dashed lines mark the cut-offs per axis.
#'   \item \strong{3D}: an interactive scatter using \pkg{plotly} if three
#'         statistics are supplied; optionally adds three semi-transparent planes
#'         at the x/y/z cut-offs when `planes = TRUE`.
#' }
#'
#' @param object   An `epmfd_misfit` or `epmfd_clean` object. If `epmfd_clean`
#'                 is supplied, its `$misfit` component is used.
#' @param stats    Character vector of length 2 or 3 naming statistics found in
#'                 `object$scores` (e.g., `c("Gnp","U3p","lpz")`). If `NULL`,
#'                 the first up to three available statistics are used.
#' @param threeD   Logical. If `TRUE` and three statistics are available, a 3D
#'                 \pkg{plotly} plot is drawn; otherwise the function falls back
#'                 to 2D and emits a warning.
#' @param any      Logical. Colouring rule:
#'                 \itemize{
#'                   \item `FALSE` (default): only two classes - \emph{all}
#'                         cut-offs exceeded (red) vs \emph{none} exceeded (blue).
#'                   \item `TRUE`: adds an intermediate class (orange) for
#'                         \emph{partial} exceedance (in 2D: exactly one; in 3D:
#'                         one or two).
#'                 }
#' @param planes   Logical (3D only). If `TRUE`, draw three semi-transparent
#'                 planes at the x, y, and z cut-off values; if `FALSE`, no
#'                 planes are shown. Ignored for 2D plots.
#' @param label_ids Logical. If `TRUE`, label points by `id` in 2D plots
#'                  (uses \pkg{ggrepel} when available).
#' @param ...      Additional aesthetics passed to
#'                 \code{ggplot2::geom_point()} (2D) or
#'                 \code{plotly::add_markers()} (3D), such as `alpha`, `size`,
#'                 etc.
#'
#' @importFrom utils head
#' @importFrom stats setNames
#' @details
#' \strong{Cut-off logic.} For each selected statistic, a person is deemed to
#' exceed if its score is greater than the cut-off for upper-tailed statistics
#' or less than the cut-off for lower-tailed statistics. In 2D, dashed vertical
#' and horizontal lines indicate the cut-offs; the plot title shows
#' \code{"Y (cutY) vs X (cutX)"} with formatted values. In 3D, axis titles
#' include the cut-off values in parentheses, and (optionally) three grey planes
#' make the cut-offs explicit.
#'
#' \strong{Returned value.} With two statistics, a single `ggplot` is returned;
#' with three statistics and `threeD = FALSE`, a named list of `ggplot`s is
#' returned for all 2D pairs. With `threeD = TRUE` and three statistics, a
#' `plotly` object is returned.
#'
#' \strong{Dependencies.} This function uses \pkg{ggplot2} for 2D plots and,
#' for 3D, \pkg{plotly} (required only when `threeD = TRUE`). Optional labels in
#' 2D use \pkg{ggrepel} when installed.
#'
#' @return
#' A `ggplot` object (2D), a named list of `ggplot`s (all 2D pairs), or a
#' `plotly` object (3D), depending on `stats` and `threeD`.
#'
#' @seealso [misfit_epmfd()] for computing statistics and thresholds;
#'          [clean_epmfd()] for removing misfitting persons.
#'
#' @examplesIf FALSE
#' \donttest{
#' # Suppose 'mf' is an epmfd_misfit with scores Gnp, U3p, lpz
#'
#' # 2D: single plot
#' plot_misfit(mf, stats = c("Gnp","U3p"), any = TRUE)
#'
#' # 2D: all pairwise plots
#' plot_misfit(mf, stats = c("Gnp","U3p","lpz"))
#'
#' # 3D: with cut-off planes
#' plot_misfit(mf, stats = c("Gnp","U3p","lpz"), threeD = TRUE, planes = TRUE)
#'
#' # 3D: points only (no planes)
#' plot_misfit(mf, stats = c("Gnp","U3p","lpz"), threeD = TRUE, planes = FALSE)
#' }
#'
#' @export
plot_misfit <- function(object,
                        stats     = NULL,
                        threeD    = FALSE,
                        any       = FALSE,
                        planes    = TRUE,
                        label_ids = FALSE,
                        ...) {

  # ---------- normalize / checks ----------
  if (inherits(object, "epmfd_clean")) object <- object$misfit
  stopifnot(inherits(object, "epmfd_misfit"))

  scores <- object$scores
  avail  <- names(scores)
  if (length(avail) < 2L) stop("At least two statistics must be available in `object$scores`.")

  if (is.null(stats)) stats <- head(avail, 3L)
  if (!all(stats %in% avail))
    stop("Missing stats in object$scores: ", paste(setdiff(stats, avail), collapse = ", "))
  stats <- unique(stats)
  if (length(stats) < 2L) stop("`stats` must have length >= 2.")
  if (length(stats) > 3L) { warning("Using only the first 3 statistics."); stats <- stats[1:3] }

  # ---------- thresholds ----------
  get_thr <- function(s){
    th <- object$thresholds[[s]]
    if (is.null(th) || is.null(th$value) || is.null(th$tail))
      stop(sprintf("Missing threshold for '%s' (need $value & $tail).", s))
    list(value = as.numeric(th$value), tail = tolower(as.character(th$tail)))
  }
  thr <- setNames(lapply(stats, get_thr), stats)

  # ---------- data ----------
  df <- data.frame(id = object$table$id, stringsAsFactors = FALSE)
  for (s in stats) df[[s]] <- as.numeric(scores[[s]])

  # ---------- exceed logic ----------
  exceed_matrix <- sapply(stats, function(s){
    if (thr[[s]]$tail == "upper") df[[s]] > thr[[s]]$value else df[[s]] < thr[[s]]$value
  })
  exceed_matrix <- as.matrix(exceed_matrix)
  exceed_count  <- rowSums(exceed_matrix, na.rm = TRUE)
  d <- ncol(exceed_matrix)  # 2 or 3

  # ---------- colors & labels ----------
  col_map <- c(blue="#2c7bb6", orange="#fdae61", red="#d73027")
  df$.colour <- factor(
    if (!any) ifelse(exceed_count == d, "red", "blue")
    else ifelse(exceed_count == 0L, "blue", ifelse(exceed_count == d, "red", "orange")),
    levels = c("blue","orange","red")
  )
  legend_labels <- if (!any)
    c(blue="Fit (none exceed)", red="Misfit (all exceed)")
  else
    c(blue="Fit (none exceed)", orange=if (d==2L) "Partial exceed (1 dim)" else "Partial exceed (1-2 dims)",
      red="Misfit (all exceed)")

  # =================== 3D ===================
  if (threeD) {
    if (d < 3L) {
      warning("`threeD = TRUE` but only two stats available; falling back to 2D.")
    } else {
      if (!requireNamespace("plotly", quietly = TRUE))
        stop("`plotly` is required for 3D plotting. Please install.packages('plotly').")

      s1 <- stats[1]; s2 <- stats[2]; s3 <- stats[3]
      x_title <- sprintf("%s (%s)", s1, format(thr[[s1]]$value, digits = 4))
      y_title <- sprintf("%s (%s)", s2, format(thr[[s2]]$value, digits = 4))
      z_title <- sprintf("%s (%s)", s3, format(thr[[s3]]$value, digits = 4))

      # axis ranges include cutoffs so planes are always visible
      xr <- range(c(df[[s1]], thr[[s1]]$value), finite = TRUE)
      yr <- range(c(df[[s2]], thr[[s2]]$value), finite = TRUE)
      zr <- range(c(df[[s3]], thr[[s3]]$value), finite = TRUE)

      df$cls <- as.character(df$.colour)
      present_levels <- intersect(names(col_map), unique(df$cls))

      p3 <- plotly::plot_ly()
      for (lv in present_levels) {
        dsub <- df[df$cls == lv, , drop = FALSE]
        p3 <- plotly::add_markers(
          p3, data = dsub,
          x = ~ get(s1), y = ~ get(s2), z = ~ get(s3),
          mode = "markers",
          marker = list(size = 4, color = unname(col_map[lv])),
          name = legend_labels[[lv]], showlegend = TRUE, ...
        )
      }

      # ---- plane helper (mesh3d: two triangles) ----
      add_plane_mesh <- function(p, axis = c("x","y","z"), val, xr, yr, zr,
                                 color = "#a9a9a9", opacity = 0.25) {
        axis <- match.arg(axis)
        if (axis == "x") {
          x <- rep(val, 4)
          y <- c(yr[1], yr[2], yr[2], yr[1])
          z <- c(zr[1], zr[1], zr[2], zr[2])
        } else if (axis == "y") {
          x <- c(xr[1], xr[2], xr[2], xr[1])
          y <- rep(val, 4)
          z <- c(zr[1], zr[1], zr[2], zr[2])
        } else { # z
          x <- c(xr[1], xr[2], xr[2], xr[1])
          y <- c(yr[1], yr[1], yr[2], yr[2])
          z <- rep(val, 4)
        }
        plotly::add_trace(
          p,
          type = "mesh3d",
          x = x, y = y, z = z,
          i = c(0, 0), j = c(1, 2), k = c(2, 3),  # two triangles
          color = color, opacity = opacity,
          hoverinfo = "skip", showscale = FALSE, name = "cutoff"
        )
      }

      if (planes) {
        p3 <- add_plane_mesh(p3, "x", thr[[s1]]$value, xr, yr, zr)
        p3 <- add_plane_mesh(p3, "y", thr[[s2]]$value, xr, yr, zr)
        p3 <- add_plane_mesh(p3, "z", thr[[s3]]$value, xr, yr, zr)  # lpz plane
      }

      p3 <- plotly::layout(
        p3,
        title = list(text = "3D Misfit Scatter", y = 0.92),
        scene = list(
          xaxis = list(title = x_title, range = xr),
          yaxis = list(title = y_title, range = yr),
          zaxis = list(title = z_title, range = zr),
          aspectmode = "cube"
        ),
        legend = list(title = list(text = if (any) "Exceed pattern" else "Classification"))
      )
      return(p3)
    }
  }

  # =================== 2D (unchanged) ===================
  make_2d_plot <- function(a, b) {
    pair_exceed <- exceed_matrix[, c(a, b), drop = FALSE]
    pair_count  <- rowSums(pair_exceed, na.rm = TRUE)
    pair_colour <- if (!any) ifelse(pair_count == 2L, "red", "blue")
    else ifelse(pair_count == 0L, "blue", ifelse(pair_count == 2L, "red", "orange"))
    df$.pair_colour <- factor(pair_colour, levels = c("blue","orange","red"))

    # Title: "Y (cutoffY) vs X (cutoffX)"; axes: just names
    main_title <- sprintf("%s (%s) vs %s (%s)",
                          b, format(thr[[b]]$value, digits = 4),
                          a, format(thr[[a]]$value, digits = 4))

    present <- levels(droplevels(df$.pair_colour))
    values  <- col_map[present]; breaks <- present
    labels  <- (if (!any) c(blue="Fit (none exceed)", red="Misfit (all exceed)")
                else c(blue="Fit (none exceed)", orange="Partial exceed (1 dim)", red="Misfit (all exceed)"))[present]

    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = a, y = b)) +
      ggplot2::geom_point(ggplot2::aes_string(colour = ".pair_colour"), size = 2.8, ...) +
      ggplot2::scale_colour_manual(values = values, breaks = breaks, labels = labels,
                                   name = ifelse(any, "Exceed pattern", "Classification")) +
      ggplot2::geom_vline(xintercept = thr[[a]]$value, linetype = 2) +
      ggplot2::geom_hline(yintercept = thr[[b]]$value, linetype = 2) +
      ggplot2::labs(title = main_title, x = a, y = b) +
      ggplot2::theme_minimal()

    if (label_ids) {
      if (requireNamespace("ggrepel", quietly = TRUE))
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = id), size = 3, min.segment.length = 0)
      else
        p <- p + ggplot2::geom_text(ggplot2::aes(label = id), vjust = -0.6, size = 3)
    }
    p
  }

  if (length(stats) == 2L) {
    return(make_2d_plot(stats[1], stats[2]))
  } else {
    nm <- c(
      sprintf("%s_vs_%s", stats[2], stats[1]),
      sprintf("%s_vs_%s", stats[3], stats[1]),
      sprintf("%s_vs_%s", stats[3], stats[2])
    )
    return(stats::setNames(
      list(
        make_2d_plot(stats[1], stats[2]),
        make_2d_plot(stats[1], stats[3]),
        make_2d_plot(stats[2], stats[3])
      ), nm))
  }
}
