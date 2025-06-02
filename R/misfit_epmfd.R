#' Compute person-fit statistics (polytomous)
#'
#' @param object An \code{epmfd_scaled} object.
#' @param stats  Character vector: any of \code{"auto"}, \code{"lpz"},
#'   \code{"Gp"} , \code{"U3p"}.
#' @param alpha  Significance level (default 0.05).
#' @return An \code{epmfd_misfit} object.
#' @export
misfit_epmfd <- function(object,
                         stats = c("auto", "lpz", "Gp", "U3p"),
                         alpha = 0.05) {

  if (!inherits(object, "epmfd_scaled"))
    stop("Input must be an 'epmfd_scaled' object.")

  stats <- match.arg(stats, several.ok = TRUE)
  if ("auto" %in% stats) stats <- c("lpz", "Gp","U3p")

  ## veri --------------------------------------------------------------------
  X <- object$raw$data |>
    dplyr::select(dplyr::all_of(object$kept))
  X[] <- lapply(X, as.integer)     # faktör → sayı
  X   <- as.matrix(X) - 1          # PerFit 0:(K-1)

  K     <- object$raw$K
  crit  <- stats::qnorm(1 - alpha)

  res    <- list()      # mantıksal bayrak
  scores <- list()      # ham skor

  pf_ns <- asNamespace("PerFit")

  ## lpz ---------------------------------------------------------------------
  if ("lpz" %in% stats) {
    converged <- tryCatch({
      mirt::converged(object$model)
    }, error = function(e) FALSE)

    if (!converged) {
      warning("GRM model did not converge; skipping lpz statistic.")
      stats <- setdiff(stats, "lpz")
    } else {
      scores$lpz <- PerFit::lzpoly(X, Ncat = K)$PFscores
      res$lpz    <- scores$lpz > crit
    }
  }
  ## Gp ----------------------------------------------------------------------
  if ("Gp" %in% stats) {
    scores$Gp <- PerFit::Gpoly(X, Ncat = K)$PFscores
    res$Gp    <- scores$Gp > crit
  }

  ## U3p ---------------------------------------------------------------------
  if ("U3p" %in% stats) {
    scores$U3p <- PerFit::U3poly(X, Ncat = K)$PFscores
    res$U3p    <- scores$U3p > crit
  }

  ## tablo -------------------------------------------------------------------
  flag_tbl <- as.data.frame(res, check.names = FALSE)
  names(flag_tbl) <- names(res)     # << zaten vardı

  flag_tbl$id <- object$raw$id
  cols <- intersect(names(res), stats)
  flag_tbl$misfit_any <- if (length(cols))
    apply(flag_tbl[cols], 1, any) else FALSE

  ## ----- DÜZELTME burası ---------------------------------------------------
  scores_df <- as.data.frame(scores, check.names = FALSE)
  names(scores_df) <- names(scores)           #  <<— ÖNEMLİ
  ## ------------------------------------------------------------------------

  out <- list(
    scaled = object,
    stats  = stats,
    alpha  = alpha,
    table  = flag_tbl,
    scores = scores_df                       # güncel adlarıyla
  )
  class(out) <- c("epmfd_misfit", "list")
  return(out)
}
