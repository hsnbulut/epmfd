#' Compute Person Misfit Statistics
#' @param object An `epmfd_scaled`.
#' @param stats "auto", "lpz", "Gp", "Gpn", "U3p".
#' @param alpha Significance level (0.05).
#' @return An `epmfd_misfit` object with misfit flags.
#' @export
misfit_epmfd <- function(object,
                         stats = c("auto", "lpz", "Gp", "Gpn", "U3p"),
                         alpha = 0.05) {

  if (!inherits(object, "epmfd_scaled"))
    stop("Input must be an 'epmfd_scaled' object.")

  stats <- match.arg(stats, several.ok = TRUE)
  if ("auto" %in% stats) stats <- c("lpz", "Gp", "Gpn", "U3p")

  X <- object$raw$data |>
    dplyr::select(dplyr::all_of(object$kept))

  X[] <- lapply(X, as.integer)
  X <- as.matrix(X)

  K <- object$raw$K

  # PerFit expects numeric matrix with 0:(K-1)
  X <- X - 1
  crit <- stats::qnorm(1 - alpha)

  res <- list()
  # ----- hesaplamalar -----
  if ("lpz" %in% stats)
    res$lpz  <- PerFit::lzpoly(X, K = K)$PFscores > crit

  if ("Gp"   %in% stats)
    res$Gp   <- PerFit::Gpoly(X, K = K)$PFscores  > crit

  if ("Gpn" %in% stats) {
    Gnorm_fun <- get("Gnorm", envir = asNamespace("PerFit"))
    res$Gpn   <- Gnorm_fun(X, K = K)$PFscores > crit
  }

  if ("U3p"  %in% stats)
    res$U3p  <- PerFit::U3poly(X, K = K)$PFscores > crit


  flag_tbl <- as.data.frame(res)
  flag_tbl$id <- object$raw$id
  flag_tbl$misfit_any <- apply(flag_tbl[stats], 1, any)

  structure(
    list(scaled = object,
         stats  = stats,
         alpha  = alpha,
         table  = flag_tbl),
    class = "epmfd_misfit"
  )
}
