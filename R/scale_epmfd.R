#' Scale polytomous item responses
#'
#' `scale_epmfd()` fits either a parametric graded response model (GRM, via
#' \pkg{mirt}) or a nonparametric Mokken model (via \pkg{mokken}) to
#' polytomous item-response data and filters out weak items based on
#' user-specified thresholds.
#'
#' @param object An \code{epmfd_raw} object created by [load_epmfd()].
#' @param method Scaling method. One of:
#'   \itemize{
#'     \item \code{"mirt"}: fit a one-factor graded response model (GRM).
#'     \item \code{"mokken"}: perform nonparametric Mokken scale analysis.
#'     \item \code{"auto"} (default): choose based on sample size
#'           (\code{n >= 500} → GRM, otherwise Mokken).
#'   }
#' @param a_thr Numeric. Threshold for item discrimination parameter \code{a}
#'              when using GRM (default = 0.5). Items with \code{a < a_thr} are
#'              removed.
#' @param H_thr Numeric. Threshold for item scalability coefficient \code{H_i}
#'              when using Mokken analysis (default = 0.3). Items with
#'              \code{H_i < H_thr} are removed.
#'
#' @return An object of class \code{epmfd_scaled}, a list containing:
#' \itemize{
#'   \item \code{raw}: the original \code{epmfd_raw} object
#'   \item \code{method}: scaling method actually used (\code{"mirt"} or
#'         \code{"mokken"})
#'   \item \code{kept}: names of items retained
#'   \item \code{removed}: names of items removed
#'   \item \code{model}: fitted GRM model (for \code{"mirt"}), else \code{NULL}
#'   \item \code{ai}: item parameter estimates (for \code{"mirt"})
#'   \item \code{a_thr}: discrimination threshold used (for \code{"mirt"})
#'   \item \code{model_fit}: results of \code{mirt::M2()} (if available)
#'   \item \code{Hi}: vector of item scalability coefficients (for
#'         \code{"mokken"})
#'   \item \code{H_thr}: scalability threshold used (for \code{"mokken"})
#'   \item \code{items}: the vector containing all items names.
#' }
#'
#' @details
#' The function converts ordered factors to numeric before analysis.
#' - For GRM (\code{mirt}), items are filtered by their discrimination
#'   parameter \code{a}. The overall model fit is attempted using
#'   \code{mirt::M2()}; if this fails (e.g., due to insufficient df), a warning
#'   is issued and \code{model_fit = NULL}.
#' - For Mokken, item scalability coefficients \code{H_i} are computed and
#'   compared to \code{H_thr}.
#'
#' @seealso [load_epmfd()], [misfit_epmfd()], [plot.epmfd_scaled()]
#'
#' @examplesIf FALSE
#' \donttest{
#' # Example: scale raw data automatically
#' raw <- load_epmfd(mydata, id_col = "ID", likert_levels = 4)
#' sc1 <- scale_epmfd(raw, method = "auto")
#'
#' # Force Mokken analysis
#' sc2 <- scale_epmfd(raw, method = "mokken", H_thr = 0.4)
#'
#' # Force GRM (mirt) analysis with stricter a-threshold
#' sc3 <- scale_epmfd(raw, method = "mirt", a_thr = 0.7)
#' }
#'
#' @export
scale_epmfd <- function(object,
                        method = c("auto", "mirt", "mokken"),
                        a_thr = 0.5,
                        H_thr = 0.3) {

  if (!inherits(object, "epmfd_raw"))
    stop("Input must be an 'epmfd_raw' object.")

  # veriyi sayısala çevir
  X <- apply(as.data.frame(object$data), 2, as.numeric)
  n <- nrow(X)
  method <- match.arg(method)

  if (method == "auto") {
    method <- if (n >= 500) "mirt" else "mokken"
  }

  kept <- removed <- NULL
  Hi <- NULL
  model <- NULL

  if (method == "mirt") {
    # 1 faktörlü GRM
    model <- mirt::mirt(X, 1, itemtype = "graded", verbose = FALSE)

    # madde ayırıcılıklarına göre filtre
    pars <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)$items
    kept <- rownames(pars)[pars[, "a"] >= a_thr]
    removed <- setdiff(rownames(pars), kept)

    # M2: hata verirse durma; uyarı ver ve NULL ata
    M2_results <- tryCatch(
      mirt::M2(model),
      error = function(e) {
        warning(
          "M2 could not calculated.", conditionMessage(e),
          "(The degree of freedom may not be enough)."
        )
        NULL
      }
    )

    out <- list(
      raw       = object,
      method    = method,
      kept      = kept,
      removed   = removed,
      model     = model,
      ai        = pars,
      a_thr     = a_thr,
      items     = rownames(pars),
      model_fit = M2_results   # hata varsa NULL
    )

  } else {
    # Mokken ölçekleme
    mok <- mokken::coefH(X, results = FALSE)
    name <- rownames(mok$Hi)
    Hi <- as.numeric(mok$Hi[, 1])
    kept <- name[Hi >= H_thr]
    removed <- setdiff(name, kept)

    out <- list(
      raw     = object,
      method  = method,
      kept    = kept,
      removed = removed,
      Hi      = Hi,
      H_thr   = H_thr,
      items   = name
    )
  }

  class(out) <- c("epmfd_scaled", "list")
  out
}

