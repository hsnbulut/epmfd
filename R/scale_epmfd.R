#' Scale polytomous item responses
#'
#' Automatically fits either a GRM model (via `mirt`) or a non-parametric
#' Mokken model to the response data and filters out weak items.
#'
#' @param object An \code{epmfd_raw} object.
#' @param method Scaling method: \code{"mirt"}, \code{"mokken"}, or
#'   \code{"auto"} (chooses based on sample size).
#' @param a_thr Threshold for discrimination parameter \code{a} in GRM
#'   (default 0.5).
#' @param H_thr Threshold for scalability coefficient \code{H_i} in Mokken
#'   (default 0.3).
#' @param drop_constant_persons Logical. Whether to remove persons who gave
#'   the same response to all items (default \code{TRUE}).
#'
#' @return An \code{epmfd_scaled} object.
#' @export
scale_epmfd <- function(object,
                        method = c("auto", "mirt", "mokken"),
                        a_thr = 0.5,
                        H_thr = 0.3,
                        drop_constant_persons = TRUE) {

  if (!inherits(object, "epmfd_raw"))
    stop("Input must be an 'epmfd_raw' object.")

  X <- object$data

  # ----- Drop constant-response persons -----------------------------------
  if (drop_constant_persons) {
    same_response <- apply(X, 1, function(row) length(unique(row)) == 1)
    n_dropped <- sum(same_response)
    if (n_dropped > 0) {
      warning(sprintf("%d persons gave identical responses to all items and were removed.", n_dropped))
      X <- X[!same_response, , drop = FALSE]
      object$id <- object$id[!same_response]
    }
  }

  n <- nrow(X)
  method <- match.arg(method)

  if (method == "auto") {
    method <- if (n >= 100) "mirt" else "mokken"
  }

  kept <- removed <- NULL
  Hi <- NULL
  model <- NULL

  if (method == "mirt") {
    model <- mirt::mirt(X, 1, itemtype = "graded", verbose = FALSE)
    pars <- mirt::coef(model, IRTpars = TRUE, simplify = TRUE)$items
    kept <- rownames(pars)[pars[, "a"] >= a_thr]
    removed <- setdiff(rownames(pars), kept)
  } else {
    mokken <- mokken::mokken(X)
    Hi <- mokken$Hi
    kept <- names(Hi)[Hi >= H_thr]
    removed <- setdiff(names(Hi), kept)
  }

  out <- list(
    raw     = object,
    method  = method,
    kept    = kept,
    removed = removed,
    model   = model,
    Hi      = Hi,
    a_thr   = a_thr,
    H_thr   = H_thr
  )
  class(out) <- c("epmfd_scaled", "list")
  return(out)
}
