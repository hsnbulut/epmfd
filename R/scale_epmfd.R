#' Scale Data via MIRT or Mokken
#'
#' @param object  An `epmfd_raw` created by [load_epmfd()].
#' @param method  "auto", "mirt", or "mokken".
#' @param H_thr   Numeric. Min scalability coefficient for Mokken (default 0.3).
#' @param a_thr   Numeric. Min discrimination for MIRT (default 0.5).
#' @param ...     Further arguments passed to `mirt()` or `mokken::check()`/`mokken::aisp`.
#' @return An object of class `epmfd_scaled`.
#' @export
scale_epmfd <- function(object,
                        method = c("auto", "mirt", "mokken"),
                        H_thr = 0.3,
                        a_thr = 0.5,
                        ...) {

  method <- match.arg(method)
  n      <- nrow(object$data)

  # automatic decision --------------------------------------------------------
  if (method == "auto")
    method <- if (n >= 100) "mirt" else "mokken"

  dat <- object$data
  dat[] <- lapply(dat, function(z) if (is.factor(z)) as.integer(z) else z)
  removed <- character(0)      # isimlerini tutacağız

  if (method == "mirt") {
    # ----------------- 1. model (tüm maddelerle) -----------------------------
    mod1 <- mirt::mirt(dat, 1, itemtype = "graded", verbose = FALSE, ...)
    pars <- mirt::coef(mod1, IRTpars = TRUE, simplify = TRUE)$items

    weak_items <- rownames(pars)[pars[ , "a"] < a_thr]
    if (length(weak_items)) {
      message("Removed items (a < ", a_thr, "): ", paste(weak_items, collapse = ", "))
      dat <- dat[ , !(names(dat) %in% weak_items), drop = FALSE]
      removed <- weak_items
      # ----------------- 2. model (temizlenen set) ---------------------------
      mod1 <- mirt::mirt(dat, 1, itemtype = "graded", verbose = FALSE, ...)
    }

    result <- list(raw     = object,
                   method  = "mirt",
                   model   = mod1,
                   kept    = names(dat),
                   removed = removed,
                   a_thr   = a_thr)

  } else {  # ---------- mokken --------------------------------------------
    # H_i katsayılarını al
    Hs <- mokken::coefH(dat)$Hi        # <- check() yerine

    weak_items  <- names(Hs)[Hs < H_thr]
    if (length(weak_items)) {
      message("Removed items (H < ", H_thr, "): ",
              paste(weak_items, collapse = ", "))
      dat     <- dat[ , !(names(dat) %in% weak_items), drop = FALSE]
      removed <- weak_items
      Hs      <- mokken::coefH(dat)$Hi   # yeniden hesapla
    }

    scale_out <- mokken::aisp(dat, lowerbound = H_thr, ...)

    result <- list(raw      = object,
                   method   = "mokken",
                   scales   = scale_out,
                   kept     = names(dat),
                   removed  = removed,
                   H_thr    = H_thr,
                   Hi       = Hs)
  }

  class(result) <- c("epmfd_scaled", class(result))
  return(result)
}
