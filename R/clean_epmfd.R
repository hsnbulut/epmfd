#' Remove mis-fitting persons
#'
#' @param misfit An object returned by [misfit_epmfd()].
#' @param criterion "union" (default) = at least one statistic flags
#'   a person; "intersection" = all statistics flag the person.
#'
#' @return An `epmfd_clean` object that also contains a ready-to-use
#'   `epmfd_raw` object in the slot `raw`, so you can call
#'   `scale_epmfd(clean$raw, ...)` directly.
#' @export
clean_epmfd <- function(misfit,
                        criterion = c("union", "intersection")) {

  stopifnot(inherits(misfit, "epmfd_misfit"))
  criterion <- match.arg(criterion)

  tbl   <- misfit$table
  stats <- misfit$stats

  ## ------------------------------------------------------------------
  ## 1  – Uyumsuz kişileri belirle
  ## ------------------------------------------------------------------
  fit_flag <- if (criterion == "union") {
    !tbl$misfit_any
  } else {                              # intersection
    rowSums(!tbl[stats]) == length(stats)
  }

  n_removed <- sum(!fit_flag)

  ## ------------------------------------------------------------------
  ## 2  – Temiz madde verisi (yalnız kept maddeler)
  ## ------------------------------------------------------------------
  kept_items <- misfit$scaled$kept            # dizge vektörü
  raw_orig   <- misfit$scaled$raw

  clean_data <- raw_orig$data[fit_flag, kept_items, drop = FALSE]
  clean_id   <- raw_orig$id[fit_flag]

  ## sütunları ordered-factor olarak bırakalım → load_epmfd gerekmez
  K <- raw_orig$K

  ## ------------------------------------------------------------------
  ## 3  – Yeni epmfd_raw objesini oluştur
  ## ------------------------------------------------------------------
  raw_clean <- list(
    data = clean_data,
    id   = clean_id,
    K    = K
  )
  class(raw_clean) <- c("epmfd_raw", "list")

  ## ------------------------------------------------------------------
  ## 4  – epmfd_clean nesnesi
  ## ------------------------------------------------------------------
  out <- list(
    raw        = raw_clean,      # doğrudan ölçeklenebilir
    clean_data = clean_data,     # ham veri çerçevesi
    n_removed  = n_removed,
    criterion  = criterion,
    misfit     = misfit
  )
  class(out) <- c("epmfd_clean", "list")
  return(out)
}
