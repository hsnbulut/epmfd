#' Load and validate data for epmfd workflow
#'
#' @param data A data.frame / tibble: rows = persons, columns = items.
#' @param id_col Optional <character>: column name holding unique IDs.
#' @param likert_levels Optional <integer>: maximum category K; if NULL
#'   it is inferred from the data.
#'
#' @return An object of class `epmfd_raw`.
#' @export
load_epmfd <- function(data,
                       id_col        = NULL,
                       likert_levels = NULL) {

  ## ---- 1. temel kontroller --------------------------------------------
  stopifnot(is.data.frame(data))

  ## ---- 2. ID -----------------------------------------------------------
  if (!is.null(id_col)) {
    if (!id_col %in% names(data))
      stop("id_col '", id_col, "' not found in data.")
    data <- dplyr::rename(data, id = {{ id_col }})
  } else {
    data$id <- seq_len(nrow(data))
  }
  id_vec <- data$id
  data   <- dplyr::select(data, -id)

  ## ---- 3. Likert seviyesi ---------------------------------------------
  if (is.null(likert_levels)) {
    K <- max(data, na.rm = TRUE)
    message("Detected maximum category K = ", K)
  } else {
    K <- as.integer(likert_levels)
  }

  ## ---- 4. Dönüştürme ve doğrulama -------------------------------------
  check_item <- function(x) {
    if (any(!is.na(x) & (x < 1 | x > K)))
      stop("Items contain values outside 1:", K, call. = FALSE)
    factor(x, levels = 1:K, ordered = TRUE)
  }
  data[] <- lapply(data, check_item)

  ## ---- 5. Eksik değer raporu ------------------------------------------
  nas <- sum(is.na(data))
  if (nas)
    message("Missing responses: ", nas, " (", round(100 * nas / prod(dim(data)), 2), "%)")

  ## ---- 6. Çıktı --------------------------------------------------------
  structure(
    list(data = data, id = id_vec, K = K),
    class = "epmfd_raw"
  )
}
