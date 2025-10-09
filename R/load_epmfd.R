#' Load and validate raw data for the epmfd workflow
#'
#' `load_epmfd()` prepares raw item-response data for subsequent
#' functions in the epmfd workflow. It validates input, ensures that all
#' item responses fall within the expected range of categories, converts
#' items to ordered factors, and attaches person IDs.
#'
#' @param data A data.frame or tibble with persons in rows and items in columns.
#'             All item responses must be integers in \code{1:K}, possibly with
#'             missing values.
#' @param id_col Optional \code{character} string giving the column name
#'               containing unique person identifiers. If \code{NULL}, a simple
#'               integer sequence \code{1:n} is used.
#' @param likert_levels Optional \code{integer} specifying the maximum category
#'               value (K). If \code{NULL}, K is inferred automatically as the
#'               maximum observed value in the data.
#'
#' @return An object of class \code{epmfd_raw}, a list with elements:
#' \itemize{
#'   \item \code{data}: A data.frame of ordered-factor responses
#'   \item \code{id}: Vector of person IDs
#'   \item \code{K}: Maximum number of categories per item
#' }
#'
#'@importFrom rlang sym
#' @details
#' Each column of \code{data} is validated to ensure responses are within
#' \code{1:K}. Values outside this range cause an error. Missing values
#' are allowed and reported.
#'
#' @examples
#' # Example: 5 persons × 3 items, responses 1–4
#' df <- data.frame(
#'   Pid = paste0("P", 1:5),
#'   Item1 = c(1, 2, 3, 2, 1),
#'   Item2 = c(2, 3, 4, 2, 1),
#'   Item3 = c(3, 4, 1, 2, 2)
#' )
#'
#' raw <- load_epmfd(df, id_col = "Pid", likert_levels = 4)
#' str(raw)
#'
#'
#' @seealso [scale_epmfd()], [misfit_epmfd()]
#'
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
