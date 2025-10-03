#' Compute person-fit statistics (polytomous data)
#'
#' `misfit_epmfd()` computes selected person-fit statistics for polytomous
#' responses and returns an `epmfd_misfit` object with scores, thresholds,
#' and logical flags per person.
#'
#' @param object An `epmfd_scaled` object (output of your scaling step).
#' @param stats  Character vector choosing which statistics to compute.
#'   Allowed values: `"auto"`, `"lpz"`, `"Gnp"`, `"U3p"`.
#'   If `"auto"` is present, the set is chosen based on the detected
#'   scaling method:
#'   - for `"mirt"`: `c("lpz","Gnp","U3p")`
#'   - for `"mokken"`: `c("Gnp","U3p")`
#' @param cut.off Cut-off for `Gnp` and `U3p`. Either `"auto"` (default; uses
#'   \pkg{PerFit}’s `cutoff()` with its implied tail), or a single numeric value
#'   (interpreted with tail `"upper"` for both `Gnp` and `U3p`). `lpz` uses a
#'   fixed lower-tail cut-off of `-1.645`.
#'
#' @importFrom stats setNames
#' @return An `epmfd_misfit` list with:
#' \itemize{
#'   \item `scaled`: the input `epmfd_scaled` object
#'   \item `method`: detected method (`"mirt"` or `"mokken"`)
#'   \item `stats`: actually computed statistics (subset of `c("lpz","Gnp","U3p")`)
#'   \item `thresholds`: named list of lists with `value` and `tail`
#'   \item `scores`: named list of numeric score vectors per statistic
#'   \item `table`: a tibble with `id`, one logical column per statistic,
#'                 `misfit_any` (OR over selected stats), and `misfit_final`
#'                 (see Details)
#' }
#'
#' @details
#' **Auto vs manual decision** for `misfit_final`:
#' - If `stats` contains `"auto"`:
#'   - for `"mokken"`: `misfit_final = Gnp & U3p`
#'   - for `"mirt"`:   `misfit_final = (lpz & Gnp & U3p)` if any such rows exist,
#'                     otherwise fallback to `lpz` only.
#' - If `stats` is manual (no `"auto"`): `misfit_final` is the AND over the
#'   selected statistics (if only one selected, it is used directly).
#'
#' All PerFit statistics for polytomous items assume a *common number of
#' categories per item*. The function checks this and errors otherwise.
#'
#' @examples
#' \dontrun{
#' # Assume `sc` is an epmfd_scaled object
#' mf1 <- misfit_epmfd(sc, stats = "auto")             # method-dependent set
#' mf2 <- misfit_epmfd(sc, stats = c("Gnp","U3p"))     # manual selection
#' mf3 <- misfit_epmfd(sc, stats = c("lpz","Gnp"), cut.off = 0.45)
#'
#' # Inspect flags
#' head(mf1$table)
#' mf1$thresholds
#' }
#'
#' @export
misfit_epmfd <- function(object,
                         stats   = c("auto", "lpz", "Gnp", "U3p"),
                         cut.off = "auto") {
  if (!inherits(object, "epmfd_scaled"))
    stop("Input must be an 'epmfd_scaled' object.")

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  ## 1) Detect scaling method
  method <- tolower(as.character(object$method %||% ""))
  if (!method %in% c("mokken","mirt")) {
    is_mirt <- !is.null(object$model) && inherits(object$model, "SingleGroupClass")
    method  <- if (is_mirt) "mirt" else "mokken"
    warning("`object$method` was not recognized; inferred method = '", method, "'.")
  }

  ## 2) Select stats (auto vs manual)
  valid_stats <- c("lpz","Gnp","U3p")
  auto_mode <- "auto" %in% stats
  if (auto_mode) {
    stats <- if (method == "mirt") c("lpz","Gnp","U3p") else c("Gnp","U3p")
  } else {
    stats <- intersect(valid_stats, stats)
    if (!length(stats)) stop("`stats` must include at least one of: 'lpz', 'Gnp', 'U3p'.")
  }

  ## 3) Prepare polytomous matrix for PerFit: 0..(Ncat-1)
  X_df <- object$raw$data |>
    dplyr::select(dplyr::all_of(object$kept))

  normalize0 <- function(v) {
    v <- as.integer(v)
    u <- sort(unique(v))
    if (identical(u, 0:(length(u)-1L))) return(v)
    if (identical(u, 1:length(u)))      return(v - 1L)
    map <- setNames(0:(length(u)-1L), u)
    as.integer(map[as.character(v)])
  }

  X_df[] <- lapply(X_df, normalize0)
  X <- as.matrix(X_df)
  storage.mode(X) <- "numeric"

  ncat_by_item <- apply(X, 2, function(col) length(unique(col)))
  if (length(unique(ncat_by_item)) != 1L) {
    stop(
      "PerFit polytomous statistics require a common number of categories ",
      "across items. Found Ncat per item: ",
      paste0(ncat_by_item, collapse = ", "), "."
    )
  }
  Ncat <- as.integer(unique(ncat_by_item))
  IDs  <- object$raw$id

  ## 4) 4) Helpers
  .flag_by_cutoff <- function(scores, cutoff_obj, default_tail = "upper") {
    cutv <- as.numeric(cutoff_obj$Cutoff)[1L]
    tail <- tolower(as.character((cutoff_obj$Tail %||% default_tail)))[1L]
    if (tail == "lower") scores < cutv else scores > cutv
  }
  .flag_by_numeric <- function(scores, thr, tail = "upper") {
    thr  <- as.numeric(thr)[1L]
    tail <- tolower(as.character(tail))[1L]
    if (tail == "lower") scores < thr else scores > thr
  }
  .coalesce_logical <- function(x) ifelse(is.na(x), FALSE, x)

  ## 5) Compute requested statistics
  scores <- list(); flags <- list(); thresholds <- list()

  if ("Gnp" %in% stats) {
    Gobj <- PerFit::Gnormed.poly(X, Ncat = Ncat)
    g    <- as.vector(Gobj$PFscores)
    names(g)<-"Gnp"
    g<-sapply(g,as.numeric)
    scores$Gnp <- g
    if (identical(cut.off, "auto")) {
      Gcut <- PerFit::cutoff(Gobj)
      thresholds$Gnp <- list(type="auto",
                             value = as.numeric(Gcut$Cutoff)[1L],
                             tail  = tolower(as.character(Gcut$Tail))[1L] %||% "upper")
      flags$Gnp <- .flag_by_cutoff(g, Gcut, default_tail = "upper")
    } else if (is.numeric(cut.off) && length(cut.off) == 1L) {
      thresholds$Gnp <- list(type="numeric", value=cut.off, tail="upper")
      flags$Gnp <- .flag_by_numeric(g, cut.off, tail = "upper")
    } else stop("`cut.off` must be 'auto' or a single numeric value.")
  }

  if ("U3p" %in% stats) {
    Uobj <- PerFit::U3poly(X, Ncat = Ncat)
    u    <- as.vector(Uobj$PFscores)
    names(u)<-"U3p"
    u<-sapply(u,as.numeric)
    scores$U3p <- u
    if (identical(cut.off, "auto")) {
      Ucut <- PerFit::cutoff(Uobj)
      thresholds$U3p <- list(type="auto",
                             value = as.numeric(Ucut$Cutoff)[1L],
                             tail  = tolower(as.character(Ucut$Tail))[1L] %||% "upper")
      flags$U3p <- .flag_by_cutoff(u, Ucut, default_tail = "upper")
    } else if (is.numeric(cut.off) && length(cut.off) == 1L) {
      thresholds$U3p <- list(type="numeric", value=cut.off, tail="upper")
      flags$U3p <- .flag_by_numeric(u, cut.off, tail = "upper")
    } else stop("`cut.off` must be 'auto' or a single numeric value.")
  }

  if ("lpz" %in% stats) {
    Lobj <- PerFit::lzpoly(X, Ncat = Ncat)
    l    <- as.vector(Lobj$PFscores)
    names(l)<-"lpz"
    l<-sapply(l,as.numeric)
    scores$lpz <- l
    thresholds$lpz <- list(type="fixed", value=-1.645, tail="lower")
    flags$lpz <- l < -1.645
  }

  ## 6) Assemble table and decision rules
  tbl <- data.frame(id = IDs, stringsAsFactors = FALSE)
  for (nm in names(flags)) tbl[[nm]] <- .coalesce_logical(as.logical(flags[[nm]]))

  # OR over selected flags
  if (length(names(flags))) {
    tbl$misfit_any <- Reduce(`|`, lapply(names(flags), function(nm) tbl[[nm]]))
  } else {
    tbl$misfit_any <- FALSE
  }

  # misfit_final:
  if (auto_mode) {
    if (method == "mokken") {
      # auto → classical rule
      g <- tbl[["Gnp"]] %||% FALSE
      u <- tbl[["U3p"]] %||% FALSE
      tbl$misfit_final <- g & u
    } else { # mirt + auto
      lp <- tbl[["lpz"]] %||% FALSE
      g  <- tbl[["Gnp"]] %||% FALSE
      u  <- tbl[["U3p"]] %||% FALSE
      all3 <- lp & g & u
      tbl$misfit_final <- if (any(all3)) all3 else lp
    }
  } else {

    if (length(names(flags))) {
      tbl$misfit_final <- Reduce(`&`, lapply(names(flags), function(nm) tbl[[nm]]))
    } else {
      tbl$misfit_final <- FALSE
    }
  }

  ## 7) Output
  out <- list(
    scaled     = object,
    method     = method,
    stats      = names(scores),
    thresholds = thresholds,
    scores     = scores,
    table      = tibble::as_tibble(tbl)
  )
  class(out) <- c("epmfd_misfit","list")
  out
}




