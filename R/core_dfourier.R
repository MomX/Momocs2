# dfourier ------------------------------------------------

#' Discrete cosine transform
#'
#' Calculates discrete cosine transforms, as introduced by Dommergues and colleagues, on
#' open outlines.
#'
#' Shapes must be normalized, ie using [coo_bookstein] before performing the dct transform.
#'
#' @param x [coo_single], [coo_list] or [mom_tbl]
#' @param nb_h `int` nb of harmonics. Default to `6` for `dfourier`, to all of them for `dfourier_i`
#' @param raw `logical` whether to return raw and full results for [dfourier]
#' @param nb_pts `int` nb of points for the reconstruction
#' @param drop_coo `logical` whether to drop coo column (default to `TRUE`)
#' @param from_col,to_col column names
#' @param ... for generics. Useless here.
#' @return a list with components when applied on a single shape:
#'
#' @references
#' \itemize{
#'  \item Dommergues, C. H., Dommergues, J.-L., & Verrecchia, E. P. (2007).
#'  The Discrete Cosine Transform, a Fourier-related Method for Morphometric Analysis of Open Contours.
#'  \emph{Mathematical Geology}, 39(8), 749-763. doi:10.1007/s11004-007-9124-6
#'  \item Many thanks to Remi Laffont for the original translation in R).
#' }
#'
#' @family morphometrics
#' @examples
#' olea[1:3, ] %>% dfourier()
#' @export
dfourier <- function(x, nb_h, raw=FALSE, from_col, to_col, drop_coo, ...) {
  UseMethod("dfourier")
}

#' @export
dfourier.default <- function(x, ...){
  not_defined("dfourier")
}

#' @export
dfourier.coo_single <- function(x, nb_h, raw=FALSE, ...) {
  # we check a bit
  if (missing(nb_h)) {
    nb_h <- 6
    .msg_warning("dfourier: 'nb_h' not provided and set to {nb_h}")
  }
  # preliminaries
  x <- as.matrix(x)
  N <- nrow(x)
  pol <- x[, 1] + (0+1i) * x[, 2]
  # dfourier
  c <- rep(sqrt(2/N), N)
  c[1] <- 1/sqrt(N)
  Sv <- S <- rep(NA, N)
  for (k in 0:(N-1)) {
    Sv <- pol * cos(((2 * 0:(N-1)) * k * pi)/(2 * N))
    S[k+1] <- c[k+1] * sum(Sv)
  }
  S <- S[2:(nb_h+1)] #we remove the 1st harmonic

  # prepare for exit
  res <- list(an = Re(S), bn = Im(S), mod = Mod(S), phi = Arg(S))
  # early return if raw
  if (raw)
    return(res)
  # otherwise, turn it into a coe_single
  ns <- paste0(rep(letters[1:2], each=nb_h), rep(1:nb_h, times=2))
  res[c("an", "bn")] %>%
    purrr::flatten() %>%
    purrr::set_names(ns) %>%
    dplyr::bind_cols() %>%
    coe_single() %>%
    .append_class("dfourier_single")
}


#' @export
dfourier.coo_list <- function (x, nb_h, ...) {
  # we check a bit
  if (missing(nb_h)) {
    nb_h <- 6
    .msg_warning("dfourier: 'nb_h' not provided and set to {nb_h}")
  }

  x %>%
    purrr::map(dfourier, nb_h=nb_h, raw=FALSE) %>%
    new_coe_list() %>%
    .append_class("dfourier")
}


#' @export
dfourier.mom_tbl <- function(x, nb_h, raw=FALSE, from_col=coo, to_col=coe, drop_coo=TRUE, ...) {
  # we check a bit
  if (missing(nb_h)) {
    nb_h <- 6
    .msg_warning("dfourier: 'nb_h' not provided and set to {nb_h}")
  }
  from_col <- enquo(from_col)
  to_col   <- enquo(to_col)

  res <- dplyr::pull(x, !!from_col) %>% dfourier(nb_h, raw=FALSE)
  res <- dplyr::mutate(x, !!to_col := res)

  # drop or dont drop coo and add class labels
  if (drop_coo)
    res <- dplyr::select(res, -(!!from_col))
  # return this beauty
  res
}


# dfourier_i ----------------------------------------------
#' @describeIn dfourier inverse dfourier method
#' @export
dfourier_i <- function(x, nb_h, nb_pts, from_col, to_col, ...){
  UseMethod("dfourier_i")
}

#' dfourier_i.default <- function(x, ...){
#'   not_defined("dfourier_i")
#' }
#'
#' dfourier_i.coe_single <- function(x, nb_h, nb_pts=120, ...){
#'   # full usage by default
#'   # deduce the number of harmonics
#'   # not worth a message
#'   if (missing(nb_h))
#'     nb_h <- ncol(x)/2
#'
#'
#'
#'   # inherited from Momocs
#'   A <- x[1:nb_h] %>% unlist()
#'   B <- x[-(1:nb_h)] %>% unlist() %>% print()
#'
#'   # yep
#'   nb_h <- nb_h+1
#'
#'   c <- rep(sqrt(2/nb_pts), nb_pts)
#'   c[1] <- 1/sqrt(nb_pts)
#'
#'   S <- A + (0+1i) * B
#'   S <- c(0+1i, S)  # we add a trivial harmonic corresponding to (0; 0)
#'
#'   sv_r <- rep(NA, nb_h)
#'   s_r <- rep(NA, nb_pts)
#'   # idfourier pour le nombre d'harmonique specifie
#'   for (n in 0:(nb_pts - 1)) {
#'     for (k in 0:(nb_h - 1)) {
#'       sv_r[k + 1] <- c[k + 1] * S[k + 1] * cos(((2 * n + 1) * k * pi)/(2 * nb_pts))
#'     }
#'     s_r[n + 1] <- sum(sv_r)
#'   }
#'  cbind(Re(s_r), Im(s_r)) %>%
#'    coo_single() %>%
#'    coo_bookstein()
#' }
#'
#' # microbenchmark::microbenchmark(
#' # olea %>% pick %>% dfourier() %>% dfourier_i()
#' # )
#'
#' dfourier_i.coe_list <- function(x, nb_h, nb_pts, ...){
#'
#' }
#'
#' #' @describeIn dfourier inverse dfourier method
#' #' @export
#' dfourier_i <- function(x, nb_h, nb_pts = 60) {
#'   A <- df$an
#'   B <- df$bn
#'   if (missing(nb_h)) {
#'     nb_h <- length(A) + 1
#'   }
#'
#'   c <- rep(sqrt(2/nb_pts), nb_pts)
#'   c[1] <- 1/sqrt(nb_pts)
#'
#'   S <- A + (0+1i) * B
#'   S <- c(0+1i, S)  # we add a trivial harmonic corresponding to (0; 0)
#'
#'   sv_r <- rep(NA, nb_h)
#'   s_r <- rep(NA, nb_pts)
#'   # idfourier pour le nombre d'harmonique specifie
#'   for (n in 0:(nb_pts - 1)) {
#'     for (k in 0:(nb_h - 1)) {
#'       sv_r[k + 1] <- c[k + 1] * S[k + 1] * cos(((2 * n + 1) * k * pi)/(2 * nb_pts))
#'     }
#'     s_r[n + 1] <- sum(sv_r)
#'   }
#'   shp <- cbind(Re(s_r), Im(s_r))
#'   return(shp)
#' }
