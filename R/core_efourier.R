#' EFT
#'
#' @param x plo
#' @param nb_h `int` nb of harmonics. Default to `6` for `efourier`, to all of them for `efourier_i`
#' @param nb_pts `int` nb of points for the reconstruction
#' @param keep_coo `logical` whether to retain coo column
#' @param ... for generics. Useless here.
#'
#' @export
efourier <- function(x, nb_h, keep_coo, ...) {
  UseMethod("efourier")
}

#' @export
#' @rdname efourier
efourier.default <- function(x, nb_h, ...){
  .msg_warning("only defined on <coo_single> and <coo_tbl>")
}

#' @export
#' @rdname efourier
efourier.coo_single <- function (x, nb_h, ...) {
  x <- validate_coo_single(x)
  # if (coo_is_closed(coo))
  #   coo <- coo_unclose(coo)
  n <- nrow(x)
  nb_h_max <- floor(n/2)

  # handles missing or too ambitious nb_h
  if (missing(nb_h)){
    nb_h <- ifelse(nb_h_max < 6, nb_h_max, 6)
    .msg_info("nb_h was missing and set to {nb_h}")
  } else {
    if (nb_h > nb_h_max){
      nb_h <- nb_h_max
      .msg_info("nb_h was too ambitious and set to {nb_h}")
    }
  }

  # directly borrowed from Julien Claude
  # and brushed up but fundamentally untouched since Momocs v0
  Dx <- x$x - x$x[c(n, (1:(n - 1)))]
  Dy <- x$y - x$y[c(n, (1:(n - 1)))]
  Dt <- sqrt(Dx^2 + Dy^2)
  Dt[Dt < 1e-10] <- 1e-10
  t1 <- cumsum(Dt)
  t1m1 <- c(0, t1[-n])
  T <- sum(Dt)

  # prepare res
  a <- numeric(nb_h) %>% .seq_naming_vector("a")
  b <- numeric(nb_h) %>% .seq_naming_vector("b")
  c <- numeric(nb_h) %>% .seq_naming_vector("c")
  d <- numeric(nb_h) %>% .seq_naming_vector("d")

  for (i in 1:nb_h) {
    Ti <- (T/(2 * pi^2 * i^2))
    r <- 2 * i * pi
    a[i] <- Ti * sum((Dx/Dt) * (cos(r * t1/T) - cos(r * t1m1/T)))
    b[i] <- Ti * sum((Dx/Dt) * (sin(r * t1/T) - sin(r * t1m1/T)))
    c[i] <- Ti * sum((Dy/Dt) * (cos(r * t1/T) - cos(r * t1m1/T)))
    d[i] <- Ti * sum((Dy/Dt) * (sin(r * t1/T) - sin(r *  t1m1/T)))
  }
  a0 <- 2 * sum(x[, 1] * Dt/T)
  c0 <- 2 * sum(x[, 2] * Dt/T)

  # return this beauty as an efourier_single list
  res <- list(a = a, b = b, c = c, d = d,
              a0 = a0, c0 = c0)
  res %>% .append_class("efourier_single")
}

#' @export
#' @rdname efourier
efourier.coo_tbl <- function(x, nb_h, keep_coo=FALSE, ...){
  if (missing(nb_h)){
    nb_h <- 6
    .msg_info("nb_h was missing and set to {nb_h}")
  } else {
    min_nb_pts <- min(purrr::map_dbl(x$coo, nrow))
    nb_h_max <- floor(min_nb_pts/2)
    if (nb_h > nb_h_max){
      nb_h <- nb_h_max
      .msg_info("nb_h was too ambitious and set to {nb_h}")
    }
  }

  # map efourier
  res <- x %>% dplyr::mutate(coe=.data$coo %>%
                               purrr::map(efourier, nb_h) %>%
                               purrr::map(as_tibble))

  # drop or dont drop coo and add class labels
  if (keep_coo){
    res <- res %>% .append_class("coe_tbl")
  } else {
    res <- res %>% dplyr::select(-.data$coo) %>% .replace_class("coo_tbl", "coe_tbl")
  }
  # return this beauty
  res
}

# help split coeff lists
.coeff_split <- function (x, cph = 4){
  # deduce the number of harmonics
  nb.h <- length(x)/cph
  # ensure individual names
  names(x) <- paste0(rep(letters[1:cph], each=nb.h), rep(1:nb.h, times=cph))
  # split the vector into a list
  split(x, rep(paste0(letters[1:cph], "n"), each=nb.h))
}

#' @export
#' @rdname efourier
efourier_i <- function(x, nb_h, nb_pts = 120){
  UseMethod("efourier_i")
}

#' @export
efourier_i.default <- function (x, nb_h, nb_pts = 120) { # efourier_i.efourier_single ?
  # so that list of vectors can be passed
  if (is.vector(x))
    x <- .coeff_split(x)
  if (is.null(x$ao))
    ao <- 0
  if (is.null(x$co))
    co <- 0

  # regular theta seq
  theta <- seq(0, 2 * pi, length.out = nb_pts+1)[-(nb_pts+1)]

  # by default use them all
  if (missing(nb_h))
    nb_h <- length(x$an)

  # prototypic matrices to host results
  hy <- hx <- matrix(NA_real_, nb_h, nb_pts)

  # what's wrong with loops?
  for (i in 1:nb_h) {
    hx[i, ] <- x$an[i] * cos(i * theta) + x$bn[i] * sin(i * theta)
    hy[i, ] <- x$cn[i] * cos(i * theta) + x$dn[i] * sin(i * theta)
  }

  # add positionnal params and summed harmonic contributions
  # and return a tibble
  tibble::tibble(x=(ao/2) + apply(hx, 2, sum),
                 y=(co/2) + apply(hy, 2, sum)) %>%
    coo_single()
}

# could have been in efourier_default
# but save readability in the latter
#' @export
as_tibble.efourier_single <- function(x, ...){
  x[c("a", "b", "c", "d")] %>%
    purrr::flatten() %>%
    dplyr::bind_cols()
}

