# utils ---------------------------------------------------
# help split coeff lists
.coeff_split <- function (x, cph = 4){
  # so that it can work with coe_single
  if (is.data.frame(x))
    x <- unlist(x)

  # if missing cph, tries to deduce it
  if (missing(cph)){
    n <- x %>% names()
    chp <- n %>% stringr::str_remove("[[:digit:]]") %>% table %>% length()
  }

  # deduce the number of harmonics
  nb.h <- length(x)/cph
  # ensure individual names
  names(x) <- paste0(rep(letters[1:cph], each=nb.h), rep(1:nb.h, times=cph))
  # split the vector into a list
  split(x, rep(paste0(letters[1:cph], "n"), each=nb.h))
}

.check_efourier_nb_h <- function(x, nb_h=NA){
  # is single, list it
  if (is_coo_single(x))
    x <- list(x)

  # if tbl extract coo column
  if (is_mom_tbl(x))
    x <- x$coo

  n <- min(purrr::map_dbl(x, nrow))
  nb_h_max <- floor(n/2)

  # handles missing or too ambitious nb_h
  if (missing(nb_h) | is.na(nb_h)){
    nb_h <- ifelse(nb_h_max < 6, nb_h_max, 6)
    .msg_info("efourier: nb_h was missing and set to {nb_h}")
  } else {
    if (nb_h > nb_h_max){
      nb_h <- nb_h_max
      .msg_info("efourier: nb_h was too ambitious and set to {nb_h}")
    }

  }
  # return safe nb_h
  nb_h
}

# efourier ------------------------------------------------
#' Elliptical Fourier transforms
#'
#' @param x [coo_single], [coo_list] or [mom_tbl]
#' @param nb_h `int` nb of harmonics. Default to `6` for `efourier`, to all of them for `efourier_i`
#' @param nb_pts `int` nb of points for the reconstruction
#' @param keep_coo `logical` whether to retain coo column
#' @param raw `logical` whether to return raw and full results for [efourier] and [efourier_norm]
#' @param first_point `logical` whether to normalize for the first point using [efourier_norm]
#' @param ... for generics. Useless here.
#'
#' @details For the maths behind see the paper in JSS.
#'
#' Normalization of coefficients has long been a matter of trouble,
#' and not only for newcomers. There are two ways of normalizing outlines: the first,
#' and by far the most used, is to use a "numerical" alignment, directly on the
#' matrix of coefficients. The coefficients of the first harmonic are consumed
#' by this process but harmonics of higher rank are normalized in terms of size
#' and rotation. This is sometimes referred as using the "first ellipse", as the
#' harmonics define an ellipse in the plane, and the first one is the mother of all
#' ellipses, on which all others "roll" along. This approach is really convenient
#' as it is done easily by most software (if not the only option) and by Momocs too.
#' It is the default option of [efourier].
#'
#' But here is the pitfall: if your shapes are prone to bad aligments among all
#' the first ellipses, this will result in poorly (or even not at all) "homologous" coefficients.
#' The shapes particularly prone to this are either (at least roughly) circular and/or with a strong
#' bilateral symmetry. Also, and perhaps more explicitely, morphospace usually show a mirroring symmetry,
#' typically visible when calculated in some couple of components (usually the first two).
#'
#' If you see these  upside-down (or 180 degrees rotated) shapes on the morphospace,
#' you should seriously consider aligning your shapes __before__ the [efourier] step,
#' and performing the latter with `norm = FALSE`.
#'
#' You have several options to align your shapes, using control points (or landmarks),
#' by far the most time consuming (and less reproducible) but possibly the best one too
#' when alignment is too tricky to automate.
#' You can also try Procrustes alignment (see fgProcrustes) (<- todo link 4 here) through their calliper
#' length (see coo_aligncalliper), etc. You should also make the first
#' point homologous either with coo_slide or coo_slidedirection
#' to minimize any subsequent problems.
#'
#' I will dedicate (some day) a vignette or a paper to this problem.

#' @references Claude, J. (2008) __Morphometrics with R__, Use R! series,
#' Springer 316 pp.
#' Ferson S, Rohlf FJ, Koehn RK. 1985. Measuring shape variation of
#' two-dimensional outlines. __Systematic Biology__ **34**: 59-68.
#'
#' @family efourier
#' @family morphometrics
#'
#' @examples
#' bot %>% pick() %>% efourier(4) %>% print() %>% efourier_i()
#' bot$coo[1:2] %>% efourier(4) %>% print() %>% efourier_i() %>% class()
#' bot[1:3, ] %>% efourier(4) %>% efourier_norm()
#'
#' @export
efourier <- function(x,  ...) {
  UseMethod("efourier")
}

#' @export
#' @rdname efourier
efourier.default <- function(x, nb_h=NA, raw=FALSE, ...){
  not_defined("efourier")
}

#' @export
#' @rdname efourier
efourier.coo_single <- function (x, nb_h=NA, raw=FALSE, ...) {
  # x <- validate_coo_single(x)
  # if (coo_is_closed(coo))
  #   coo <- coo_unclose(coo)
  n <- nrow(x)
  nb_h <- .check_efourier_nb_h(x, nb_h=nb_h)

  # directly borrowed from Julien Claude
  # slightly brushed up but fundamentally untouched since Momocs v0
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
  if (raw){
    res
  } else {
    res[c("a", "b", "c", "d")] %>%
      purrr::flatten() %>%
      dplyr::bind_cols() %>%
      coe_single() %>%
      .append_class("efourier_single")
  }
}

#' @export
#' @rdname efourier
efourier.coo_list <- function (x, nb_h=NA, ...) {
  nb_h <- .check_efourier_nb_h(x, nb_h=nb_h)

  x %>%
    purrr::map(efourier, nb_h=nb_h, raw=FALSE) %>%
    new_coe_list() %>%
    .append_class("efourier")
}


#' @export
#' @rdname efourier
efourier.mom_tbl <- function(x, nb_h=NA, keep_coo=FALSE, ...){
  nb_h <- .check_efourier_nb_h(x, nb_h=nb_h)

  # !!! handles coo column
  # map efourier
  res <- x %>% dplyr::mutate(coe=.data$coo %>%
                               efourier(nb_h, raw=FALSE) %>%
                               .append_class("efourier"))

  # drop or dont drop coo and add class labels
  if (keep_coo){
    res <- res
  } else {
    res <- res %>% coo_drop()
  }
  # return this beauty
  res
}

# efourier_i ----------------------------------------------
#' @export
#' @describeIn  efourier inverse efourier function
efourier_i <- function(x, nb_h=NA, nb_pts = 120){
  UseMethod("efourier_i")
}

# may have been .coo_single but suceptible to arrive as numeric
#' @export
efourier_i.default <- function (x, nb_h=NA, nb_pts = 120) { # efourier_i.efourier_single ?
  if (is.data.frame(x))
    x <- x %>% unlist %>% .coeff_split()

  if (is.numeric(x))
    x <- x %>% .coeff_split()

  if (is.null(x$ao))
    ao <- 0
  if (is.null(x$co))
    co <- 0

  # regular theta seq
  theta <- seq(0, 2 * pi, length.out = nb_pts+1)[-(nb_pts+1)]

  # by default use them all
  if (missing(nb_h) | is.na(nb_h))
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

#' @export
efourier_i.coe_list <- function(x, nb_h=NA, nb_pts=120){
  x %>%
    purrr::map(efourier_i, nb_h=nb_h, nb_pts=nb_pts) %>%
    coo_list()
}

#todo handles columns and possibly use _at or _if

#' @export
efourier_i.mom_tbl <- function(x, nb_h=NA, nb_pts=120){
  x$coe %>%
    purrr::map(efourier_i, nb_h=nb_h, nb_pts=nb_pts) %>%
    coo_list() -> res
  dplyr::mutate(x, coe_i=res) %>% new_mom()
}


# efourier norm -------------------------------------------

#' @export
#' @describeIn efourier efourier numerical normalization
efourier_norm <- function(x, ...){
  UseMethod("efourier_norm")
}

#' @export
#' @rdname efourier
efourier_norm.default <- function(x, first_point = FALSE, raw=FALSE, ...) {
  if (is.data.frame(x))
    x <- .coeff_split(x, cph=4)

  A1 <- x$an[1]
  B1 <- x$bn[1]
  C1 <- x$cn[1]
  D1 <- x$dn[1]
  nb.h <- length(x$an)
  theta <- 0.5 * atan(2 * (A1 * B1 + C1 * D1)/(A1^2 + C1^2 -
                                                 B1^2 - D1^2))%%pi
  phaseshift <- matrix(c(cos(theta), sin(theta), -sin(theta),
                         cos(theta)), 2, 2)
  M2 <- matrix(c(A1, C1, B1, D1), 2, 2) %*% phaseshift
  v <- apply(M2^2, 2, sum)
  if (v[1] < v[2]) {
    theta <- theta + pi/2
  }
  theta <- (theta + pi/2)%%pi - pi/2
  Aa <- A1 * cos(theta) + B1 * sin(theta)
  Cc <- C1 * cos(theta) + D1 * sin(theta)
  scale <- sqrt(Aa^2 + Cc^2)
  psi <- atan(Cc/Aa)%%pi
  if (Aa < 0) {
    psi <- psi + pi
  }
  size <- 1/scale
  rotation <- matrix(c(cos(psi), -sin(psi), sin(psi), cos(psi)),
                     2, 2)
  A <- B <- C <- D <- numeric(nb.h)
  if (first_point) {
    theta <- 0
  }
  for (i in 1:nb.h) {
    mat <- size * rotation %*%
      matrix(c(x$an[i], x$cn[i],
               x$bn[i], x$dn[i]), nrow=2, ncol=2) %*%
      matrix(c(cos(i * theta), sin(i * theta),
               -sin(i * theta), cos(i * theta)), nrow=2, ncol=2)
    A[i] <- mat[1, 1]
    B[i] <- mat[1, 2]
    C[i] <- mat[2, 1]
    D[i] <- mat[2, 2]
    lnef <- c(A[i], B[i], C[i], D[i])
  }

  # either return full results (ala Claude)
  if (raw) {
    list(A = A, B = B, C = C, D = D, size = scale, theta = theta,
         psi = psi, ao = x$ao, co = x$co, lnef = lnef,
         first_point = first_point)
  } else {
    # or a coe single
    list(a=A, b=B, c=C, d=D) %>%
      .seq_naming_list() %>%
      purrr::flatten() %>%
      dplyr::bind_cols() %>%
      coe_single() %>%
      .append_class("efourier_single")
  }
}

#' @export
#' @rdname efourier
efourier_norm.coe_list <- function(x, first_point = FALSE, ...){
  purrr::map(x, efourier_norm, first_point=first_point) %>%
    new_coe_list() %>%
    .append_class("efourier")
}

#' @export
#' @rdname efourier
efourier_norm.mom_tbl <- function(x, first_point = FALSE, ...){
  x$coe <- x$coe %>%
    purrr::map(efourier_norm, first_point=first_point) %>%
    new_coe_list() %>%
    .append_class("efourier")
  x
}
