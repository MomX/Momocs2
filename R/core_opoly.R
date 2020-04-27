# OPOLY ===================================================
# npoly ---------------------------------------------------
#' Orthogonal polynomials
#'
#' Calculates orthogonal polynomial coefficients using [stats::lm]
#'
#' @details
#'
#' Curves must be registered on a bookstein baseline, with the first coordinates on (-0.5, 0)
#' and the last on (0.5, 0). Use [coo_bookstein]
#'
#' A polynomial of degree `n` use this fit:
#'
#' \eqn{y = a_{0} + a_{1}x^1 + a_{...}x^{...} + a_{n}x^{n}}
#'
#' And thus returns `n+1` coefficients. The `+1` being $a_0$  ie the intercept.
#'
#' Orthogonal polynomials are also called Legendre's polynomials. They are
#' preferred over natural polynomials since adding a degree does not
#' change lower order coefficients.
#'
#' In retired Momocs, baseline was free and not necessarily Bookstein. Not sure this
#' was really helpful but I'm sure this overcomplicated coding. If your shapes are not
#' registered on Bookstein coordinates, you will be messaged (not for [coo_single] though)
#'
#' @param x [coo_single], [coo_list] or [mom_tbl]
#' @param degree polynomial degree for the fit (`degree+1`) coefficients are returned, see Details)
#' @param raw `logical` whether to return raw and full results
#' @param nb_pts number of points to sample and on which to calculate polynomials
#' @param drop_coo `logical` whether to drop coo column (default to TRUE)
#' @param from_coo,to_coe column names
#' @param from_coe column name for inverse method
#' @param ... for generics. Useless here.
#' @return a list with components when applied on a single shape:
#' \itemize{
#'  \item \code{coeff} the coefficients (including the intercept)
#'  \item \code{ortho} whether orthogonal or natural polynomials were fitted
#'  \item \code{degree} degree of the fit (could be retrieved through \code{coeff} though)
#'  \item \code{baseline1} the first baseline point (so far the first point)
#'  \item \code{baseline2} the second baseline point (so far the last point)
#'  \item \code{r2} the r2 from the fit
#'  \item \code{mod} the raw lm model
#' }
#'
#' @family polynomials
#' @family morphometrics
#'
#' @examples
#'
#' o <- olea %>% pick(5) %>% coo_bookstein()
#' o %>% pile()
#' op <- o %>% opoly(degree=3)
#' op
#' op %>% class
#'
#' op %>% opoly_i() %>% draw()
#'
#' olea %>% dplyr::slice(1:3) %>%
#'   opoly(drop=FALSE) %>% npoly_i
#' @export
opoly <- function(x, degree, raw, drop_coo, from_coo=coo, to_coe={{coe}}, ...){
  UseMethod("opoly")
}

#' @export
opoly.default <- function(x, ...){
  not_defined("opoly")
}

#' @export
opoly.coo_single <- function(x, degree, raw=TRUE, ...){

  if (missing(degree)) {
    degree <- 5
    .msg_warning("opoly: 'degree' not provided and set to {degree}")
  }

  # consumed by intercept as implemented below
  if (degree < 1)
    stop("opoly: degree must be >= 1")

  y0 <- unlist(x[, 2])
  x0 <- unlist(x[, 1])
  # x <- poly(coo[, 1], degree = degree, raw = TRUE)
  # mod <- lm(coo[, 2] ~ x)
  x_pol <- stats::poly(x0, degree = degree, raw = FALSE) #! not the same 'raw' as the arg
  mod <- stats::lm(y0 ~ x_pol)

  # prepare coe, get them
  res <-  mod$coefficients %>%
    # rename
    set_names_poly() %>%
    # turn into a tibble
    list() %>%
    purrr::flatten() %>%
    dplyr::bind_cols() %>%
    # append classes
    coe_single() %>%
    .append_class("opoly_single")

  # early return if we just want the coe
  if (raw)
    return(res)
  # otherwise return details

  list(coe    = res,
       r2     = summary(mod)$r.squared,
       AIC    = stats::AIC(mod),
       mod    = mod,
       orthod = FALSE)   # a flag for o/n poly
}

#' @export
opoly.coo_list <- function(x, degree, ...){
  # check that all are booksteined
  if (any(purrr::map_lgl(x, purrr::negate(likely_bookstein)))){
    stop("opoly: your shapes are (likely) not registered on bookstein coordinates. Use coo_bookstein().")
  }

  # check for missing degree here
  if (missing(degree)){
    degree <- 5
    .msg_warning("opoly: 'degree' not provided and set to {degree}")
  }
  # run and return that beauty
  purrr::map(x, opoly, degre=degree) %>%
    coe_list() %>%
    .append_class("opoly")
}

#' @export
opoly.mom_tbl <- function(x, degree, raw, drop_coo=TRUE, from_coo=coo, to_coe=coe, ...){
  # prelim ---
  # stupid but S3 arguments order rule, rules...
  if (provided(raw))
    .msg_info("opoly: `raw` provided but useless here")
  # check for missing degree here
  if (missing(degree)){
    degree <- 5
    .msg_warning("opoly: 'degree' not provided and set to {degree}")
  }

  # tidyeval ---
  from_coo <- enquo(from_coo)
  to_coe   <- enquo(to_coe)

  res <- x %>%
    dplyr::pull(!!from_coo) %>%
    opoly(degree=degree) %>%
    .append_class("opoly")
  res <- dplyr::mutate(x, !!to_coe := res)

  # drop_coo if required and return this beauty
  if (drop_coo)
    dplyr::select(res, -!!from_coo)
  else
    res
}

# opoly_i -------------------------------------------------
#' @describeIn opoly inverse opoly method
#' @export
opoly_i <- function(x, nb_pts, from_coe, ...){
  UseMethod("opoly_i")
}

# see efourier_i for the .default here
#' @export
opoly_i.default <- function(x, nb_pts=120, ...){
  # domestic inherited from Momocs
  .mprod <- function(m, s) {
    res <- m
    for (i in 1:ncol(m)) {
      res[, i] <- m[, i] * s[i]
    }
    res
  }
  # extract
  intercept <- x[[1]]
  coeffs    <- x[-1]
  # deduce the degree
  degree <- ncol(x) - 1
  # x template
  x_new <- seq(-0.5, 0.5, length=nb_pts)
  x_poly <- stats::poly(x_new, degree=degree)
  y_pred <- stats::predict(x_poly, x_new)
  # here is the calculation
  y_new <- intercept + rowSums(.mprod(m = y_pred, s = unlist(coeffs)))
  # return a coo_single
  tibble::tibble(x=x_new, y=y_new) %>% coo_single()
}

#' @export
opoly_i.coe_list <- function(x, nb_pts=120, ...){
  purrr::map(x, opoly_i, nb_pts=nb_pts) %>% coo_list()
}

#' @export
opoly_i.mom_tbl <- function(x, nb_pts=120, from_coe=coe, ...){
  from_coe <- enquo(from_coe)
  dplyr::mutate(x,
                "{{from_coe}}_i" := x %>%
                  dplyr::pull(!!from_coe) %>%
                  opoly_i(nb_pts=nb_pts))
}

# calibrate -----------------------------------------------
calibrate_opoly_r2 <- function(x, degree_range){
  UseMethod("calibrate_opoly_r2")
}

calibrate_opoly_r2.default <- function(x, degree_range){
  not_defined("calibrate_opoly_r2")
}

calibrate_opoly_r2.coo_single <- function(x, degree_range){
  if (missing(degree_range)){
    degree_range <- 1:6
    .msg_info("calibrate_opoly_r2: 'degree_range' was missing and set to {degree_range}")
  }
  purrr::map_dbl(degree_range,
                 ~opoly(x, degree=.x, raw=FALSE)$r2)
}

calibrate_opoly_r2.coo_list <- function(x, degree_range){
  if (missing(degree_range)){
    degree_range <- 1:6
    .msg_info("calibrate_opoly_r2: 'degree_range' was missing and set to {degree_range}")
  }

  res <- x %>%
    # calibrate
    purrr::map(calibrate_opoly_r2, degree_range=degree_range) %>%
    # turn into a data.frame
    do.call("rbind", .) %>%
    # rename cols before tibble
    `colnames<-`(paste0("degree", degree_range)) %>%
    tibble::as_tibble() %>%
    # add group name and pivot longer
    dplyr::mutate(shp=paste0("shp", 1:dplyr::n())) %>%
    tidyr::pivot_longer(cols=tidyselect::starts_with("degree"),
                        names_to  = "degree",
                        values_to = "r2" )

  # summary here?
  # return this beauty
  return(res)

  # x %>%
  #   # dplyr::filter(degree != "degree1") %>%
  #   dplyr::group_by(.data$degree) %>%
  #   dplyr::mutate(min_r2=min(.data$r2),
  #                 median_r2=median(.data$r2),
  #                 max_r2=max(.data$r2)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(degree=stringr::str_remove(degree, "degree") %>% as.numeric) %>%
  #   ggplot() +
  #   aes(x=degree) +
  #   geom_ribbon(aes(ymin = min_r2, ymax = max_r2), fill = "grey70", alpha=0.5) +
  #   geom_line(aes(y = median_r2), alpha=0.5) +
  #   theme_minimal() +
  #   labs(x="degree", y=expression(r^2))
}

calibrate_opoly_r2.mom_tbl <- function(x, degree_range, from_col=coo){
  if (missing(degree_range)){
    degree_range <- 1:6
    .msg_info("calibrate_opoly_r2: 'degree_range' was missing and set to {degree_range}")
  }
  coo <- enquo(from_col)
  x %>% dplyr::pull(!!coo) %>% calibrate_opoly_r2(degree_range=degree_range)
}

# opoly_calibrate_bla
