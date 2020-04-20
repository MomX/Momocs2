##### Core functions for polynomials approaches on open outlines

# likely_bookstein ----------------------------------------
# helper method to test if a shape is likely booksteined

likely_bookstein <- function(x){
  # get first and last coordinates
  tips <- x[c(1, nrow(x)), ]
  # expected if booksteined before
  book <- tibble::tibble(x=c(-0.5, 0.5), y=0)
  sum(abs(tips-book)) < 1e-5
}

# set sensible names in the form a0, a1, ..., an
set_names_poly <- function(x){
  names(x) <- paste0("a", 0:(length(x)-1))
  x
}
# npoly ---------------------------------------------------
#' Natural and orthogonal polynomials
#'
#' Calculates natural and orthogonal polynomial coefficients using [stats::lm]
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
#' @param x a matrix (or a list) of (x; y) coordinates
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
#' @family efourier
#' @family morphometrics
#'
#' @examples
#'
#' o <- olea %>% pick(5) %>% coo_bookstein()
#' o %>% gg()
#' op <- o %>% npoly(degree=3)
#' op
#' op %>% class
#'
#' op %>% npoly_i() %>% draw()
#'
#' olea %>% dplyr::slice(1:3) %>%
#'   npoly(drop=FALSE) %>% npoly_i
#' @export
npoly <- function(x, degree, raw, drop_coo, from_coo=coo, to_coe={{coe}}, ...){
  UseMethod("npoly")
}

#' @export
npoly.default <- function(x, ...){
  not_defined("npoly")
}

#' @export
npoly.coo_single <- function(x, degree, raw=TRUE, ...){

  if (missing(degree)) {
    degree <- 5
    .msg_warning("npoly: 'degree' not provided and set to {degree}")
  }

  # consumed by intercept as implemented below
  if (degree < 1)
    stop("npoly: degree must be >= 1")

  y0 <- unlist(x[, 2])
  x0 <- unlist(x[, 1])
  # x <- poly(coo[, 1], degree = degree, raw = TRUE)
  # mod <- lm(coo[, 2] ~ x)
  x_pol <- stats::poly(x0, degree = degree, raw = TRUE) #! not the same 'raw' as the arg
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
    .append_class("npoly_single")

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
npoly.coo_list <- function(x, degree, ...){
  # check that all are booksteined
  if (any(purrr::map_lgl(x, purrr::negate(likely_bookstein)))){
    stop("npoly: your shapes are (likely) not registered on bookstein coordinates. Use coo_bookstein().")
  }

  # check for missing degree here
  if (missing(degree)){
    degree <- 5
    .msg_warning("npoly: 'degree' not provided and set to {degree}")
  }
  # run and return that beauty
  purrr::map(x, npoly, degre=degree) %>%
    coe_list() %>%
    .append_class("npoly")
}

#' @export
npoly.mom_tbl <- function(x, degree, raw, drop_coo=TRUE, from_coo=coo, to_coe=coe, ...){
  # prelim ---
  # stupid but S3 arguments order rule, rules...
  if (provided(raw))
    .msg_info("npoly: `raw` provided but useless here")
  # check for missing degree here
  if (missing(degree)){
    degree <- 5
    .msg_warning("npoly: 'degree' not provided and set to {degree}")
  }

  # tidyeval ---
  from_coo <- enquo(from_coo)
  to_coe   <- enquo(to_coe)

  res <- x %>%
    dplyr::pull(!!from_coo) %>%
    npoly(degree=degree) %>%
    .append_class("npoly")
  res <- dplyr::mutate(x, !!to_coe := res)

  # drop_coo if required and return this beauty
  if (drop_coo)
    dplyr::select(res, -!!from_coo)
  else
    res
}

# npoly_i -------------------------------------------------
#' @describeIn npoly inverse npoly method
#' @export
npoly_i <- function(x, nb_pts){
  UseMethod("npoly_i")
}

#' @export
npoly_i.default <- function(x, nb_pts){
  not_defined("npoly_i")
}

# see efourier_i for the .default here
#' @export
npoly_i.default <- function(x, nb_pts=120, ...){
  intercept <- x[[1]]
  coeffs    <- x[-1]
  # deduce the degree
  degree <- ncol(x) - 1
  # x template
  x_pred <- seq(-0.5, 0.5, length=nb_pts)
  # map over coefficients
  y_pred <- seq_along(coeffs) %>%
    # return their contributions
    purrr::map(~(x_pred^.x)*coeffs[[.x]]) %>%
    # reduce by additionning all of them ("rowwise")
    purrr::reduce(`+`)
  # add back the intercept
  y_pred <- intercept + y_pred
  # return a coo_single
  tibble::tibble(x=x_pred, y=y_pred) %>% coo_single()
}

#' @export
npoly_i.coe_list <- function(x, nb_pts=120, ...){
  purrr::map(x, npoly_i, nb_pts=nb_pts) %>% coo_list()
}

#' @export
npoly_i.mom_tbl <- function(x, nb_pts=120, from_coe=coe, ...){
  from_coe <- enquo(from_coe)
  dplyr::mutate(x,
                "{{from_coe}}_i" := x %>%
                  dplyr::pull(!!from_coe) %>%
                  npoly_i(nb_pts=nb_pts))
}

# calibrate -----------------------------------------------
calibrate_npoly_r2 <- function(x, degree_range){
  UseMethod("calibrate_npoly_r2")
}

calibrate_npoly_r2.default <- function(x, degree_range){
  not_defined("calibrate_npoly_r2")
}

calibrate_npoly_r2.coo_single <- function(x, degree_range){
  if (missing(degree_range)){
    degree_range <- 1:6
    .msg_info("calibrate_npoly_r2: 'degree_range' was missing and set to {degree_range}")
  }
  purrr::map_dbl(degree_range,
                 ~npoly(x, degree=.x, raw=FALSE)$r2)
}

calibrate_npoly_r2.coo_list <- function(x, degree_range){
  if (missing(degree_range)){
    degree_range <- 1:6
    .msg_info("calibrate_npoly_r2: 'degree_range' was missing and set to {degree_range}")
  }

  res <- x %>%
    # calibrate
    purrr::map(calibrate_npoly_r2, degree_range=degree_range) %>%
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

calibrate_npoly_r2.mom_tbl <- function(x, degree_range, from_col=coo){
  if (missing(degree_range)){
    degree_range <- 1:6
    .msg_info("calibrate_npoly_r2: 'degree_range' was missing and set to {degree_range}")
  }
  coo <- enquo(from_col)
  x %>% dplyr::pull(!!coo) %>% calibrate_npoly_r2(degree_range=degree_range)
}

