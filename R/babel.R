
#' Convert between numeric and classes systems
#'
#' Function names should be explicit
#'
#' @param x some object to convert
#' @return converted object
#'
#' @name babel
#'
#' @examples
#' bot2 %>% pick(1) %>% head() %>%
#'   cartesian_2_complex() %T>% print() %>%
#'   complex_2_cartesian()
NULL

#' @describeIn babel complex numbers
#' @export
complex_2_cartesian <- function(x){
  cbind(Re(x), Im(x)) %>% coo_single()
}

#' @describeIn babel complex numbers
#' @export
cartesian_2_complex <- function(x){
  x <- coo_single(x)
  complex(real=x$x, imaginary = x$y)
}
