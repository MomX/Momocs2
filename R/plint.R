#' Print, plot and continue the pipe
#'
#' @param x object to plint
#' @param ... additional parameters
#'
#' @export
plint <- function(x, ...){
  UseMethod("plint")
}

#' @export
plint.coo_single <- function(x, ...){
  x %>% g() %>% print()
  x %>% print()
  x
}
