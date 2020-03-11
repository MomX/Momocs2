#' #' Pick a single coo_df
#' #'
#' #' @param x a `coo_tbl` or a `coo_list`
#' #' @param i `int` which to extract. If not provided, pick one randomly.
#' #' @return a coo_df
#' #' @examples
#' #' bot2 %>% pick(1) %>% g()
#' #' bot2$coo %>% pick(2)
#' #' @export
#' pick <- function(x, i){
#'   UseMethod("pick")
#' }
#'
#' pick.default <- function(x, i){
#'   .msg_warning("do not know how to pick on this class")
#' }
#'
#' pick.coo_tbl <- function(x, i){
#'   if (missing(i))
#'     i <- sample(nrow(x), 1)
#'   x$coo[[i]]
#' }
#'
#' pick.coo_list <- function(x, i){
#'   if (missing(i))
#'     i <- sample(length(x), 1)
#'   x[[i]]
#' }
