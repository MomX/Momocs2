# pick ----------------------------------------------------
#' Pick a single coo_df
#'
#' @param x a `coo_tbl` or a `coo_list`
#' @param i `int` which to extract. If not provided, pick one randomly.
#' @return a coo_df
#' @examples
#' bot2 %>% pick(1) %>% g()
#' bot2$coo %>% pick(2)
#' @export
pick <- function(x, i){
  UseMethod("pick")
}

#' @export
pick.default <- function(x, i){
  .msg_warning("do not know how to pick on this class")
}

#' @export
pick.coo_tbl <- function(x, i){
  if (missing(i))
    i <- sample(nrow(x), 1)
  x$coo[[i]]
}

#' @export
pick.coo_list <- function(x, i){
  if (missing(i))
    i <- sample(length(x), 1)
  x[[i]]
}

# plint ---------------------------------------------------
#' Plot, print and pipe forward
#'
#' Within a pipe, will take left hand side and:
#' i)  produce the default graphics (using [gg]),
#' ii) print the object  (using [print]) and
#' iii) forward in untouched to right hand side of the pipe.
#' @param x object to plint
#' @param ... additional parameters
#'
#' @return what was passed to it (`x`), untouched. Side effects are plotting and printing
#'
#' @export
plint <- function(x, ...){
  UseMethod("plint")
}

#' @export
plint.coo_single <- function(x, ...){
  x %>% gg(...) %>% print()
  x %>% print()
  x
}
