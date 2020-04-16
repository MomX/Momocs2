#' Tidyeval helpers
#'
#' Pack repeated tidyeval behaviour.
#' Expose to ease development (todo extending vignettes, todo everything here)
#'
#' @param from_col column name
#' @param to_col column name
#' @param ldk_col column name
#'
#' @return [closure], for delayer evaluation
#'
#' @details
#'  * tidyeval_coo_modifyers: used in `coo_*` modifyers.
#'    * Default to `coo` for both `from_col` and `to_col`.
#'    * If `from_col` is passed but not `to_col`, the latter also uses  `from_col`.
#'    This is where the _modifying_ behaviour is reflected.
#'
#' @examples
#' #tidyeval_coo_modifyers()
#' #tidyeval_coo_modifyers(plip)
#' #tidyeval_coo_modifyers(to_col=plop)
#' #tidyeval_coo_modifyers(plip, plop)
#'
#' @name tidyeval_helpers
NULL

#' @describeIn tidyeval_helpers coo_ helpers
#' @export
tidyeval_coo_modifyers <- function(from_col, to_col){
  # missing or not, enquote from_col
  from_col <- enquo(from_col)

  # if missing to_col is the same column
  # if provided, use it
  if (missing(to_col))
    to_col <- from_col
  else
    to_col <- enquo(to_col)

  # return these (promised) beauties
  list(from_col=from_col, to_col=to_col)
}

#' @describeIn tidyeval_helpers ldk helpers
#' @export
tidyeval_coo_and_ldk <- function(from_col, ldk_col){
  # missing or not, enquote both
  # and return these (promised) beauties
  list(from_col = enquo(from_col),
       ldk_col  = enquo(ldk_col))
}
