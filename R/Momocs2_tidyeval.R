#' Tidyeval helpers
#'
#' Functions that pack repeated tidyeval behaviour
#'
#' @param from_col column name
#' @param to_col column name
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
#'
#' tidyeval_coo_modifyers()
#' tidyeval_coo_modifyers(plip)
#' tidyeval_coo_modifyers(to_col=plop)
#' tidyeval_coo_modifyers(plip, plop)
#' @export
tidyeval_coo_modifyers <- function(from_col=coo, to_col=coo){

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
