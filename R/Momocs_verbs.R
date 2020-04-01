# pick ----------------------------------------------------
#' Pick a single coo_df from a coo_tbl
#'
#' @param x a `coo_tbl` or a `coo_list`
#' @param i `int` which to extract. If not provided, pick one randomly.
#' @return a coo_df
#' @note Used to be `[[` in Momocs < 2.0 (eg `bot[[1]]` is replace with `bot %>% pick(1)`)
#' @examples
#' bot2 %>% pick(1) %>% gg()
#' bot2$coo %>% pick(2)
#' @export
pick <- function(x, i){
  UseMethod("pick")
}

#' @export
pick.default <- function(x, i){
  .msg_warning("pick: do not know how to pick on this class")
}

# #' @export
# pick.list <- function(x, i){
#   if (missing(i))
#     i <- sample(length(x), 1)
#   x[[i]]
# }

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


# unfold --------------------------------------------------
#' Unfold list columns
#'
#' Unfold list columns such as [coo_list] and [coe_list], repeat lines if required.
#'
#' @param x a Momocs object
#' @param ... useless
#' @note Differs from `tidyr::unnest`, in that it adds a "shp" column.
#' Also `unpack` was already take by `tidyr`. `unfold.list` is just `dplyr::bind_rows`.
#' @examples
#'
#' bot2$coo %>% unfold
#'
#' bot2 %>% unfold
#' @export
unfold <- function(x, ...){
  UseMethod("unfold")
}

#' @export
unfold.default <- function(x, ...){
  .msg_info("unfold: no unfold method for this class")
}

#' @export
unfold.list <- function(x, ...){
  dplyr::bind_rows(x)
}

# todo only a single coo
#' @export
unfold.coo_tbl <- function(x, ...){
  # how many times do we have to repeat each line
  # ie number of rows for each coo_tbl
  reps <- rep(1:nrow(x), times=purrr::map_dbl(x$coo, nrow))
  # unpack coo, bind cols with others
  dplyr::bind_cols(
    tibble::tibble(shp=reps),
    x$coo %>% unfold(),
    x %>% dplyr::select(-coo) %>% dplyr::slice(reps)
  )
}

#' @export
unfold.coe_tbl <- function(x, ...){
  dplyr::bind_cols(
    x %>% coe_drop(),
    x %>%
      coe_only() %>%
      prefix_col() %>%
      purrr::map(unfold) %>%
      dplyr::bind_cols()
  )
}



