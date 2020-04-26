# pick ----------------------------------------------------
#' Pick a single coo_df from a mom_tbl
#'
#' @param x a `mom_tbl` or a `coo_list`
#' @param i `int` which to extract. If not provided, pick one randomly.
#' @return a coo_df
#' @note Used to be `[[` in Momocs < 2.0 (eg `bot[[1]]` is replace with `bot %>% pick(1)`)
#' @family mom verbs
#' @seealso [slive]
#' @examples
#' bot %>% pick(1) %>% gg()
#' bot$coo %>% pick(2)
#' @export
pick <- function(x, i){
  UseMethod("pick")
}

#' @export
pick.default <- function(x, i){
  .msg_warning("pick: do not know how to pick on this class")
}

#' @export
# pick.list <- function(x, i){
#   if (missing(i))
#     i <- sample(length(x), 1)
#   x[[i]]
# }
# todo handle coo
# todo handle coe
#' @export
pick.mom_tbl <- function(x, i){
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


# slive ----------------------------------------------------

# pick a single row from a tibble
# https://english.stackexchange.com/questions/20948/single-word-for-thin-slice

#' Pick a single row from a tbl
#'
#' Just a wrapper around `[` (so a raw [dplyr::slice] that also add the random aspect.
#'
#' @param x a `mom_tbl` (or a [tibble::tibble] or a [data.frame])
#' @param i `int` which to extract. If not provided, pick one randomly.
#' @return a [mom_tbl] (or a [tibble::tibble] or a [data.frame])
#' @family mom verbs
#' @seealso [pick]
#' @examples
#' bot %>% slive(1)
#' @export
slive <- function(x, i){
  UseMethod("slive")
}

#' @export
slive.default <- function(x, i){
  .msg_info("slive: not method defined for this class")
}

#' @export
slive.data.frame <- function(x, i){
  if (missing(i))
    i <- sample(nrow(x), 1)
  dplyr::slice(x, i)
}


# plint ---------------------------------------------------
# todo good idea that waits for more graphs
# #' Plot, print and pipe forward
# #'
# #' Within a pipe, will take left hand side and:
# #' i)  produce the default graphics (using [gg]),
# #' ii) print the object  (using [print]) and
# #' iii) forward in untouched to right hand side of the pipe.
# #' @param x object to plint
# #' @param ... additional parameters
# #'
# #' @return what was passed to it (`x`), untouched. Side effects are plotting and printing
# #' @family mom verbs
# #' @export
# plint <- function(x, ...){
#   UseMethod("plint")
# }

##' @export
# plint.coo_single <- function(x, ...){
#   x %>% gg(...) %>% print()
#   x %>% print()
#   x
# }

# press  --------------------------------------------------
# todo prefix within
# Given coe_tbl with 1 or mroe coe_list columns
# Prefix individual coe_tbl colnames composing them with coe_list column name
# See examples

.prefix_columns <- function(x, str){
  purrr::set_names(x, paste0(str, "_", colnames(x)))
}

#' Flatten coe list columns
#'
#' Take all coe list columns, prefix them with list column names and return a tidy tibble.
#' This function is typically used for analyses, after morphometrics.
#'
#' @param x [mom_tbl]
#' @param keep_others logical whether to return other columns (`TRUE` and default`) or only coe
#' @return [tibble::tibble]
#' @family mom verbs
#' @seealso [unfold] for a related reshaping
#' @examples
#' iris %>% mom %>% press
#'
#' x <- bot %>% efourier(4)
#' x$coe2 <- bot$coo %>% efourier(6)
#' x %>% press
#'
#' x %>% press(keep_others=FALSE) # eg for PCA
#'
#' @export
press <- function(x, keep_others){
  UseMethod("press")
}

#' @export
press.default <- function(x, keep_others){
  .msg_info("press only handles mom_tbl objects")
}

#' @export
press.mom_tbl <- function(x, keep_others=TRUE){
  # early return and forward if no coe
  if (coe_nb(x)==0){
    .msg_info("press: found no coe; forward")
    return(x)
  }

  # take the coe columns, and get their names
  purrr::map2(coe_only(x), coe_names(x),
              # turn into list (should not be need todo vctrs)
              # bind rows and prefix with partition name
              ~.x %>% as.list %>% dplyr::bind_rows() %>% .prefix_columns(.y) %>% as_tibble) %>%
    dplyr::bind_cols() -> res

  # if required, add to the left of original non coe columns
  if (keep_others)
    res <- dplyr::bind_cols(coe_drop(x) %>% tibble::as_tibble(), res)

  # and return this beauty
  res
}

# unfold --------------------------------------------------
# todo replace group for .group everywhere so that we minimize conflict
#  with any col named group

#' Unfold list columns
#'
#' Unfold list columns such as [coo_list] and [coe_list], repeat lines if required.
#'
#' @param x a Momocs object
#' @param from_col column name
#' @param ... useless
#' @note Differs from `tidyr::unnest`, in that it adds a "shp" column.
#' Also `unpack` was already take by `tidyr`. `unfold.list` is just `dplyr::bind_rows`.
#' @seealso [press] for a related reshaping
#' @family mom verbs
#' @examples
#'
#' bot$coo %>% unfold
#'
#' bot %>% unfold
#' @export
unfold <- function(x, from_col, ...){
  UseMethod("unfold")
}

#' @export
unfold.default <- function(x, ...){
  .msg_info("unfold: no unfold method for this class")
}

#' @export
unfold.coo_single <- function(x, ...){
  x %>% dplyr::mutate(group=1) %>% tibble::as_tibble()
}

# useful below when x is already unfolded
#'@export
unfold.tbl <- function(x, ...){
  if ("group" %in% colnames(x))
    return(x)
  x %>% dplyr::mutate(group=1)
}

#' @export
unfold.coo_list <- function(x, ...){
  # dplyr::bind_rows(x, .id="group") wouldnt work since we want numeric
  reps <- get_nb(x)

  coords <- x %>% as.list() %>% dplyr::bind_rows() %>% tibble::as_tibble()
  group  <- tibble::tibble(group=purrr::map2(seq_along(reps), reps, ~rep(.x, .y)) %>% unlist())
  dplyr::bind_cols(
    coords,
    group) %>%
    tibble::as_tibble()
}

# should be unfold_coo
# and unfold_coe
# was coo below
#' @export
unfold.mom_tbl <- function(x, from_col, ...){
  # check a bit for coo_list columns
  if (coo_nb(x) == 0)
    stop("unfold.mom_tbl: no coo_list column")


  if (missing(from_col)){
    from_col <- coo_names(x)[1]
    .msg_info("unfold.mom_tbl: can only handle a single coo_list, using {from_col} column")
  }
  from_col <- enquo(from_col)

  # there is probably a more elegant way..
  # times we have to repeat each line
  reps_n   <- x %>% dplyr::pull(!!from_col) %>% get_nb()
  # same as a vector
  group    <- purrr::map2(seq_along(reps_n), reps_n, ~rep(.x, .y)) %>% unlist()


  # unfold from_col, bind cols with others
  dplyr::bind_cols(
    x %>% dplyr::pull(!!from_col) %>% unfold,
    x %>% coo_drop() %>% dplyr::slice(group) %>% tibble::as_tibble()
  )
}

# was coe below
# #' @export
# unfold.coe_tbl <- function(x, ...){
#   dplyr::bind_cols(
#     x %>% coe_drop(),
#     x %>%
#       coe_only() %>%
#       prefix_col() %>%
#       purrr::map(unfold) %>%
#       dplyr::bind_cols()
#   )
# }




# list col verbs --------------------------------------
#' List columns verbs
#'
#' @param x [mom_tbl]
#' @param coo,coe column names to test for existence
#' @param col column names to test for existence
#' @param ... additional parameters, just like in `dplyr::select`
#' @name list_columns
#' @examples
#'
#' bot %>% coo_nb()
#' bot %>% coo_names()
#' bot %>% coo_present(coo)
#' bot %>% coo_present(foo)  # non existing column
#' bot %>% coo_only()
#' bot %>% coo_drop()
#' bot$coo2 <- bot$coo
#' bot %>% coo_select(coo2)
#'
#' hearts %>% col_present(ldk)
#'
#' # and works the same for coe columns
NULL

# *_exist
#' @export
#' @describeIn list_columns test coo columns existence
col_present <- function(x, col){
  as_name(enquo(col)) %in% names(x)
}

#' @export
#' @describeIn list_columns test coo columns existence
coo_present <- function(x, coo){
  as_name(enquo(coo)) %in% coo_names(x)
}

#' @export
#' @describeIn list_columns test coe columns existence
coe_present <- function(x, coe){
  as_name(enquo(coe)) %in% coe_names(x)
}


# *_nb -------
#' @export
#' @describeIn list_columns count the number of coo columns
coo_nb <- function(x){
  x %>% purrr::map_lgl(is_coo_list) %>% sum()
}

#' @export
#' @describeIn list_columns count the number of coe columns
coe_nb <- function(x){
  x %>% purrr::map_lgl(is_coe_list) %>% sum()
}

# *_only -------
#' @export
#' @describeIn list_columns only retain coo columns
coo_only <- function(x){
  dplyr::select_if(x, is_coo_list)
}

#' @export
#' @describeIn list_columns only retain coe columns
coe_only <- function(x){
  dplyr::select_if(x, is_coe_list)
}

# *_drop -------
#' @export
#' @describeIn list_columns drop coo columns
coo_drop <- function(x){
  dplyr::select_if(x, purrr::negate(is_coo_list))
}

#' @export
#' @describeIn list_columns drop coe columns
coe_drop <- function(x){
  dplyr::select_if(x, purrr::negate(is_coe_list))
}

# *_select -------
#' @export
#' @describeIn list_columns use select among coo columns
coo_select <- function(x, ...){
  y <- x %>% coo_drop()
  x %>% dplyr::select(!!!enquos(...)) %>%
    dplyr::bind_cols(y)
}

#' @export
#' @describeIn list_columns use select among coe columns
coe_select <- function(x, ...){
  y <- x %>% coe_drop()
  x %>% dplyr::select(!!!enquos(...)) %>%
    dplyr::bind_cols(y)
}

# *_names -------
#' @export
#' @describeIn list_columns return names of coo columns
coo_names <- function(x){
  coo_only(x) %>% names()
}

#' @export
#' @describeIn list_columns return names of coe columns
coe_names <- function(x){
  coe_only(x) %>% names()
}


