# MOM -----------------------------------------------------

#' Declare MomX tibble
#'
#' The main class used accros MomX ecosystem. Basically, a tibble with benefits.
#' @param x a tibble that may contain column lists
#'
#' @name mom_tbl
#' @aliases mom
NULL

# creator -----
#' @export
#' @describeIn mom_tbl Constructor
new_mom <- function(x=tibble()){
  # vec_assert(x, tibble())
  x <- tibble::tibble(x)
  tibble::new_tibble(x, nrow=nrow(x), class="mom_tbl")
}

# validator (todo) -----

# workers -----
#' @export
#' @describeIn mom_tbl Helpers
mom <- function(x){
  UseMethod("mom")
}

#' @export
mom.default <- function(x){
  .msg_info("mom: no method defined on this class")
}

#' @export
mom.coo_single <- function(x){
  x %>% coo_single() %>% coo_list() %>% tibble::tibble(coo=.) %>% new_mom()
}

#' @export
mom.tbl <- function(x){
  new_mom(x)
}

#' @export
mom.data.frame <- function(x){
  new_mom(x)
}

#' @export
mom.coo_list <- function(x){
  x %>% coo_list() %>% tibble::tibble(coo=.) %>% new_mom()
}

#' @export
mom.coe_list <- function(x){
  x %>% coo_list() %>% tibble::tibble(coo=.) %>% new_mom()
}

#' @export
mom.Coo <- function(x){
  res <- dplyr::bind_cols(
    tibble::tibble(coo=coo_list(x$coo)),
    tibble::as_tibble(x$fac)
  )

  # include $ldk if any
  if (!is.null(x[["ldk"]]) && length(x[["ldk"]])==length(x$coo))
    res <- res %>%
      dplyr::mutate(ldk=x[["ldk"]]) %>%
      dplyr::select(coo, ldk, dplyr::everything())

  # return this beauty
  res %>% new_mom()
}

#' @export
mom_tbl <- mom

# printers -----
#' @export
print.mom_tbl <- function(x, ...){
  NextMethod(print, mom())
  glue::glue(cli::symbol$pointer,
             crayon::bgGreen("mom_tbl")) %>%
    cli::cat_line()

  if (coo_nb(x)>0)
    glue::glue(cli::symbol$pointer,
               crayon::bgBlue("coo_list:"), " ",
               "{coo_names(x)}") %>%
    cli::cat_line()

  if (coe_nb(x)>0)
    glue::glue(cli::symbol$pointer,
               crayon::bgBlue("coe_list:"), " ",
               "{coe_names(x)}") %>%
    cli::cat_line()

}

# testers -----
#' @export
#' @describeIn mom_tbl Class tester
is_mom_tbl <- function(x){
  inherits(x, "mom_tbl")
}

#' @export
#' @describeIn mom_tbl Class1 tester
is_mom_tbl1 <- function(x){
  class(x)[1]=="mom_tbl"
}


# COO_SINGLE -----------------------------------------------------

#' Create single shape made of coordinates
#'
#' `coo_single` objects are [tibble][tibble::tibble-package] with _exactly_ two columns named `x` and `y`.
#' Anything that can be turned  should work.
#' @param x anything that can be turned a tibble by `tibble::tibble()` should work
#' @details You should use the helper `coo_single`, `new_coo_single` and `validate_coo_single` are for internal operations.
#' @return a `coo_single` object
#' @examples
#' coo_single()
#' matrix(1:12, ncol=2) %>% coo_single()
#' list(x=1:2, y=3:4) %>% coo_single()
#' @name coo_single
NULL

# creator -----
#' @export
#' @describeIn coo_single Constructor
# constructor
new_coo_single <- function(x=tibble::tibble(x=double(), y=double())){
  # vec_assert(x, tibble())
  tibble::new_tibble(x, nrow=nrow(x), class="coo_single")
}

# validator -----
#' @export
#' @describeIn coo_single Validator
validate_coo_single <- function(x){
  res <- list()

  if (ncol(x)!=2)
    res <- append(res, "validate_coo_single: must have two columns named 'x' and 'y'")

  if (!identical(colnames(x), c("x", "y")))
    res <- append(res, "validate_coo_single: must have two columns named 'x' and 'y'")

  if (sum(is.na(x))>0)
    res <- append(res, "validate_coo_single: must not have NAs")

  # if (nrow(x)<3)
  #   res <- append(res, "validate_coo_single: must have more than two points")

  if (length(res)>0){
    purrr::walk(res, cli::cli_alert_danger)
    .msg_info("coo_single() is malformed, use coo_single()")
  } else {
    x
  }
}

# workers -----
#' @export
#' @describeIn coo_single Helper
coo_single <- function(x){
  UseMethod("coo_single")
}

#' @export
coo_single.default <- function(x=new_coo_single()) {
  x %>%
    # cast to tibble
    tibble::as_tibble() %>%
    `colnames<-`(c("x", "y")) %>%
    # construct
    new_coo_single %>%
    # validate
    validate_coo_single()
}

#' @export
coo_single.matrix <- function(x){
  x[, 1:2, drop=FALSE] %>%
    `colnames<-`(c("x", "y")) %>%
    tibble::as_tibble() %>%
    new_coo_single() %>%
    validate_coo_single()
}

# printers -----
#' @export
print.coo_single <- function(x, ...){
  NextMethod(print, coo_single())
  glue::glue(cli::symbol$pointer,
             crayon::bgBlue("coo_single"), " ",
             "with {nrow(x)} coordinates") %>%
    cli::cat_line()
}

# testers -----
#' @export
#' @describeIn coo_single Class tester
is_coo_single <- function(x){
  inherits(x, "coo_single")
}

#' @export
#' @describeIn coo_single Class1 tester
is_coo_single1 <- function(x){
  class(x)[1]=="coo_single"
}

#' @export
plot.coo_single <- function(x, ...){
  gg(x, ...)
}

# COO_LIST ------------------------------------------------

#' Create list of coo_single
#'
#' `coo_list` objects are [list] of [coo_single].
#' @param x a list of [coo_single]
#' @details They behave like lists
#' and thus as regular [tibble]/[data.frame] columns.
#' The only difference is that and are understood by morphometric methods used in MomX.
#' @return a `coo_list` object
#' @examples
#' matrix(1:12, ncol=2) %>% coo_single() %>% list() %>% coo_list()
#' @name coo_list
NULL

# creator -----
#' @export
#' @describeIn coo_list Constructor
new_coo_list <- function(x=list()){
  vctrs::new_list_of(x,
              ptype = new_coo_single(),
              class = c("coo_list", "list"))
}

# workers -----
#' @export
#' @describeIn coo_list Helper
coo_list <- function(x){
  UseMethod("coo_list")
}

#' @export
coo_list.default <- function(x){
  .msg_info("coo_list: not defined on this class")
}

#' @export
coo_list.coo_single <- function(x){
  x %>% coo_single %>% list %>% new_coo_list()
}

#' @export
coo_list.list <- function(x){
  x %>% purrr::map(coo_single) %>% new_coo_list()
}

#' @export
coo_list.coo_list <- function(x){
  x %>% new_coo_list()
}

# validator (todo) -----

# printers -----
#' @rdname coo_list
#' @export
vec_ptype_full.coo_list <- function(x) "coo_list"
#' @rdname coo_list
#' @export
vec_ptype_abbr.coo_list <- function(x) "coo_list"

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.coo_list <- function(x) TRUE

# testers -----
#' @export
#' @describeIn coo_list Class tester
is_coo_list <- function(x){
  inherits(x, "coo_list")
}

#' @export
#' @describeIn coo_list Class1 tester
is_coo_list1 <- function(x){
  class(x)[1]=="coo_list"
}


# COE_SINGLE -----------------------------------------------------

#' Create single coefficients
#'
#' `coe` objects are [tibble][tibble::tibble-package] with as many rows as created by morphometric methods.
#' It is not really intended to be used directly but is useful if you want to extend Momocs.
#' @name coe_single
#' @param x anything that can be turned into a coe_single, typically something that looks like a
#'  two columns
#' @return a `coe_single` object
#' @examples
#' matrix(1:12, nrow=1) %>% coe_single()
NULL

# creator -----
#' @export
#' @describeIn coe_single Constructor
# constructor
new_coe_single <- function(x=tibble::tibble()){
  # vec_assert(x, tibble())
  tibble::new_tibble(x, nrow=nrow(x), class="coe_single")
}

# validator (todo) -----
# # @export
# # @describeIn coo_single Validator
# validate_coo_single <- function(x){
#   res <- list()
#
#   if (ncol(x)!=2)
#     res <- append(res, "validate_coo_single: must have two columns named 'x' and 'y'")
#
#   if (!identical(colnames(x), c("x", "y")))
#     res <- append(res, "validate_coo_single: must have two columns named 'x' and 'y'")
#
#   if (sum(is.na(x))>0)
#     res <- append(res, "validate_coo_single: must not have NAs")
#
#   # if (nrow(x)<3)
#   #   res <- append(res, "validate_coo_single: must have more than two points")
#
#   if (length(res)>0){
#     purrr::walk(res, cli::cli_alert_danger)
#     .msg_info("coo_single() is malformed, use coo_single()")
#   } else {
#     x
#   }
# }

# workers -----
#' @export
#' @describeIn coe_single Helper
coe_single <- function(x=new_coe_single()) {
  x %>%
    # cast to tibble
    tibble::as_tibble() %>%
    # `colnames<-`(c("x", "y")) %>%
    # construct
    new_coe_single #%>%
    # validate
    #validate_coo_single()
}

# printers -----
#' @export
print.coe_single <- function(x, ...){
  NextMethod(print, coe_single())
  glue::glue(cli::symbol$pointer,
             crayon::bgMagenta("coe_single"), " ",
             "with {ncol(x)} coefficients") %>%
    cli::cat_line()
}

# testers -----
#' @export
#' @describeIn coe_single Class tester
is_coe_single <- function(x){
  x %>% .is_class("coe_single")
}

#' @export
#' @describeIn coe_single Class1 tester
is_coe_single1 <- function(x){
  x %>% .is_class1("coe_single")
}


# COE_LIST ------------------------------------------------

#' Create list of coe_single
#'
#' `coe_list` objects are [list] of [coe_single].
#' @param x a list of [coe_single]
#' @details They behave like lists
#' and thus as regular [tibble]/[data.frame] columns.
#' The only difference is that and are understood by morphometric methods used in MomX.
#' @return a `coe_list` object
#' @examples
#' matrix(1:12, nrow=1) %>% coe_single() %>% coo_list()
#' @name coe_list
NULL

# creator -----
#' @export
#' @describeIn coe_list Constructor
new_coe_list <- function(x=list()){
  vctrs::new_list_of(x,
                     ptype = new_coe_single(),
                     class = c("coe_list", "list"))
}

# validator (todo) -----
# workers -----
#' @export
#' @describeIn coe_list Helper
coe_list <- function(x){
  UseMethod("coe_list")
}

#' @export
coe_list.coe_single <- function(x){
  x %>% coe_single %>% list %>% new_coe_list()
}

#' @export
coe_list.list <- function(x){
  x %>% new_coe_list()
}

#' @export
coe_list.coe_list <- function(x){
  x %>% new_coe_list()
}

# #' @export
# #' @describeIn coe_single Class validator
# validate_coe_single <- function(x){
#   res <- list()
#   if (!is_coe_single(x))
#     res <- append(res, "must be a <coe_single>")
#
#   if (!tibble::is_tibble(x))
#     res <- append(res, "must be a <tbl>")
#
#   if (!is.data.frame(x))
#     res <- append(res, "must be a <data.frame>")
#
#   if (!ncol(x)>0)
#     res <- append(res, "must have at least one column")
#
#   if (is.null(colnames(x)))
#     res <- append(res, "columns must be named")
#
#   if (sum(is.na(x))>0)
#     res <- append(res, "must not have NAs")
#
#   if (nrow(x)>1)
#     res <- append(res, "must have a single row")
#
#   if (length(res)>0){
#     purrr::walk(res, cli::cli_alert_danger)
#     .check(FALSE, "use coe_single()")
#   }
#   return(x)
# }

#' @rdname coe_list
#' @export
vec_ptype_full.coe_list <- function(x) "coe_list"
#' @rdname coe_list
#' @export
vec_ptype_abbr.coe_list <- function(x) "coe_list"

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.coe_list <- function(x) TRUE

# testers -----
#' @export
#' @describeIn coe_list Class tester
is_coe_list <- function(x){
  x %>% .is_class("coe_list")
}

#' @export
#' @describeIn coe_list Class1 tester
is_coe_list1 <- function(x){
  x %>% .is_class1("coe_list")
}

