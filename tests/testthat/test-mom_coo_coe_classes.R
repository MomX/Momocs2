# MOM -----------------------------------------------------
test_that("mom_tbl works", {
  # testing class inheritance ---
  x <- new_mom()
  test_inheritance <- function(x){
    expect_true(inherits(x, "mom_tbl"))
    expect_true(inherits(x, "tbl"))
    expect_true(inherits(x, "data.frame"))
  }
  # testing workers ---
  # not defined
  expect_message(mom_tbl("a"))
  # coo_single
  new_coo_single() %>% mom() %>% test_inheritance()
  # data.frame
  iris %>% mom() %>% test_inheritance()
  # tbl
  iris %>% tibble::as_tibble() %>% mom() %>% test_inheritance()
  # coo_list
  new_coo_list() %>% mom() %>% test_inheritance()
  # coe_list
  new_coe_list() %>% mom() %>% test_inheritance()

  # recreating a Coo
  list(coo=replicate(3, matrix(runif(6), ncol=2), simplify=FALSE),
       fac=iris[1:3, ],
       ldk=list(1, 2, 3)) %>%
    .append_class("Ldk") %>% .append_class("Coo") %>%
    mom() -> foo_Coo

  # check ldk export
  x <- foo_Coo %>% mom()
  expect_true(all(c("coo", "ldk") %in% colnames(x)))
  # and more generally
  foo_Coo %>% mom() %>% test_inheritance()
  # also test the alias
  foo_Coo %>% mom_tbl() %>% test_inheritance()

  # print method
  expect_output(print(bot), "mom_tbl")

  # test what used to fail befor vctrs
  x <- bot %>% dplyr::slice(1:2)
  x %>% test_inheritance()
  expect_is(x$coo, "coo_list")
})


# # COO ------
# coo_single -------

test_that("coo_single constructor works", {

  test_inheritance <- function(x){
    expect_true(inherits(x, "coo_single"))
    expect_true(inherits(x, "tbl"))
    expect_true(inherits(x, "data.frame"))
    expect_true(is_coo_single(x))
    expect_true(is_coo_single1(x))
  }
  # anything that can be coerced by tibble should work
  list(
    matrix(1:2, ncol=2),
    matrix(1:4, ncol=2),
    list(x=1:5, y=5:1),
    matrix(1:2, ncol=2) %>% `colnames<-`(c("x", "y")) %>% as.data.frame(),
    matrix(1:2, ncol=2) %>% as.data.frame(),
    new_coo_single(),
    new_coo_single() %>% coo_single(),
    tibble::tibble(x=1:3, y=3:1),
    tibble::tibble(x=1:3, y=3:1) %>% coo_single(),
    tibble::tibble(x=1:3, y=3:1) %>% `colnames<-`(c("X", "Y"))
  ) %>% purrr::walk(~ .x %>% coo_single %>% test_inheritance)

  expect_message("a" %>% coo_single(), "use coo_single()")
  expect_message(list(x="a") %>% coo_single(), "use coo_single()")
  expect_message(1:2 %>% coo_single(), "use coo_single()")
  expect_message(matrix(c(NA, 2), ncol=2) %>% coo_single(), "use coo_single()")
  expect_message(matrix(c(NA, 2:4), ncol=2) %>% coo_single(), "use coo_single()")
  expect_message(matrix(c(NA, 2), ncol=2) %>% `colnames<-`(c("x", "y")) %>% as.data.frame()%>% coo_single(), "use coo_single()")
  expect_message( matrix(c(NA, 2), ncol=2) %>% as.data.frame() %>% coo_single(), "use coo_single()")
  # expect_message(tibble::tibble(x=c(NA, 2:3), y=3:1) %>% coo_single(), "use coo_single()")
  expect_message(tibble::tibble(x=c(NA, 2:3), y=3:1) %>% coo_single(), "use coo_single()")
  expect_message(tibble::tibble(x=c(NA, 2:3), y=3:1) %>% coo_single(), "use coo_single()")

  expect_true(new_coo_single() %>% is_coo_single())
  expect_true(new_coo_single() %>% is_coo_single1())

  expect_output(new_coo_single() %>% print(), "coo_single")
})

# coo_list ------------------------------------------------
test_that("coo_list works", {
  test_inheritance <- function(x){
    expect_true(inherits(x, "coo_list"))
    expect_true(inherits(x, "list"))
    expect_true(inherits(x, "vctrs_list_of"))
    #expect_true(all(class(x)==c("coo_list", "list", "vctrs_list_of", "vctrs_vctr")))
    expect_true(vctrs::is_list_of(x))
  }

  # classes ---
  expect_message(coo_list("a"))
  # coo_single
  new_coo_single() %>% coo_list() %>% test_inheritance()
  # list
  # this one should not work!
  expect_message(list(1, 2, 3) %>% coo_list())

  # list of coo_single
  list(new_coo_single(), new_coo_single()) %>% coo_list() %>% test_inheritance()

  # coo_list of coo_single
  list(new_coo_single(), new_coo_single()) %>% coo_list() %>% test_inheritance()

  # test dplyr retain coo_list
  expect_is(bot %>% dplyr::slice(1) %>% dplyr::pull(coo), "coo_list")
  expect_is(bot %>% dplyr::mutate(coo2=coo) %>% dplyr::pull(coo2), "coo_list")

  # printers and pillars ---
  expect_output(new_coo_list() %>% print, "coo_list") # vec_ptype_full.coo_list
  expect_output(bot %>% print, "mom_tbl") # vec_ptype_abbr.coo_list
  expect_true(new_coo_list() %>% pillar::is_vector_s3())
})

# COE =====================================================
# coe_single ----

test_that("coe_single works", {
  expect_message(coe_single("foo"))

  # passing a tibble
  x <- tibble::tibble(a=1, b=2)
  expect_is(coe_single(x), "coe_single")

  # passing coe_single
  expect_identical(coe_single(x), coe_single(coe_single(x)))

  # passing a numeric
  expect_is(coe_single(c(4, 5, 6)), "coe_single")

  y <- list(tibble::tibble(a=3, b=4),
            x)
  expect_is(coe_list(y), "coe_list")
  expect_message(coe_list("foo"))
})



test_that("coe_single works", {

  test_inheritance <- function(x){
    expect_true(inherits(x, "coe_single"))
    expect_true(inherits(x, "tbl"))
    expect_true(inherits(x, "data.frame"))
    expect_true(is_coe_single(x))
    expect_true(is_coe_single1(x))
  }
  # anything that can be coerced by tibble should work
  1:5 %>% coe_single() %>% test_inheritance()
  1:2 %>% `names<-`(c("foo", "plop")) %>%  coe_single() %>% test_inheritance()
  iris[, 1:2] %>% coe_single() %>% test_inheritance()
  iris %>% tibble::as_tibble() %>% coe_single() %>% test_inheritance()
  matrix(1:5) %>% coe_single() %>% test_inheritance()

  expect_message("a" %>% coe_single(), "no method")
  expect_output(new_coe_single() %>% print, "coe_single")

  expect_true(new_coe_single() %>% is_coe_single())
  expect_true(new_coe_single() %>% is_coe_single1())
})

# coe_list ------------------------------------------------
test_that("coe_list works", {
  test_inheritance <- function(x){
    expect_true(inherits(x, "coe_list"))
    expect_true(inherits(x, "list"))
    expect_true(inherits(x, "vctrs_list_of"))
    # make travis fail for a very weird reason
    # expect_true(all(class(x)==c("coe_list", "list", "vctrs_list_of", "vctrs_vctr")))
    expect_true(vctrs::is_list_of(x))
  }

  # classes ---
  expect_message(coe_list("a"))
  # coo_single
  new_coe_single() %>% coe_list() %>% test_inheritance()
  # list
  # I think this one should work but lets see
  # how different coe shape make this thing evolve
  list(1, 2, 3) %>% coe_list() %>% test_inheritance()

  # list of coe_single
  list(new_coe_single(), new_coe_single()) %>% coe_list() %>% test_inheritance()

  # test dplyr retain coo_list
  x <- bot %>% efourier(4)
  expect_true(x$coe %>% is_coe_list)
  expect_true(x %>% dplyr::mutate(coe2=coe) %>% dplyr::pull(coe2) %>% is_coe_list())

  # printers and pillars --
  expect_output(new_coe_list() %>% print, "coe_list") # vec_ptype_full.coo_list
  expect_output(x %>% print, "mom") # vec_ptype_abbr.coo_list
  expect_true(new_coe_list() %>% pillar::is_vector_s3())
})
