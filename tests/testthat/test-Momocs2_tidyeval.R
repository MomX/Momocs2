test_that("tidyeval_coo_modifyers works", {
  # simulate behaviour within a function
  coo_foo <- function(from_col=coo, to_col={{from_col}}){
    # tidy eval
    tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})
  }

  # default (nothing passed)
  x <- coo_foo()
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$to_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "coo")
  expect_true(rlang::quo_get_expr(x$to_col)   == "coo")

  # from passed, not col but equals to from
  x <- coo_foo(plop)
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$to_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "plop")
  expect_true(rlang::quo_get_expr(x$to_col)   == "plop")

  # col passed, to passed
  x <- coo_foo(plop, plip)
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$to_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "plop")
  expect_true(rlang::quo_get_expr(x$to_col)   == "plip")

  # col not passed, to passed
  x <- coo_foo(to_col = plip)
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$to_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "coo")
  expect_true(rlang::quo_get_expr(x$to_col)   == "plip")

  x <- tidyeval_coo_modifyers(yop)
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$to_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "yop")
  expect_true(rlang::quo_get_expr(x$to_col)   == "yop")
})

test_that("tidyeval_coo_and_ldk works", {
  # simulate behaviour within a function
  coo_foo <- function(from_col=coo, ldk_col=ldk){
    # tidy eval
    tidyeval_coo_and_ldk(from_col={{from_col}}, ldk_col={{ldk_col}})
  }

  # default (nothing passed)
  x <- coo_foo()
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$ldk_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "coo")
  expect_true(rlang::quo_get_expr(x$ldk_col)   == "ldk")

  # from passed, not col but equals to from
  x <- coo_foo(plop, plip)
  expect_true(x$from_col %>% rlang::is_quosure())
  expect_true(x$ldk_col %>% rlang::is_quosure())
  expect_true(rlang::quo_get_expr(x$from_col) == "plop")
  expect_true(rlang::quo_get_expr(x$ldk_col)   == "plip")

})
