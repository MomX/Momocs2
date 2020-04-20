test_that("likely_bookstein works", {
  not_booked <- olea %>% pick(5) %>% coo_trans(10, 10)
  booked     <- not_booked %>% coo_bookstein()
  expect_false(not_booked %>% likely_bookstein())
  expect_true(booked %>% likely_bookstein())
})


test_that("set_names_poly works", {
  n <- 5
  x <- runif(n) %>% set_names_poly() %>% names
  expect_true(all(x == c("a0", "a1", "a2", "a3", "a4")))
})

test_that("npoly works", {
  booked <- olea %>% pick(5) %>% coo_bookstein()
  expect_message(booked %>% npoly(), "degree")
  op <- booked %>% npoly(5)
  expect_is(op, "npoly_single")
  expect_is(op, "coe_single")
  expect_is(op, "tbl")

  x <- olea$coo[1:3]
  expect_message(npoly(x), "not provided")
  y <- npoly(x, degree=3)
  expect_is(y, "coe_list")

  x <- dplyr::slice(olea, 1:3)
  expect_message(npoly(x), "not provided")
  y <- npoly(x, degree=3)
  expect_is(y, "mom_tbl")
  expect_is(y, "tbl")

  y <- x %>% dplyr::rename(coo2=coo) %>%
    npoly(degree=3, drop_coo=FALSE, from_coo=coo2, to_coe=coe2) %>%
    dplyr::rename(coo=coo2) %>%
    npoly(degree=3)
  expect_identical(y$coe, y$coe2)
  expect_is(y$coe, "coe_list")
  expect_is(y$coe, "npoly")
})

