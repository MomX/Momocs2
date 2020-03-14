test_that("getters works", {
  x <- bot2 %>% get_centpos()
  expect_equal(sum(c("centpos_x", "centpos_y") %in% colnames(x)), 2)
  expect_is(x, "coo_tbl")

  # some are coo_scale and coo_center side
  x <- bot2 %>% get_centsize()
  expect_is(x, "coo_tbl")
  y <- bot2$coo %>% get_centsize
  # tests equality and column presence
  expect_true(all(x$centsize==y))
})


test_that("get_range works", {
  x <- get_range(bot2 %>% pick(1))
  expect_is(x, "tbl")
  expect_true(all(c("x_min", "x_max", "y_min", "y_max") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  x <- get_range(bot2$coo)
  expect_is(x, "tbl")
  expect_true(all(c("x_min", "x_max", "y_min", "y_max") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  expect_is(bot2 %>% get_range, "coo_tbl")
})

test_that("get_diff works", {
  x <- get_diffrange(bot2 %>% pick(1))
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  x <- get_diffrange(bot2$coo)
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  expect_is(bot2 %>% get_diffrange, "coo_tbl")
  expect_equal(bot2 %>% get_diffrange %>% dplyr::select(x_range, y_range), x)
})

test_that("get_lw works", {
  x <- get_lw(bot2 %>% pick(1))
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  x <- get_lw(bot2$coo)
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  expect_is(bot2 %>% get_lw, "coo_tbl")
  expect_equal(bot2 %>% get_lw %>% dplyr::select(x_range, y_range), x)
})

test_that("get_length and get_width works", {
  x <- get_length(bot2$coo)
  y <- get_width(bot2$coo)
  expect_is(x, "numeric")
  expect_is(y, "numeric")

  z <- bot2 %>% get_length() %>% get_width()
  expect_is(z, "coo_tbl")
  expect_true(all(c("length", "width") %in% colnames(z)))
})

