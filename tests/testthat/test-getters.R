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

test_that("get_perim and friends work", {
  x <- bot2 %>% pick(4)
  y <- bot2$coo
  z <- bot2

  x_0 <- get_perim(x)
  y_0 <- get_perim(y)
  z_0 <- get_perim(z)

  x_a <- get_perim_along(x)
  y_a <- get_perim_along(y)
  z_a <- get_perim_along(z)

  x_s <- get_perim_cumsum(x)
  y_s <- get_perim_cumsum(y)
  z_s <- get_perim_cumsum(z)

  expect_is(x_0, "numeric")
  expect_is(y_0, "numeric")
  expect_is(z_0, "coo_tbl")

  expect_is(x_a, "tbl")
  expect_is(y_a, "list")
  expect_true(y_a %>% purrr::map_lgl(tibble::is_tibble) %>% all)
  expect_is(z_a, "coo_tbl")
  expect_equal(y_a, z_a$perim_along)

  # sum along is last of cumsum and is also perim
  expect_equivalent(sum(x_a), unlist(x_s[nrow(x_s), 1]))
  expect_equivalent(sum(x_a), x_0)
})
