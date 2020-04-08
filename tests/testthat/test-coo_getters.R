test_that("getters works", {
  x <- bot %>% get_centpos()
  expect_equal(sum(c("centpos_x", "centpos_y") %in% colnames(x)), 2)
  expect_is(x, "mom_tbl")

  # some are coo_scale and coo_center side
  x <- bot %>% get_centsize()
  expect_is(x, "mom_tbl")
  y <- bot$coo %>% get_centsize
  # tests equality and column presence
  expect_true(all(x$centsize==y))
})


test_that("get_range works", {
  x <- get_range(bot %>% pick(1))
  expect_is(x, "tbl")
  expect_true(all(c("x_min", "x_max", "y_min", "y_max") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  x <- get_range(bot$coo)
  expect_is(x, "tbl")
  expect_true(all(c("x_min", "x_max", "y_min", "y_max") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  expect_is(bot %>% get_range, "mom_tbl")
})

test_that("get_diffrange works", {
  x <- get_diffrange(bot %>% pick(1))
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  x <- get_diffrange(bot$coo)
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  expect_is(bot %>% get_diffrange, "mom_tbl")
  expect_equivalent(bot %>% get_diffrange %>% dplyr::select(x_range, y_range), x)
})

test_that("get_lw works", {
  x <- get_lw(bot %>% pick(1))
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  x <- get_lw(bot$coo)
  expect_is(x, "tbl")
  expect_true(all(c("x_range", "y_range") %in% colnames(x)))
  expect_true(all(is.numeric(unlist(x))))

  expect_is(bot %>% get_lw, "mom_tbl")
  expect_equivalent(bot %>% get_lw %>% dplyr::select(x_range, y_range), x)
})

test_that("get_length and get_width works", {
  x <- get_length(bot$coo)
  y <- get_width(bot$coo)
  expect_is(x, "numeric")
  expect_is(y, "numeric")

  z <- bot %>% get_length() %>% get_width()
  expect_is(z, "mom_tbl")
  expect_true(all(c("length", "width") %in% colnames(z)))
})

test_that("get_perim and friends work", {
  x <- bot %>% pick(4)
  y <- bot$coo
  z <- bot

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
  expect_is(y_0, "tbl")
  expect_is(z_0, "mom_tbl")

  expect_is(x_a, "tbl")
  expect_is(y_a, "list")
  expect_true(y_a %>% purrr::map_lgl(tibble::is_tibble) %>% all)
  expect_is(z_a, "mom_tbl")
  expect_equal(y_a, z_a$perim_along)

  # sum along is last of cumsum and is also perim
  expect_equivalent(sum(x_a), unlist(x_s[nrow(x_s), 1]))
  expect_equivalent(sum(x_a), x_0)
})

test_that("get_nb works", {
  x <- matrix(1:10, nrow=5, ncol=2) %>% coo_single()
  y <- matrix(1:12, nrow=6, ncol=2) %>% coo_single()
  expect_equal(get_nb(x), 5)
  expect_equal(coo_list(list(x, y)) %>% get_nb(), c(5, 6))
  expect_equal(mom(coo_list(list(x, y))) %>% get_nb() %>% dplyr::pull(nb), c(5, 6))
})
