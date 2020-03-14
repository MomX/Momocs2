test_that("coo_center, coo_trans works", {
  xy <- bot2$coo[[1]] %>% coo_center %>% get_centpos() %>% unlist
  expect_equivalent(xy[1], expected=0, tolerance=1e-10)
  expect_equivalent(xy[2], expected=0, tolerance=1e-10)

  df <- bot2$coo %>% coo_center %>% get_centpos() %>% dplyr::bind_rows()
  expect_equivalent(sum(df$x), 0, tolerance=1e-10)
  expect_equivalent(sum(df$y), 0, tolerance=1e-10)

  df <- bot2 %>% coo_center %>% dplyr::pull(coo) %>% get_centpos() %>% dplyr::bind_rows()
  expect_equivalent(sum(df$x), 0, tolerance=1e-10)
  expect_equivalent(sum(df$y), 0, tolerance=1e-10)

  xy <- bot2$coo[[1]] %>% coo_center %>% coo_trans(5, -3) %>% get_centpos() %>% unlist()
  expect_equivalent(xy[1], expected=5, tolerance=1e-10)
  expect_equivalent(xy[2], expected=-3, tolerance=1e-10)

  x <- bot2$coo[[1]] %>% as.matrix() %>% coo_center()
  expect_is(x, "coo_single")

  expect_is(bot2$coo %>% coo_center, "coo_list")
  expect_is(bot2$coo %>% coo_trans, "coo_list")
})

test_that("coo_scale works", {
  expect_equivalent(tibble(x=c(0, sqrt(2)), y=c(0, -sqrt(2))) %>% get_centsize(),
                    expected = 1, tolerance=1e-10)
  expect_equivalent(tibble(x=c(0, 2*sqrt(2)), y=c(0, -2*sqrt(2))) %>% get_centsize(),
                    expected = 2, tolerance=1e-10)
  expect_equivalent(bot2$coo[[1]] %>% coo_scale %>% get_centsize(), 1, tol=1e-10)
  x <- bot2$coo %>% coo_scale()
  expect_is(x, "coo_list")
  expect_equivalent(x %>% purrr::map_dbl(get_centsize), rep(1, length(x)), tol=1e-10)

  expect_is(bot2 %>% coo_scale, "coo_tbl")

})

test_that("coo_trans works", {
  expect_is(bot2$coo %>% coo_trans(), "coo_list")

})

test_that("coo_template works", {
  x <- bot2$coo %>% coo_template()
  expect_equivalent(apply(get_diffrange(x), 1, max), expected=rep(1, nrow(bot2)), tol=1e-10)
  expect_is(x, "coo_list")

  y <- bot2 %>% coo_template() %>% get_diffrange()
  expect_equivalent(apply(dplyr::select(y, x_range, y_range), 1, max),
                          expected=rep(1, nrow(bot2)), tol=1e-10)
  expect_is(x, "coo_tbl")
})

test_that("coo_rotate works", {
  expect_equal(radians_to_degrees(pi/2), 90)
  expect_equal(radians_to_degrees(-pi/2), -90)
  x <- 127
  expect_equal(x %>% degrees_to_radians() %>% radians_to_degrees(), x)
  y <- -pi/56
  expect_equal(y %>% radians_to_degrees() %>% degrees_to_radians(), y)
})
