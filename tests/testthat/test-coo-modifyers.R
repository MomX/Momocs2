# coo_center ---------
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

# coo_scale ---------
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

# coo_trans ---------
test_that("coo_trans works", {
  expect_is(bot2$coo %>% coo_trans(), "coo_list")
  expect_is(bot2 %>% coo_trans(), "coo_tbl")

})

# coo_template ---------
test_that("coo_template works", {
  x <- bot2$coo %>% coo_template()
  expect_equivalent(apply(get_diffrange(x), 1, max), expected=rep(1, nrow(bot2)), tol=1e-10)
  expect_is(x, "coo_list")

  y <- bot2 %>% coo_template() %>% get_diffrange()
  expect_equivalent(apply(dplyr::select(y, x_range, y_range), 1, max),
                          expected=rep(1, nrow(bot2)), tol=1e-10)
  expect_is(y, "coo_tbl")
})

# coo_rotate ---------
test_that("coo_rotate works", {
  expect_equal(radians_to_degrees(pi/2), 90)
  expect_equal(radians_to_degrees(-pi/2), -90)
  x <- 127
  expect_equal(x %>% degrees_to_radians() %>% radians_to_degrees(), x)
  y <- -pi/56
  expect_equal(y %>% radians_to_degrees() %>% degrees_to_radians(), y)

  expect_is(bot2 %>% pick(1) %>% coo_align, "coo_single")
  expect_is(bot2$coo %>% coo_align, "coo_list")
  expect_is(bot2 %>% coo_align, "coo_tbl")

  expect_is(bot2 %>% pick(1) %>% coo_rotate, "coo_single")
  expect_is(bot2$coo %>% coo_rotate, "coo_list")
  expect_is(bot2 %>% coo_rotate, "coo_tbl")

  expect_is(bot2 %>% pick(1) %>% coo_rotatecenter, "coo_single")
  expect_is(bot2$coo %>% coo_rotatecenter, "coo_list")
  expect_is(bot2 %>% coo_rotatecenter, "coo_tbl")
})

# coo_sample -----
test_that("coo_sample works", {
  x <- bot2 %>% pick(1) %>% coo_sample(12)
  y <- bot2 %>% dplyr::slice(1:5) %$% coo %>% coo_sample(6)
  z <- bot2 %>% dplyr::slice(1:5) %>% coo_sample(6) %$% coo

  # early return
  expect_identical(x, x %>% coo_sample(n=nrow(x)))

  # classes
  expect_is(x, "coo_single")
  expect_is(y, "coo_list")
  expect_is(z, "coo_list")

  # nrow
  expect_equal(nrow(x), 12)
  expect_equivalent(purrr:::map_dbl(y, nrow), rep(6, 5))
  expect_equivalent(purrr:::map_dbl(z, nrow), rep(6, 5))
  expect_equivalent(y, z)

  # interpolate dispatch
  x2 <- expect_message(x %>% coo_sample(120))
  expect_equal(nrow(x2), 120 )
  expect_equal(x2 %>% dplyr::distinct() %>% nrow(), 120)
})

test_that("coo_interpolate works", {
  x <- bot2 %>% pick(5) %>% coo_sample(12)
  expect_equal(x %>% coo_interpolate(120) %>% nrow(), 120)
  y <- bot2 %>% dplyr::slice(1:2)

  # coo_list
  expect_equivalent(y$coo %>% coo_sample(12) %>% coo_interpolate(24) %>% purrr::map_dbl(nrow),
                    rep(24, 2))

  # coo_tbl
  expect_equivalent(y %>% coo_sample(12) %>% coo_interpolate(24) %$% coo %>% purrr::map_dbl(nrow),
               rep(24, 2))

})








