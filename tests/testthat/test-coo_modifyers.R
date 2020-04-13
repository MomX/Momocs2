test_cols_tidyeval <- function(fun, ...){
  w <- bot %>% dplyr::slice(1:3) # for the sake fo speed
  # default
  x <- w %>% fun(...) %>% dplyr::pull(coo)
  # another from used but still default target (ie from_col too)
  y <- w %>% dplyr::rename(coo2=coo) %>% fun(from_col=coo2, ...) %>% dplyr::pull(coo2)
  # from and col specified
  z <- w %>% dplyr::rename(coo2=coo) %>% fun(from_col=coo2, to_col=coo3, ...) %>% dplyr::pull(coo3)
  # list(x, y, z) %>% return()
  expect_identical(x, y)
  expect_identical(y, z)
  expect_identical(x, z)
}
# eg test_cols_tidyeval

test_classes <- function(fun, ...){
  x <- dplyr::slice(bot, 1:3) # for the sake of speed
  expect_is(x %>% fun(...),     "mom_tbl")
  expect_is(x$coo %>% fun(...), "coo_list")
  expect_is(x$coo[[1]] %>% fun(...), "coo_single")
}
# eg test_classes(coo_center)

test_equivalence <- function(fun, ...){
  x <- dplyr::slice(bot, 1:3) %>% coo_sample(20) # for the sake of speed
  x1 <- x %>% fun(...) %>% dplyr::pull(coo)
  y1 <- x$coo %>% fun(...)
  z1 <- new_coo_list(list(x$coo[[1]] %>% fun(...),
                          x$coo[[2]] %>% fun(...),
                          x$coo[[3]] %>% fun(...)))
  expect_equivalent(x1, y1)
  expect_equivalent(y1, z1)
  expect_equivalent(x1, z1) # not exactly require but still...
}

# coo_center ---------
test_that("coo_center, coo_trans works", {
  test_classes(coo_center)
  test_cols_tidyeval(coo_center)
  test_equivalence(coo_center)

  xy <- bot$coo[[1]] %>% coo_center %>% get_centpos() %>% unlist
  expect_equivalent(xy[1], expected=0, tolerance=1e-10)
  expect_equivalent(xy[2], expected=0, tolerance=1e-10)

  df <- bot$coo %>% coo_center %>% get_centpos() %>% dplyr::bind_rows()
  expect_equivalent(sum(df$centpos_x), 0, tolerance=1e-10)
  expect_equivalent(sum(df$centpos_y), 0, tolerance=1e-10)

  df <- bot %>% coo_center %>% dplyr::pull(coo) %>% get_centpos() %>% dplyr::bind_rows()
  expect_equivalent(sum(df$centpos_x), 0, tolerance=1e-10)
  expect_equivalent(sum(df$centpos_y), 0, tolerance=1e-10)

  xy <- bot$coo[[1]] %>% coo_center %>% coo_trans(5, -3) %>% get_centpos() %>% unlist()
  expect_equivalent(xy[1], expected=5, tolerance=1e-10)
  expect_equivalent(xy[2], expected=-3, tolerance=1e-10)
})

# coo_trans ---------
test_that("coo_trans works", {
  test_classes(coo_trans)
  test_cols_tidyeval(coo_trans)
  test_equivalence(coo_trans)
})

# coo_scale ---------
test_that("coo_scale works", {
  test_classes(coo_scale)
  test_cols_tidyeval(coo_scale)
  test_equivalence(coo_scale)

  expect_equivalent(tibble(x=c(0, sqrt(2)), y=c(0, -sqrt(2))) %>% get_centsize(),
                    expected = 1, tolerance=1e-10)
  expect_equivalent(tibble(x=c(0, 2*sqrt(2)), y=c(0, -2*sqrt(2))) %>% get_centsize(),
                    expected = 2, tolerance=1e-10)
  expect_equivalent(bot$coo[[1]] %>% coo_scale %>% get_centsize(), 1, tol=1e-10)
})


# coo_template ---------
test_that("coo_template works", {
  test_classes(coo_template)
  test_cols_tidyeval(coo_template)
  test_equivalence(coo_template)

  x <- bot$coo %>% coo_template()
  expect_equivalent(apply(get_diffrange(x), 1, max), expected=rep(1, nrow(bot)), tol=1e-10)
  expect_is(x, "coo_list")

  y <- bot %>% coo_template() %>% get_diffrange()
  expect_equivalent(apply(dplyr::select(y, x_range, y_range), 1, max),
                    expected=rep(1, nrow(bot)), tol=1e-10)
  expect_is(y, "mom_tbl")

})

# coo_flip ---------
test_that("coo_flipx and y work", {
  test_classes(coo_flip_x)
  test_cols_tidyeval(coo_flip_x)
  test_equivalence(coo_flip_x)

  test_classes(coo_flip_y)
  test_cols_tidyeval(coo_flip_y)
  test_equivalence(coo_flip_y)

})


# coo_rotate(center) ------
test_that("coo_rotate works", {
  test_classes(coo_rotate)
  test_cols_tidyeval(coo_rotate)
  test_equivalence(coo_rotate)

  test_classes(coo_rotatecenter)
  test_cols_tidyeval(coo_rotatecenter)
  test_equivalence(coo_rotatecenter)

  expect_equal(radians_to_degrees(pi/2), 90)
  expect_equal(radians_to_degrees(-pi/2), -90)
  x <- 127
  expect_equal(x %>% degrees_to_radians() %>% radians_to_degrees(), x)
  y <- -pi/56
  expect_equal(y %>% radians_to_degrees() %>% degrees_to_radians(), y)
})

# coo_align and align_xax--------
test_that("coo_align and coo_align_xax work", {
  # coo_align_xax
  # todo I copy paste this here, above is possibly obsolete now
  test_classes(coo_align)
  test_cols_tidyeval(coo_align)
  test_equivalence(coo_align)

  test_classes(coo_align_xax)
  test_cols_tidyeval(coo_align_xax)
  test_equivalence(coo_align_xax)
})

# coo_sample -----
test_that("coo_sample works", {
  # if not provided stop and message

  expect_error(coo_sample(pick(bot)), "n")

  test_classes(coo_sample, 12)
  test_cols_tidyeval(coo_sample, 6)
  test_equivalence(coo_sample, 5)


  x <- bot %>% pick(1) %>% coo_sample(12)
  y <- bot %>% dplyr::slice(1:5) %$% coo %>% coo_sample(6)
  z <- bot %>% dplyr::slice(1:5) %>% coo_sample(6) %$% coo

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

# coo_sample_rr
test_that("coo_sample_rr works", {
  test_classes(coo_sample_rr, 5)
  test_cols_tidyeval(coo_sample_rr, 5)
  test_equivalence(coo_sample_rr, 5)
  # missing case
  expect_error(bot %>% pick(1) %>% coo_sample_rr, "n")
  #too ambitious case
  expect_error(bot %>% pick(1) %>% coo_sample_rr(1e5), "coo_interpolate")
})

# coo_sample_prop -----
test_that("coo_sample_prop works", {
  test_classes(coo_sample_prop, 0.5)
  test_cols_tidyeval(coo_sample_prop, 0.5)
  test_equivalence(coo_sample_prop, 0.5)

  # bot$coo %>% coo_sample_prop(0.1) %>% purrr::map_dbl(nrow)

})

# coo_interpolate ------
test_that("coo_interpolate works", {
  test_classes(coo_interpolate, 12)
  test_cols_tidyeval(coo_interpolate, 6)
  test_equivalence(coo_interpolate, 24)

  x <- bot %>% pick(5) %>% coo_sample(12)
  expect_equal(x %>% coo_interpolate(120) %>% nrow(), 120)
  y <- bot %>% dplyr::slice(1:2)

  # early return
  expect_identical(x, x %>% coo_interpolate(n=nrow(x)))

  # coo_list
  expect_equivalent(y$coo %>% coo_sample(12) %>% coo_interpolate(24) %>% purrr::map_dbl(nrow),
                    rep(24, 2))

  # mom_tbl
  expect_equivalent(y %>% coo_sample(12) %>% coo_interpolate(24) %$% coo %>% purrr::map_dbl(nrow),
                    rep(24, 2))
})


# coo_smooth -------

# coo_close ---------
test_that("coo_close, coo_unclose and testers work", {
  test_classes(coo_close)
  test_cols_tidyeval(coo_close)
  test_equivalence(coo_close)

  test_classes(coo_unclose)
  test_cols_tidyeval(coo_unclose)
  test_equivalence(coo_unclose)


  x <- bot %>% pick(1)
  # test helpers
  expect_true(x %>% coo_close %>% is_closed)
  expect_true(x %>% coo_unclose %>% is_unclosed)

  # test composition
  expect_equal(x %>% coo_close %>% coo_unclose, x)

  # test that once it's closed, it's closed
  expect_identical(x %>% coo_close, x %>% coo_close %>% coo_close)

  # same for open
  expect_identical(x %>% coo_unclose, x %>% coo_unclose %>% coo_unclose)

})

# coo_up
test_that("coo_up and friends work fine", {
  test_classes(coo_up)
  test_cols_tidyeval(coo_up)
  test_equivalence(coo_up)

  test_classes(coo_down)
  test_cols_tidyeval(coo_down)
  test_equivalence(coo_down)

  test_classes(coo_left)
  test_cols_tidyeval(coo_left)
  test_equivalence(coo_left)

  test_classes(coo_right)
  test_cols_tidyeval(coo_right)
  test_equivalence(coo_right)
})

test_that("coo_rev works", {
  test_classes(coo_rev)
  test_cols_tidyeval(coo_rev)
  test_equivalence(coo_rev)
})
