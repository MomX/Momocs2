test_that("coo_center, coo_trans works", {
  xy <- bot2$coo[[1]] %>% coo_center %>% coo_centpos() %>% unlist
  expect_equivalent(xy[1], expected=0, tolerance=1e-10)
  expect_equivalent(xy[2], expected=0, tolerance=1e-10)

  df <- bot2$coo %>% coo_center %>% coo_centpos() %>% dplyr::bind_rows()
  expect_equivalent(sum(df$x), 0, tolerance=1e-10)
  expect_equivalent(sum(df$y), 0, tolerance=1e-10)

  df <- bot2 %>% coo_center %>% dplyr::pull(coo) %>% coo_centpos() %>% dplyr::bind_rows()
  expect_equivalent(sum(df$x), 0, tolerance=1e-10)
  expect_equivalent(sum(df$y), 0, tolerance=1e-10)

  xy <- bot2$coo[[1]] %>% coo_center %>% coo_trans(5, -3) %>% coo_centpos() %>% unlist()
  expect_equivalent(xy[1], expected=5, tolerance=1e-10)
  expect_equivalent(xy[2], expected=-3, tolerance=1e-10)
})

test_that("coo_scale works", {
  expect_equivalent(tibble(x=c(0, sqrt(2)), y=c(0, -sqrt(2))) %>% coo_centsize(),
                    expected = 1, tolerance=1e-10)
  expect_equivalent(tibble(x=c(0, 2*sqrt(2)), y=c(0, -2*sqrt(2))) %>% coo_centsize(),
                    expected = 2, tolerance=1e-10)
  expect_equivalent(bot2$coo[[1]] %>% coo_scale %>% coo_centsize(), 1, tol=1e-10)
  x <- bot2$coo %>% coo_scale()
  expect_true(is.list(x))
  expect_equivalent(x %>% purrr::map_dbl(coo_centsize), rep(1, length(x)), tol=1e-10)
})
