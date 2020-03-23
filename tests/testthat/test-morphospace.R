test_that(".mprod works",{
  m <- matrix(1:12, ncol=4) %>% `colnames<-`(paste0("col", 1:4))
  s <- c(col2=1.5, col3=0.1)
  x <- .mprod(m, s)
  expect_is(x, "matrix")
  expect_equal(ncol(x), ncol(m))
  expect_equal(sum(x[, c("col1", "col4")]), 0)
})

test_that(".morphospace_templating_table works",{
  s <- 1:12
  # at least correct format is returned
  expect_equal(purrr:::map_dbl(s, ~.morphospace_templating_table(.x) %>% nrow),
               s)
})

test_that("morphospace works", {
  # todo
  x <- bot2 %>% dplyr::slice(1:5) %>% efourier %>% stat_pca
  expect_message(y <- x %>% morphospace())
  expect_is(y, "tbl")

  z <- y %>% morphospace_template()
  expect_is(z, "tbl")

  expect_equal(nrow(y), nrow(z))
})


test_that("morphospace_positionners work", {
  xy <- bot2 %>% efourier(6) %>% stat_pca() %>% dplyr::select(PC1, PC2)
  df <- morphospace_grid_window(xy, nr=12, nc=8)
  expect_is(df, "tbl")
  expect_equal(nrow(df), 12*8)

  df <- morphospace_grid_abs(xy, nr=12, nc=8)
  expect_is(df, "tbl")
  expect_equal(nrow(df), 12*8)

  # df <- morphospace_grid_range(xy)
  # expect_is(df, "tbl")
  # expect_equal(nrow(df), 4)

  df <- morphospace_circle(xy, 6, 0.1)
  expect_is(df, "tbl")
  expect_equal(nrow(df), 6)

  df <- morphospace_xy(xy)
  expect_is(df, "tbl")
  expect_equal(nrow(df), nrow(xy))
  expect_equivalent(df, xy)
})
