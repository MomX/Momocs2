test_that(".mprod works",{
  m <- matrix(1:12, ncol=4) %>% `colnames<-`(paste0("col", 1:4))
  s <- c(col2=1.5, col3=0.1)
  x <- .mprod(m, s)
  expect_is(x, "matrix")
  expect_equal(ncol(x), ncol(m))
  expect_equal(sum(x[, c("col1", "col4")]), 0)
})
