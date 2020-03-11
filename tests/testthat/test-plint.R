test_that("plint works", {
  x <- matrix(1:12, ncol=2) %>% coo_single()
  expect_output(plint(x))
  expect_s3_class(plint(x), "coo_single")
})
