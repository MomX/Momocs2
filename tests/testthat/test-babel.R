test_that("complex and cartesian work", {
  x <- coo_single(tibble::tibble(x=0.213, y=-9.2324))
  expect_equivalent(x, x %>% cartesian_2_complex() %>% complex_2_cartesian())
})
