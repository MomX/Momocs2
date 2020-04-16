test_that("get_ldk works", {
  expect_message(get_ldk("a"), "no method")

  h <- hearts %>% dplyr::slice(1:2)
  ldks0 <- h %>% get_ldk()
  ldks1 <- h %>% get_ldk(from_col=coo, ldk_col=ldk)
  expect_identical(ldks0, ldks1)


  expect_equivalent(purrr::map2(h$coo, h$ldk, ~.x[.y, ]),
                    ldks1$coo_ldk)
})
