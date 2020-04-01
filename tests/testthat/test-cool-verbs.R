
# coe verbs ---------
test_that("coe verbs work",{
  x <- bot2
  x$coo2 <- x$coo
  x$coe <- x$coo %>% efourier(4)
  x$coe2 <- x$coo2 %>% efourier(6)

  # coo_list verbs
  expect_equal(x %>% coo_select(coo) %>% coo_only() %>% colnames(), "coo")
  expect_equal(x %>% coo_select(-coo) %>% coo_only() %>% colnames(), "coo2")

  expect_equal(x %>% coo_only() %>% colnames(), c("coo", "coo2"))
  expect_equal(x %>% coo_drop() %>% colnames(), x %>% dplyr::select(-coo, -coo2) %>% colnames())

  expect_equal(x %>% coo_nb, 2)

  # coe_list verbs
  expect_equal(x %>% coe_select(coe) %>% coe_only() %>% colnames(), "coe")
  expect_equal(x %>% coe_select(-coe) %>% coe_only() %>% colnames(), "coe2")

  expect_equal(x %>% coe_only() %>% colnames(), c("coe", "coe2"))
  expect_equal(x %>% coe_drop() %>% colnames(), x %>% dplyr::select(-coe, -coe2) %>% colnames())

  expect_equal(x %>% coe_nb, 2)

})



