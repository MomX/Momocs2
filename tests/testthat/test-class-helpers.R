test_that("class helpers work", {
  x <- 1
  x2 <- x %>% .append_class("foo")
  expect_equal(x2 %>% class() %>% `[`(1), "foo")
  expect_equal(x2 %>% class() %>% `[`(2), class(x))

  expect_true(x2 %>% .is_class("foo"))
  expect_true(x2 %>% .is_class(class(x)))

  expect_true(x2 %>% .is_class1("foo"))
  expect_false(x2 %>% .is_class1("numeric"))
})
