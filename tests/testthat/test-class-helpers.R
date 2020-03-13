test_that("utils work", {

  # classes
  x <- 1
  x2 <- x %>% .append_class("foo")
  expect_equal(x2 %>% class() %>% `[`(1), "foo")
  expect_equal(x2 %>% class() %>% `[`(2), class(x))

  expect_true(x2 %>% .is_class("foo"))
  expect_true(x2 %>% .is_class(class(x)))

  expect_true(x2 %>% .is_class1("foo"))
  expect_false(x2 %>% .is_class1("numeric"))

  # .seq_naming_list
  expect_true(list(a=1:5, b=5:2) %>% .seq_naming_list() %>% purrr::map_lgl(~.x %>% names %>% is.null) %>% `!` %>% all)
  expect_true(list(a=1:5, b=5:2) %>%  purrr::map_lgl(~ .x %>% names %>% is.null) %>% all())
})
