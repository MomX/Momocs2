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

  # is_coo_*
  expect_true(bot %>% pick() %>% is_coo_single())
  expect_true(bot %>% pick() %>% is_coo_single1())

  expect_true(bot$coo %>% is_coo_list())
  expect_true(bot$coo %>% is_coo_list1())

  expect_true(bot %>% is_mom_tbl())
  expect_true(bot %>% is_mom_tbl1())

  # is_coe_*
  x <- bot %>% efourier(2)
  expect_true(x$coe[[1]] %>% is_coe_single())

  expect_true(x$coe %>% is_coe_list())

  expect_true(x %>% is_mom_tbl())

  # .seq_naming_list
  expect_true(list(a=1:5, b=5:2) %>% .seq_naming_list() %>% purrr::map_lgl(~.x %>% names %>% is.null) %>% `!` %>% all)
  expect_true(list(a=1:5, b=5:2) %>%  purrr::map_lgl(~ .x %>% names %>% is.null) %>% all())
})


test_that("msg work", {
  expect_message(.msg_info("plop"), "plop")
  expect_message(.msg_warning("plop"), "plop")
  expect_message(.msg_success("plop"), "plop")
  expect_message(.msg_danger("plop"), "plop")

  expect_invisible(.check(2==(1+1), "rololo"))
  expect_message(.check(2==1, "rololo"), "rololo")
})

test_that(".replace_class work", {
  x <- new_coo_single()
  expect_true(x %>% .replace_class("tbl", "tableau") %>% inherits("tableau"))
})

