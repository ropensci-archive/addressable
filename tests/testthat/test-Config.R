test_that("Config", {
  x <- Config$new()

  # the variables
  expect_is(x, "Config")
  expect_is(x, "R6")
  expect_is(x$config, "list")
  expect_is(x$providers, "list")
  expect_is(x$errors, "list")
  
  # the functions
  expect_is(x$all_settings, "function")
  expect_is(x$provider, "function")
  expect_is(x$error_message, "function")
  expect_is(x$error_messages, "function")
  expect_is(x$configure, "function")

  z <- Config$new(list(dns_timeout = 4))
  expect_equal(z$config$dns_timeout, 4)
})

test_that("Config fails", {
  # must be a list
  expect_error(Config$new("user1"))
})
