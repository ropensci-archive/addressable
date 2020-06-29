test_that("Host", {
  x <- Host$new("example.com")

  expect_is(x, "Host")
  expect_is(x, "R6")

  # some variables
  expect_type(x$MAX_HOST_LENGTH, "double")
  expect_is(x$original, "character")
  expect_equal(x$original, "example.com")
  expect_is(x$DNS_HOST_REGEX, "character")
  expect_is(x$CANONICAL_HOST_REGEX, "character")
  expect_is(x$config, "Config")
  expect_is(x$config$config, "list")
  
  # some functions
  expect_is(x$name, "function")
  expect_is(x$canonical, "function")
  expect_is(x$munge, "function")
  expect_is(x$parse, "function")
  expect_is(x$parse_comment, "function")

  expect_true(x$valid())
  expect_null(x$fail())
  expect_equal(x$munge(), "ex*****")
  expect_equal(x$name(), "example.com")
  expect_equal(x$canonical(), "example.com")
  expect_equal(x$tld, "com")
  expect_equal(x$registration_name, "example")
  expect_null(x$ip_address)
})

test_that("Host with bad address", {
  x <- Host$new("gm ail.com")
  
  expect_false(x$valid())
  expect_equal(x$fail(), "Invalid Domain Name")
})

test_that("Host fails", {
  # cant be empty
  expect_error(Host$new())
})
