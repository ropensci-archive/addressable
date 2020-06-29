host <- Host$new("gmail.com")

test_that("Local", {
  x <- Local$new("very.common", list(), host)

  expect_is(x, "Local")
  expect_is(x, "R6")

  # some variables
  expect_is(x$BUSINESS_MAILBOXES, "character")
  expect_is(x$NETWORK_MAILBOXES, "character")
  expect_is(x$SPECIAL_MAILBOXES, "character")
  expect_is(x$config, "Config")
  expect_is(x$config$config, "list")
  
  # some functions
  expect_is(x$set_local, "function")
  expect_is(x$parse, "function")
  expect_is(x$parse_comment, "function")
  expect_is(x$parse_tag, "function")
  expect_is(x$valid, "function")

  expect_true(x$valid())
  expect_true(x$valid_size())
  expect_true(x$valid_encoding())
  expect_equal(x$munge(), "ve*****")
  expect_equal(x$local, "very.common")
  expect_equal(x$format(), "very.common+NA")
  expect_null(x$error)
  expect_null(x$error_message)
})

# test_that("test address parts: good", {
#   good <- c(
#     r"{prettyandsimple}",
#     r"{very.common}",
#     r"{disposable.style.email.with+symbol}",
#     r"{other.email-with-dash}",
#     r"{"much.more unusual"}",
#     r"{"(comment)very.unusual.@.unusual.com"}",
#     r"{#!$%&'*+-/=?^_`{}|~}",
#     # r"{" "}", # fixme, should be TRUE, but is FALSE
#     r"{"very.(),:;<>[]\\".VERY.\\"very@\\ \\"very\\".unusual"}",
#     r"{"()<>[]:,;@\\\"!#$%&'*+-/=?^_`{}| ~.a"}",
#     r"{token." ".token}",
#     r"{abc."defghi".xyz}"
#   )
#   # res <- c()
#   for (i in good) {
#     expect_true(Local$new(i, list(), host)$is_standard())
#   }
# })

# test_that("test address parts: good", {
#   bad <- c(
#     r"{A@b@c}",
#     r"{a"b(c)d,e:f;g<h>i[j\k]l}",
#     r"{just"not"right}",
#     r"{this is"not\allowed}",
#     r"{this\ still\"not\\allowed}",
#     r"{john..doe}",
#     r"{ invalid}",
#     r"{invalid }",
#     r"{abc"defghi"xyz}"
#   )
#   res <- c()
#   for (i in bad) {
#     res <- c(res, Local$new(i, list(), host)$is_standard())
#   }
# })

test_that("Local fails", {
  # cant be empty
  expect_error(Local$new())
})
