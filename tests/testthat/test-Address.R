test_that("Address", {
  x <- Address$new("User+tag@example.com")

  expect_is(x, "Address")
  expect_is(x, "R6")
  expect_is(x$munge, "function")
  expect_is(x$normal, "function")
  expect_is(x$valid, "function")
  expect_true(x$valid())
  expect_null(x$fail())
  expect_equal(x$local$local, "user+tag")
  expect_equal(x$host$name(), "example.com")
  expect_equal(x$munge(), "us*****@ex*****")

  expect_equal(x$mailbox, "user")
  expect_equal(x$left, "user+tag")
  expect_equal(x$tag, "tag")
})

test_that("Address fails", {
  x <- Address$new("user1")

  expect_is(x, "Address")
  expect_false(x$valid())
  expect_equal(x$fail(), "Invalid Domain Name")
})

test_that("Address forms", {
  x <- Address$new("User+tag@example.com")

  # expect_equal(x$to_s(), "user+tag@example.com")
  expect_equal(x$base(), "user@example.com")
  expect_equal(x$canonical(), "user@example.com")
  # expect_equal(x$redact(), "{63a710569261a24b3766275b7000ce8d7b32e2f7}@example.com")
  # expect_equal(x$redact("md5"), "{b58996c504c5638798eb6b511e6f49af}@example.com")
  expect_equal(x$reference(), "b58996c504c5638798eb6b511e6f49af")
  # expect_equal(EmailAddress.reference("Gmail.User+tag@gmail.com"), "6bdd00c53645790ad9bbcb50caa93880")
})
