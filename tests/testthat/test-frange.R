context("frange()")

test_that("frange() of empty vector is Inf, -Inf", {
  expect_equal(frange(numeric()), c(Inf, -Inf))
})

test_that("frange() of date is date", {
  x <- Sys.Date() + 0:1

  expect_is(frange(x), "Date")
  expect_equal(frange(x), x)
})

test_that("frange() of time is time", {
  x <- Sys.time() + 0:1

  expect_is(frange(x), "POSIXct")
  expect_equal(frange(x), x)
})

test_that("frange() of factor is error", {
  x <- factor(letters)

  expect_error(frange(x), "x must be numeric")
})
