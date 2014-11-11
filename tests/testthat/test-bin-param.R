context("Bin: default params")

# Setting boundary/center ------------------------------------------------------

test_that("can't set both boundary and center", {
  expect_error(param_bin(c(0, 100), boundary = 0, center = 0), "Only one")
})

test_that("boundary sets edge of bin", {
  p <- param_bin(c(1, 30), boundary = 0)
  expect_equal(p$origin, 0)

  p <- param_bin(c(1, 30), boundary = 10)
  expect_equal(p$origin, 0)
})

test_that("center sets middle of bin", {
  p <- param_bin(c(0, 30), center = 0)
  expect_equal(p$origin, -0.5)

  p <- param_bin(c(0, 30), center = 10)
  expect_equal(p$origin, -0.5)
})

test_that("left-open needs extra bin", {
  origin_01 <- function(...) param_bin(..., boundary = 0, width = 1)$origin

  expect_equal(origin_01(0:1), -1)
  expect_equal(origin_01(0:1 + 1e-9), -1)

  expect_equal(origin_01(0:1, right_closed = FALSE), 0)
  expect_equal(origin_01(0:1 + 1e-9, right_closed = FALSE), 0)
})

# Other numeric types ----------------------------------------------------------

test_that("treats dates like numbers", {
  x <- as.Date("2013-06-01") + c(0, 30)

  p <- param_bin(x, boundary = x[[1]])
  expect_equal(p$origin, as.numeric(x[[1]]) - 1)
  expect_equal(p$width, 1)
})

test_that("treats times like numbers", {
  x <- as.POSIXct('2001-06-01 21:00', tz = 'America/New_York') + c(0, 30 * 24 * 60 * 60)

  p <- param_bin(x, boundary = x[[1]])
  expect_equal(p$origin, as.numeric(x[[1]]) - 24 * 60 * 60)
  expect_equal(p$width, 24 * 60 * 60)
})

test_that("can specify width as lubridate Periods", {
  x <- as.POSIXct('2001-06-01 21:00', tz = 'UTC') + c(0, 100)
  expect_identical(
    param_bin(x, width = lubridate::ms("1 42")),
    param_bin(x, width = 60 + 42)
  )


})
# width specified as a Period from lubridate



# Guessing width ---------------------------------------------------------------

test_that("Automatic width", {
  # Approximately 30 bins, at round numbers -> width 1
  expect_equal(param_bin(c(0, 25.0))$width, 1)
  expect_equal(param_bin(c(1L, 25L))$width, 1)

  expect_equal(param_bin(c(0, 50))$width, 2)
  expect_equal(param_bin(c(1L, 50L))$width, 2)
})
