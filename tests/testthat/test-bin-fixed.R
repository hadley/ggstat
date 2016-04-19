context("bin_fixed")

# Setting boundary/center ------------------------------------------------------

test_that("can't set both boundary and center", {
  expect_error(bin_fixed(c(0, 100), boundary = 0, center = 0), "Only one")
})

test_that("boundary sets edge of bin", {
  p <- bin_fixed(c(1, 30), boundary = 0)
  expect_equal(p$range[1], 0)

  p <- bin_fixed(c(1, 30), boundary = 10)
  expect_equal(p$range[1], 0)
})

test_that("center sets middle of bin", {
  p <- bin_fixed(c(0, 30), center = 0)
  expect_equal(p$range[1], -0.5)

  p <- bin_fixed(c(0, 30), center = 10)
  expect_equal(p$range[1], -0.5)
})

test_that("origin sets edge of bin", {
  p <- bin_fixed(c(0, 30), origin = 1)
  expect_equal(p$range[1], 1)
})

test_that("left-open needs extra bin", {
  origin_01 <- function(...) bin_fixed(..., boundary = 0, width = 1)$range[1]

  expect_equal(origin_01(0:1), -1)
  expect_equal(origin_01(0:1 + 1e-9), -1)

  expect_equal(origin_01(0:1, closed = "left"), 0)
  expect_equal(origin_01(0:1 + 1e-9, closed = "left"), 0)
})

# Other numeric types ----------------------------------------------------------

test_that("treats dates like numbers", {
  x <- as.Date("2013-06-01") + c(0, 30)

  p <- bin_fixed(x, boundary = x[[1]])
  expect_equal(p$range[1], x[[1]] - 1)
  expect_equal(p$width, 1)
})

test_that("treats times like numbers", {
  x <- as.POSIXct('2001-06-01 21:00', tz = 'UTC') + c(0, 30 * 24 * 60 * 60)

  p <- bin_fixed(x, boundary = x[[1]])
  expect_equal(p$range[1], x[[1]] - 24 * 60 * 60)
  expect_equal(p$width, 24 * 60 * 60)
})


# Guessing width ---------------------------------------------------------------

test_that("Automatic width", {
  # Approximately 30 bins, at round numbers -> width 1
  expect_equal(bin_fixed(c(0, 25.0))$width, 1)
  expect_equal(bin_fixed(c(1L, 25L))$width, 1)

  expect_equal(bin_fixed(c(0, 50))$width, 2)
  expect_equal(bin_fixed(c(1L, 50L))$width, 2)
})


# Computation -------------------------------------------------------------

test_that("only NA, one row of output", {
  x <- as.numeric(c(NA, NA, NA, NA))
  binned <- compute_stat(bin_fixed(x, origin = 0), x)
  expect_equal(binned$count_, 4)
  expect_equal(binned$xmin_, NA_real_)
})

test_that("empty vector gives 0 row output with correct types", {
  x <- numeric()
  out <- compute_stat(bin_fixed(x), x)

  expect_equal(out$xmin_,  NA_real_)
  expect_equal(out$xmax_,  NA_real_)
  expect_equal(out$count_, 0)
})

test_that("weights are added", {
  x <- 10:1
  binned <- compute_stat(bin_fixed(x, width = 1), x, w = 10:1)
  expect_equal(binned$count_, c(0, 1:10))
})

