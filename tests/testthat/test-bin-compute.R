context("Bin: compute")

# Missing values ---------------------------------------------------------------

test_that("NAs get own bin", {
  x <- c(1:10, NA, NA, NA, NA)
  binned <- compute_bin_vec(x, width = 100, origin = 0)
  expect_equal(binned$count_, c(4, 10))
  expect_equal(binned$x_, c(NA, 50))
})

test_that("only NA, one row of output", {
  x <- as.numeric(c(NA, NA, NA, NA))
  binned <- compute_bin_vec(x, origin = 0)
  expect_equal(binned$count_, 4)
  expect_equal(binned$x_, NA_real_)
})

# Empty inputs ----------------------------------------------------------------

test_that("empty vector gives 0 row output with correct types", {
  out <- compute_bin_vec(numeric())

  expect_is(out$x_, "numeric")
  expect_is(out$xmin_, "numeric")
  expect_is(out$xmax_, "numeric")
  expect_is(out$count_, "numeric")
})

# Floating point issues --------------------------------------------------------

# Three vectors with a classical FP issue
x1 <- 0:5
x2 <- x1 + (1 - 0.9) - 0.1
x3 <- x1 - (1 - 0.9) + 0.1

test_that("Binning robust to minor FP differences", {
  b1 <- compute_bin_vec(x1, width = 1, origin = 0, closed = "left")
  b2 <- compute_bin_vec(x2, width = 1, origin = 0, closed = "left")
  b3 <- compute_bin_vec(x3, width = 1, origin = 0, closed = "left")

  expect_equal(b1, b2)
  expect_equal(b1, b3)
  expect_equal(b1$count_, rep(1, 6))

  b4 <- compute_bin_vec(x1, width = 1, origin = -1)
  b5 <- compute_bin_vec(x2, width = 1, origin = -1)
  b6 <- compute_bin_vec(x3, width = 1, origin = -1)

  expect_equal(b4, b5)
  expect_equal(b4, b6)
  expect_equal(b4$count_, rep(1, 6))
})

test_that("Sidedness of interval doesn't matter when data far from boundaries", {
  b1 <- compute_bin_vec(x1, width = 1, origin = -0.5, closed = "left")
  b2 <- compute_bin_vec(x2, width = 1, origin = -0.5, closed = "left")
  b3 <- compute_bin_vec(x3, width = 1, origin = -0.5, closed = "left")
  b4 <- compute_bin_vec(x1, width = 1, origin = -0.5)
  b5 <- compute_bin_vec(x2, width = 1, origin = -0.5)
  b6 <- compute_bin_vec(x3, width = 1, origin = -0.5)

  expect_equal(b1, b2)
  expect_equal(b1, b3)
  expect_equal(b1, b4)
  expect_equal(b1, b5)
  expect_equal(b1, b6)
})

# Weights ----------------------------------------------------------------------

test_that("weights are added", {
  binned <- compute_bin_vec(1:10, 1:10, width = 1)
  expect_equal(binned$count_, 1:10)
})

