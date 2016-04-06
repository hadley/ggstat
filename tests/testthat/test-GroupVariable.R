context("GroupVariable")

test_that("throws error if not enough breaks", {
  expect_error(group_variable(1:10, numeric()), "`breaks` must have at least one element")
})

test_that("invalid values go bin 0", {
  out <- group_variable(c(NA, NaN), 1)
  expect_equal(out$x, c(0L, 0L))
})

test_that("-Inf and Inf go in first and last bins", {
  out <- group_variable(c(-Inf, Inf), 1:2)
  expect_equal(out$x, c(1, 3))
})

test_that("bin number as expected for middle of bin", {
  # (-Inf, 1], (1, 2], (2, Inf)
  out <- group_variable(c(0.5, 1.5, 2.5), 1:2)
  expect_equal(out$x, c(1, 2, 3))
})

test_that("respects right_closed", {
  # (-Inf, 1], (1, 2], (2, Inf)
  out1 <- group_variable(c(2, 1), 1:2, right_closed = TRUE)
  expect_equal(out1$x, c(2, 1))

  # (-Inf, 1), [1, 2), [2, Inf)
  out2 <- group_variable(c(2, 1), 1:2, right_closed = FALSE)
  expect_equal(out2$x, c(3, 2))
})
