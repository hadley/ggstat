context("GroupVariable")

test_that("throws error if not enough breaks", {
  expect_error(group_breaks(1:10, numeric()), "`breaks` must have at least one element")
})

test_that("invalid values go bin 0", {
  out <- group_breaks(c(NA, NaN), 1)
  expect_equal(out$x, c(0L, 0L))
})

test_that("data outside ranges go in bin 0", {
  out <- group_breaks(c(-Inf, Inf), 1:2)
  expect_equal(out$x, c(0, 0))
  expect_equal(out$bins$xmin_, c(NA, 1))
  expect_equal(out$bins$xmax_, c(NA, 2))
})

test_that("bin number as expected for middle of bin", {
  # (0, 1], (1, 2], (2, 3]
  out <- group_breaks(c(0.5, 1.5, 2.5), 0:3)
  expect_equal(out$x, c(1, 2, 3))
})

test_that("respects right_closed", {
  # (0, 1], (1, 2], (2, 3]
  out1 <- group_breaks(c(2, 1), 0:3, right_closed = TRUE)
  expect_equal(out1$x, c(2, 1))

  # [0, 1), [1, 2), [2, 3)
  out2 <- group_breaks(c(2, 1), 0:3, right_closed = FALSE)
  expect_equal(out2$x, c(3, 2))
})
