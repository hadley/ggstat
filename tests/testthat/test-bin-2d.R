context("bin_2d")

test_that("x and y columns are strided correctly", {
  x <- c(1, 1, 0)
  y <- c(0, 0, 1)

  out <- compute_stat(bin_2d_fixed(x, y, width = 0.5), x, y)
  out <- out[out$count_ > 0, , drop = FALSE]

  expect_equal(out$xmin_, c(-0.25,  0.75))
  expect_equal(out$ymin_, c( 0.75, -0.25))
  expect_equal(out$count_, c(2, 1))
})
