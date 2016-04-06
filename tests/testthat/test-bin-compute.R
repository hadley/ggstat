context("Bin: compute")

test_that("only NA, one row of output", {
  x <- as.numeric(c(NA, NA, NA, NA))
  binned <- compute_bin_vec(x, origin = 0)
  expect_equal(binned$count_, 4)
  expect_equal(binned$xmin_, NA_real_)
})

test_that("empty vector gives 0 row output with correct types", {
  out <- compute_bin_vec(numeric())

  expect_is(out$xmin_, "numeric")
  expect_is(out$xmax_, "numeric")
  expect_is(out$count_, "numeric")
})

test_that("weights are added", {
  binned <- compute_bin_vec(1:10, 1:10, width = 1)
  expect_equal(binned$count_, c(0, 1:10))
})

