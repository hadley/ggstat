context("count")

test_that("empty factor levels preserved", {
  x <- factor(c("A", "A", "C"), levels = LETTERS[1:3])
  out <- compute_count_vec(x)
  expect_equal(out$x_, factor(LETTERS[1:3]))
  expect_equal(out$count_, c(2, 0, 1))
})

test_that("order of levels preserved", {
  x <- factor(c("A", "A", "B"), levels = c("B", "A"))
  out <- compute_count_vec(x)
  expect_equal(out$x_, factor(LETTERS[2:1], levels = LETTERS[2:1]))
  expect_equal(out$count_, c(1, 2))
})

test_that("missing values in factors are counted", {
  x <- factor(c(NA, NA, "a", "b"))
  out1 <- compute_count_vec(x) # NA in vector
  out2 <- compute_count_vec(factor(x, levels = c(NA, "a", "b"))) # NA in levels

  expect_equal(out1$count_, c(2, 1, 1))
  expect_equal(out2$count_, c(2, 1, 1))
})

test_that("dates preserved", {
  x <- as.Date("2013-07-01") + 1:5
  out <- compute_count_vec(x)
  expect_is(out$x_, "Date")
})

test_that("times preserved", {
  x <- Sys.time() + 1:5
  out <- compute_count_vec(x)
  expect_is(out$x_, "POSIXct")
})

test_that("empty input gives zero-row output for numeric, character & factor", {
  expect_equal(nrow(compute_count_vec(numeric())), 0)
  expect_equal(nrow(compute_count_vec(character())), 0)
  expect_equal(nrow(compute_count_vec(factor())), 0)
})

test_that("empty input gives two-row output for logical", {
  # because logicals are like factors with levels c(T, F)
  expect_equal(nrow(compute_count_vec(logical())), 2)
  expect_equal(nrow(compute_count_vec(factor(levels = c("T", "F")))), 2)
})


test_that("weights are summed", {
  w <- c(10, 1)

  out_lgl <- compute_count_vec(c(TRUE, TRUE), w)
  out_num <- compute_count_vec(c(1, 1), w)
  out_str <- compute_count_vec(c("a", "a"), w)
  out_fac <- compute_count_vec(factor(c("a", "a")), w)

  expect_equal(out_lgl$count_, c(11, 0))
  expect_equal(out_num$count_, 11)
  expect_equal(out_str$count_, 11)
  expect_equal(out_fac$count_, 11)
})


