#' Fast implementation of range.
#'
#' This is an efficient C++ implementation of range for numeric vectors:
#' it avoids S3 dispatch, and computes both min and max in a single pass
#' through the input.
#'
#' @param x A numeric vector, or an S3 object built on top of integer and
#'   double atomic vectors, as long as there is a method for
#'   \code{\link{restore}()}.
#' @param finite If \code{TRUE} ignores missing values and infinities. Note
#'   that if the vector is empty, or only contains missing values,
#'   \code{frange} will return \code{c(Inf, -Inf)} because those are the
#'   identity values for \code{\link{min}} and \code{\link{max}} respectively.
#' @return A numeric vector of length two. If \code{x} contains no non-missing
#'   values, this will be \code{c(Inf, -Inf)}.
#' @export
#' @examples
#' # frange() always ignores missing values
#' frange(c(1:10, NA))
#'
#' # frange() works with dates and times
#' frange(Sys.Date())
#' frange(Sys.time())
#'
#' # frange() is much faster than range()
#' x <- runif(1e6)
#' system.time(range(x))
#' system.time(frange(x))
frange <- function(x, finite = TRUE) {
  if (is.list(x) && is.vector(x)) {
    return(frange_list(x))
  }

  if (!is_numeric(x)) {
    stop("x must be numeric", call. = FALSE)
  }

  restore(x, frange_(x, finite))
}

empty_range <- function(x) x[1] > x[2]
