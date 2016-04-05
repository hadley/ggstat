#' Count unique observations (vector).
#'
#' This function is very similar to table except that: it counts missing values
#' if present, can use weights, only does 1d, returns a 2 column data frame
#' instead of a named vector, and is much much faster.
#'
#' @param x A logical vector, a factor, a double or integer vector (or
#'   S3 object with \code{\link{restore}()} method), or a character vector.
#' @param w Optionally, a vector of weights. If present, weights are summed
#'   instead of counting observations. In other words, the default behaviour
#'   is to assign weight 1 to each point.
#' @export
#' @keywords internal
#' @return A data frame with columns:
#'  \item{x_}{value (same type as \code{x})}
#'  \item{count_}{number of observations/sum of weights (numeric)}
#' @examples
#' compute_count_vec(sample(100, 1e4, rep = TRUE))
#' compute_count_vec(sample(c(TRUE, FALSE, NA), 1e4, rep = TRUE))
compute_count_vec <- function(x, w = NULL) {
  if (is.null(w)) {
    w <- numeric(0)
  }

  if (is.factor(x)) {
    out <- count_factor(x, w)
  } else if (is.logical(x)) {
    out <- count_lgl(x, w)
  } else if (typeof(x) %in% c("double", "integer")) {
    out <- count_numeric(x, w)
    out$x_ <- restore(x, out$x_)
  } else if (is.character(x)) {
    out <- count_string(x, w)
  }

  `as.data.frame!`(out, length(out$x_))
  out
}
