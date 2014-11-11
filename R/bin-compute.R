#' Bin continuous data into equal sized ranges.
#'
#' @param data Dataset-like object to bin. Built-in methods for data frames,
#'   grouped data frames and ggvis visualisations.
#' @param x_var,w_var Names of x and weight variables. The x variable must be
#'   continuous.
#' @inheritParams compute_bin_vec
#' @seealso \code{\link{compute_count}} For counting cases at specific locations
#   of a continuous variable. This is useful when the variable is continuous
#   but the data is granular.
#' @return A data frame with columns:
#'  \item{count_}{the number of points}
#'  \item{x_}{mid-point of bin}
#'  \item{xmin_}{left boundary of bin}
#'  \item{xmax_}{right boundary of bin}
#' @export
#' @examples
#' mtcars %>% compute_bin(~mpg)
#' mtcars %>% compute_bin(~mpg, width = 10)
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   mtcars %>% dplyr::group_by(cyl) %>% compute_bin(~mpg, width = 10)
#' }
#'
#' # Missing values get own bin
#' mtcars2 <- mtcars
#' mtcars2$mpg[sample(32, 5)] <- NA
#' mtcars2 %>% compute_bin(~mpg, width = 10)
compute_bin <- function(data, x_var, w_var = NULL, width = NULL,
                        center = NULL, boundary = NULL,
                        closed = c("right", "left"), pad = FALSE) {
  closed <- match.arg(closed)

  UseMethod("compute_bin")
}

#' @export
compute_bin.data.frame <- function(data, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = FALSE) {
  x_val <- eval_vector(data, x_var)
  w_val <- eval_vector(data, w_var)

  compute_bin_vec(
    x_val,
    w = w_val,
    width = width,
    center = center,
    boundary = boundary,
    closed = closed,
    pad = pad
  )
}

#' @export
compute_bin.grouped_df <- function(data, x_var, w_var = NULL, width = NULL,
                                   center = NULL, boundary = NULL,
                                   closed = c("right", "left"), pad = FALSE) {

  # We want to use the same boundary and width across groups, so calculate
  # bin params here.
  x_val <- eval_vector(data, x_var)
  params <- param_bin(frange(x_val), width = width, center = center,
    boundary = boundary, right_closed = identical(closed, "right"))

  dplyr::do(data, compute_bin(.,
    x_var = x_var,
    w_var = w_var,
    width = params$width,
    boundary = params$origin,
    closed = params$closed,
    pad = pad
  ))
}

globalVariables(".")


#' Bin continuous vector into equal sized ranges.
#'
#' Bin a numeric vector and count how many observations fall in each bin.
#' Supports weights so that you can re-bin pre-binned data.
#'
#' @section Floating point:
#' If a point is less than \code{binwidth} / 10^8 from the boundary between
#' two bins, it is shifted to fall in the bin with the closest "closed" side.
#'
#' @param x A numeric vector to bin.
#'
#'   You can also bin S3 objects that are build on top of integer and
#'   double atomic vectors, as long as there is a method for
#'   \code{\link{restore}()}.
#' @param w If specified, an numeric vector (of the same length as \code{x})
#'   giving weights. If weights are provided, the weights in each bin are
#'   summed, rather than just counting the number of observations.
#' @param width (Positive real). The width of a bin. For S3 objects, the
#'   interpretation of width depends on the interpretation
#'   of the underlying numeric vector. For example, for dates, 1 = 1 day;
#'   for times 1 = 1 second; and for difftime, the units vary.
#'
#'   If \code{NULL}, the \code{width} will be derived from the data,
#'   picking approximately 30 bins with nice widths. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#' @param boundary,center Set the position of the first bin by specifying
#'   the position of either a boundary or the center of a bin.
#'   For example, you can always center the bins on integers with
#'   \code{center = 0} regardless of where the first bin actually falls.
#'
#'   Think of binning as tiling the real line into a infinite sequence of
#'   intervals. \code{center} and \code{boundary} set the position of
#'   one of those intervals.
#' @param origin The location of the left-most bin edge. Any values smaller
#'   than the \code{origin} will be treated as if they are missing. If
#'   \code{NULL} will be computed from \code{center} and \code{boundary}.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether the
#'   bin interval is left-closed (i.e. [a, b)), or right-closed (i.e. (a, b]).
#' @param pad If \code{TRUE}, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0 outside the range of x. Defaults to \code{FALSE}.
#' @keywords internal
#' @export
#' @examples
#' x <- runif(1e6)
#' compute_bin_vec(x)
#' compute_bin_vec(x, width = 0.25)
#'
#' # Bin other types of object
#' compute_bin_vec(Sys.time() + runif(10) * 60, 15)
#' compute_bin_vec(Sys.Date() + sample(30, 10), 7)
#'
#' # Performance scales linearly with the size of x, and the number
#' # of bins has limited impact
#' x <- runif(1e7)
#' system.time(compute_bin_vec(x, width = 0.1))
#' system.time(compute_bin_vec(x, width = 1 / 100))
#' system.time(compute_bin_vec(x, width = 1 / 1e5))
compute_bin_vec <- function(x, w = NULL, width = NULL, origin = NULL,
                            center = NULL, boundary = NULL,
                            closed = c("right", "left"), pad = FALSE) {
  stopifnot(is.atomic(x), typeof(x) %in% c("double", "integer"), !is.factor(x))
  closed <- match.arg(closed)

  if (length(w) == 0) {
    w <- numeric()
  }

  right_closed <- identical(closed, "right")
  params <- param_bin(frange(x), width = width, center = center,
    boundary = boundary, right_closed = right_closed)

  out <- condense_count(x,
    w = w,
    origin = origin %||% params$origin,
    width = params$width,
    pad = pad,
    right_closed = right_closed
  )
  out$x_ <- restore(x, out$x_)
  out$xmin_ <- restore(x, out$xmin_)
  out$xmax_ <- restore(x, out$xmax_)

  `as.data.frame!`(out, length(out[[1]]))
  out
}
