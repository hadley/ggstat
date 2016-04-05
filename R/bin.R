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

  out
}

param_bin <- function(x_range, width = NULL, center = NULL, boundary = NULL,
                       right_closed = TRUE) {
  UseMethod("param_bin")
}

#' @export
param_bin.NULL <- function(x_range, width = NULL, center = NULL,
                            boundary = NULL, right_closed = TRUE) {

  width <- width %||% 1
  if (!is.null(boundary)) {
    origin <- boundary
  } else if (!is.null(center)) {
    origin <- center - width / 2
  } else {
    origin <- width / 2
  }

  list(width = width, origin = origin)
}

#' @export
param_bin.numeric <- function(x_range, width = NULL, center = NULL,
                               boundary = NULL, right_closed = TRUE) {
  stopifnot(length(x_range) == 2)

  if (empty_range(x_range)) {
    return(param_bin.NULL(width = width, center = center,
      boundary = boundary))
  }

  width <- width %||% pretty_width(x_range)
  origin <- find_origin(x_range, width, right_closed,
    center = center, boundary = boundary)

  list(width = width, origin = origin)
}

#' @export
param_bin.Date <- function(x_range, width = NULL, center = NULL,
                            boundary = NULL, right_closed = TRUE) {
  param_bin.numeric(
    as.numeric(x_range),
    if (is.null(width)) NULL else as.numeric(width),
    if (is.null(center)) NULL else as.numeric(center),
    if (is.null(boundary)) NULL else as.numeric(boundary),
    right_closed = right_closed
  )
}

#' @export
param_bin.POSIXct <- function(x_range, width = NULL, center = NULL,
                               boundary = NULL, right_closed = TRUE) {
  stopifnot(length(x_range) == 2)

  if (empty_range(x_range)) {
    return(param_bin.NULL(width = width, center = center,
      boundary = boundary))
  }

  # Period object from lubridate package - need lubridate::as.difftime to find
  # the correct generic, instead of base::as.difftime.
  if (is.null(width)) {
    width <- as.numeric(pretty_width(x_range), units = "secs")
  } else if (is(width, "Period")) {
    width <- as.numeric(lubridate::as.difftime(width, units = "secs"))
  } else {
    width <- as.numeric(width, units = "secs")
  }

  param_bin.numeric(
    as.numeric(x_range),
    width,
    if (is.null(center)) NULL else as.numeric(center),
    if (is.null(boundary)) NULL else as.numeric(boundary),
    right_closed = right_closed
  )
}

pretty_width <- function(x, n = 30) {
  bounds <- pretty(x, 30)
  width <- bounds[2] - bounds[1]
  notify_guess(width, paste0("range / ", length(bounds) - 1))
  width
}

# Find the left side of left-most bin
find_origin <- function(x_range, width, right_closed, center = NULL, boundary = NULL) {
  if (!is.null(boundary) && !is.null(center)) {
    stop("Only one of 'boundary' and 'center' may be specified.")
  }
  if (is.null(boundary)) {
    if (is.null(center)) {
      # If neither boundary nor center given, use tile layer's algorithm.
      # This puts min and max of data in outer half of their bins.
      boundary <- width / 2
    } else {
      # If center given but not boundary, compute boundary.
      boundary <- center - width / 2
    }
  }

  shift <- floor((x_range[1] - boundary) / width)
  origin <- boundary + shift * width

  # Left-open, so need to need to add extra bin if FP-close
  if (right_closed && (x_range[1] - origin) < 1e-8) {
    origin <- origin - width
  }

  origin
}

