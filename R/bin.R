#' Bin a continuous vector.
#'
#' Bin a numeric vector and count how many observations fall in each bin.
#' Supports weights so that you can re-bin pre-binned data.
#'
#' @section Floating point:
#' If a point is less than \code{binwidth} / 10^8 from the boundary between
#' two bins, it is shifted to fall in the bin with the closest "closed" side.
#'
#' @param x A numeric vector to guess parameters from.
#' @param width (Positive real). The width of a bin. For S3 objects, the
#'   interpretation of width depends on the interpretation
#'   of the underlying numeric vector. For example, for dates, 1 = 1 day;
#'   for times 1 = 1 second; and for difftime, the units vary.
#'
#'   If \code{NULL}, the \code{width} will be derived from the data,
#'   picking approximately \code{bins} bins with nice widths. You should always
#'   override this value, exploring multiple widths to find the best to
#'   illustrate the stories in your data.
#' @param bins Number of bins to use if not specified. Pretty bin sizes are
#'   preferred over matching this value exactly.
#' @param boundary,center Set the position of the first bin by specifying
#'   the position of either a boundary or the center of a bin.
#'   For example, you can always center the bins on integers with
#'   \code{center = 0} regardless of where the first bin actually falls.
#'
#'   Think of binning as tiling the real line into a infinite sequence of
#'   intervals. \code{center} and \code{boundary} set the position of
#'   one of those intervals.
#' @param origin,terminus The locations of the left-most and right-most bins.
#'   Any values outside this range will be treated as missing. You should
#'   usually leave \code{origin} as \code{NULL} so that it is automatically
#'   computed from \code{center} and \code{boundary}.
#' @param closed One of \code{"right"} or \code{"left"} indicating whether the
#'   bin interval is left-closed (i.e. [a, b)), or right-closed (i.e. (a, b]).
#' @param pad If \code{TRUE}, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0 outside the range of x. Defaults to \code{FALSE}.
#' @param breaks A numeric vector of break points.
#' @examples
#' x <- runif(1e6)
#' compute_stat(bin_fixed(x), x)
#' compute_stat(bin_fixed(x, width = 0.25), x)
#' compute_stat(bin_breaks(c(0, 0.1, 0.9, 1)), x)
#'
#' # Can also create fixed bins without data, if you supply the origin
#' # terminus, and width
#' bin_fixed(origin = 0, terminus = 1, width = 0.25)
#'
#' bin_fixed(x, bins = 37)
#'
#' # Bin other types of object
#' x1 <- Sys.time() + runif(1000) * 60
#' compute_stat(bin_date(x1), x1)
#' x2 <- Sys.Date() + sample(30, 10)
#' compute_stat(bin_date(x2), x2)
#'
#' # For fixed bin width, performance scales linearly with the size of x.
#' x <- runif(1e7)
#' system.time(compute_stat(bin_fixed(x, width = 1e-1), x))
#' system.time(compute_stat(bin_fixed(x, width = 1e-2), x))
#' system.time(compute_stat(bin_fixed(x, width = 1e-5), x))
#'
#' # For arbitrary breaks, performance scales linearly with x and
#' # logarthmically with the number of bins.
#' system.time(compute_stat(bin_breaks(seq(0, 1, length = 10)), x))
#' system.time(compute_stat(bin_breaks(seq(0, 1, length = 100)), x))
#' system.time(compute_stat(bin_breaks(seq(0, 1, length = 1000)), x))
#' @name bin
NULL

#' @export
#' @rdname bin
bin_fixed <- function(x, width = NULL, center = NULL, boundary = NULL,
                      origin = NULL, terminus = NULL, bins = 30, pad = FALSE,
                      closed = c("right", "left")) {

  right_closed <- identical(match.arg(closed), "right")

  if (!is.null(origin) && !is.null(terminus)) {
    range <- c(origin, terminus)
  } else {
    range <- frange(x)
  }

  if (is.null(width)) {
    width <- guess_width(range, bins)
  } else {
    if (!is.numeric(width) || length(width) != 1 || width <= 0) {
      stop("`width` must be a single positive number", call. = FALSE)
    }
  }
  width <- as.numeric(width)

  if (is.null(origin)) {
    range[1] <- guess_origin(range, width, right_closed, center = center,
      boundary = boundary)
  } else {
    if (!is.numeric(origin) || length(origin) != 1) {
      stop("`origin` must be a single number.")
    }
  }

  # To pad, extend range one bin past min and max
  if (pad) {
    range <- range + c(-1, 1) * width
  }

  structure(
    list(
      width = width,
      range = range,
      right_closed = right_closed
    ),
    class = "bin_fixed"
  )
}

#' @export
compute_stat.bin_fixed <- function(params, x, y, z, w = NULL) {
  count_fixed(x,
    w =            w %||% numeric(),
    min =          params$range[1],
    max =          params$range[2],
    width =        params$width,
    right_closed = params$right_closed
  )
}

#' @export
#' @rdname bin
bin_breaks <- function(breaks, closed = c("right", "left")) {
  structure(
    list(
      breaks = breaks,
      right_closed = identical(match.arg(closed), "right")
    ),
    class = "bin_breaks"
  )
}

#' @export
#' @rdname bin
bin_date <- function(x, bins = 30, closed = c("right", "left")) {
  breaks <- pretty(x, bins)
  bin_breaks(breaks, closed = closed)
}

#' @export
compute_stat.bin_breaks <- function(params, x, y, z, w = NULL) {
  count_variable(x,
    w =            w %||% numeric(),
    breaks =       params$breaks,
    right_closed = params$right_closed
  )
}

# Guess individual parameters --------------------------------------------------

# Find the left side of left-most bin
guess_origin <- function(x, width, right_closed, center = NULL, boundary = NULL) {
  if (empty_range(x)) {
    return(-width / 2)
  }

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

  shift <- floor((x[1] - boundary) / width)
  origin <- boundary + shift * width

  # Left-open, so need to need to add extra bin if FP-close
  if (right_closed && (x[1] - origin) < 1e-8) {
    origin <- origin - width
  }

  # notify_guess(origin)
  origin
}

guess_width <- function(x, n = 30) {
  if (empty_range(x)) {
    return(1)
  }

  bounds <- pretty(x, n)
  width <- bounds[2] - bounds[1]
  notify_guess(width, paste0("range / ", length(bounds) - 1))

  if (inherits(width, "difftime")) {
    if (inherits(x, "Date")) {
      width <- as.numeric(width, unit = "days")
    } else {
      width <- as.numeric(width, unit = "secs")
    }
  }

  width
}
