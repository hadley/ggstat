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

