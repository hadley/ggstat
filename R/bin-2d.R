#' Bin in 2d
#'
#' Each argument can either be \code{NULL}, a vector of length 1 (which
#' will be used for both x and y directions), or a vector of length 2.
#'
#' @param x,y Numeric vectors to guess parameters from.
#' @inheritParams bin_fixed
#' @export
#' @examples
#' bin_2d_fixed(runif(100), runif(100))
#' bin_2d_fixed(runif(100), runif(100), origin = 0)
#' bin_2d_fixed(runif(100), runif(100), width = 0.25)
#' bin_2d_fixed(runif(100), runif(100), bins = c(10, 100))
#'
#' x <- runif(1e6)
#' y <- runif(1e6)
#' bins <- bin_2d_fixed(x, y)
#' compute_stat(bins, x, y)
#'
bin_2d_fixed <- function(x, y,
                         width = NULL, center = NULL, boundary = NULL,
                         origin = NULL, terminus = NULL, bins = 30, pad = FALSE,
                         closed = NULL) {

  width <- dual_arg(width)
  center <- dual_arg(center)
  origin <- dual_arg(origin)
  terminus <- dual_arg(terminus)
  bins <- dual_arg(bins)
  pad <- dual_arg(pad)
  closed <- dual_arg(closed %||% "right")

  bin_x <- bin_fixed(
    x,
    width = width[[1]],
    center = center[[1]],
    boundary = boundary[[1]],
    origin = origin[[1]],
    terminus = terminus[[1]],
    bins = bins[[1]],
    pad = pad[[1]],
    closed = closed[[1]]
  )
  bin_y <- bin_fixed(
    y,
    width = width[[2]],
    center = center[[2]],
    boundary = boundary[[2]],
    origin = origin[[2]],
    terminus = terminus[[2]],
    bins = bins[[2]],
    pad = pad[[2]],
    closed = closed[[2]]
  )

  structure(
    list(
      x = bin_x,
      y = bin_y
    ),
    class = "bin_2d_fixed"
  )
}

#' @export
#' @rdname compute_stat
compute_stat.bin_2d_fixed <- function(params, x, y, ..., w = NULL) {
  count_2d_fixed(x, y,
    w =              w %||% numeric(),
    min_x =          params$x$range[1],
    min_y =          params$y$range[1],
    max_x =          params$x$range[2],
    max_y =          params$y$range[2],
    width_x =        params$x$width,
    width_y =        params$y$width,
    right_closed_x = params$x$right_closed,
    right_closed_y = params$y$right_closed
  )
}


dual_arg <- function(x) {
  if (is.null(x)) {
    list(NULL, NULL)
  } else if (is_vector(x)) {
    if (length(x) == 1) {
      rep(x, 2)
    } else if (length(x) == 2) {
      x
    } else {
      stop("Vector must be length 1 or 2", call. = FALSE)
    }
  } else {
    stop("Unknown input type", call. = FALSE)
  }
}
