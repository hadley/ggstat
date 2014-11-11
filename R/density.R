#' Compute density (vector)
#'
#' @keywords internal
#' @export
#' @examples
#' x <- runif(10)
#' d <- compute_density_vec(x)
#' plot(d$x_, d$density_, type = "l")
#' lines(stats::density(x, bw = 0.02), col = "red")
#'
#' x <- runif(1e6)
#' system.time(stats::density(x, bw = 0.02 / 3))
#' system.time(compute_density_vec(x, bw = 0.02 / 3))
compute_density_vec <- function(x, w = NULL, bw = NULL, width = NULL,
                                range = NULL, reflect = TRUE) {

  if (is.null(w)) {
    w <- numeric(0)
  }

  params <- param_density(frange(x), bw = bw, width = width, range = range,
    reflect = reflect)
  out <- density(x, w, bw = params$bw, width = params$width,
    from = params$range[1], to = params$range[2], reflect = reflect)


  out$x_ <- restore(x, out$x_)
  `as.data.frame!`(out, length(out[[1]]))

  out
}

param_density <- function(x_range, bw = NULL, width = NULL, range = NULL,
                          reflect = TRUE) {

  if (is.null(bw)) {
    bw <- pretty_width(x_range)
  }

  # Default to 10 points across binwidth
  if (is.null(width)) {
    width <- bw / 10
  }

  if (is.null(range)) {
    if (reflect) {
      range <- x_range
    } else {
      range <- x_range + bw * c(-1, 1)
    }
  }

  list(bw = bw, width = width, range = range)
}
