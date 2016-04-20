#' Bin hexagons.
#'
#' @inheritParams bin_2d_fixed
#' @inheritParams bin_fixed
#' @export
#' @examples
#' bin_hex(runif(100), runif(100))
#' bin_hex(runif(100), runif(100), origin = 0)
#' bin_hex(runif(100), runif(100), width = 0.25)
#' bin_hex(runif(100), runif(100), bins = c(10, 100))
#'
#' mat <- MASS::mvrnorm(1e5, mu = c(0, 0), matrix(c(1,0.5,0.5,1),2,2))
#' bins <- bin_hex(c(-3, 3), c(-3, 3))
#' system.time(out <- compute_stat(bins, mat[, 1], mat[, 2]))
#' plot(out)
bin_hex <- function(x, y, width = NULL, center = NULL, boundary = NULL,
                    origin = NULL, bins = 30, pad = FALSE) {
  width <- dual_arg(width)
  center <- dual_arg(center)
  boundary <- dual_arg(boundary)
  origin <- dual_arg(origin)
  bins <- dual_arg(bins)
  pad <- dual_arg(pad)

  bin_x <- bin_fixed(
    x,
    width = width[[1]],
    center = center[[1]],
    boundary = boundary[[1]],
    origin = origin[[1]],
    bins = bins[[1]],
    pad = pad[[1]]
  )
  bin_y <- bin_fixed(
    y,
    width = width[[2]],
    center = center[[2]],
    boundary = boundary[[2]],
    origin = origin[[2]],
    bins = bins[[2]],
    pad = pad[[2]]
  )

  structure(
    list(
      x = bin_x,
      y = bin_y
    ),
    class = "bin_hex"
  )
}

#' @export
#' @rdname compute_stat
compute_stat.bin_hex <- function(params, x, y, ..., w = NULL) {
  out <- count_2d_hex(x, y,
    w =              w %||% numeric(),
    min_x =          params$x$range[1],
    min_y =          params$y$range[1],
    max_x =          params$x$range[2],
    max_y =          params$y$range[2],
    width_x =        params$x$width,
    width_y =        params$y$width
  )

  attr(out, "width") <- params$x$width
  attr(out, "height") <- params$y$width

  class(out) <- c("tbl_hex", class(out))
  out
}

#' @noRd
#' @examples
#' hex_coord(c(0, 1, 0.5), c(0, 0, 1), 1, 1)
#' plot(hex_coord(c(0, 1, 0.5), c(0, 0, 1), 1, 1))
hex_coord <- function(x, y, width, height) {
  hex_x <- rep(x, each = 7) + c(0,   1/2,  1/2,    0, -1/2, -1/2, NA) * width
  hex_y <- rep(y, each = 7) + c(1/2, 1/6, -1/6, -1/2, -1/6,  1/6, NA) * height * 1.5

  structure(cbind(hex_x, hex_y), class = "hex_coord")
}

#' @export
plot.hex_coord <- function(x, ...) {
  plot(x[, 1], x[, 2], type = "n", xlab = "", ylab = "")
  polygon(x)
}

#' @export
plot.tbl_hex <- function(x, ...) {
  x <- x[!is.na(x$x_) & !is.na(x$y_), , drop = FALSE]

  hexes <- hex_coord(x$x_, x$y_, attr(x, "height"), attr(x, "width"))

  n <- rescale01(x$count_)
  bg <- grey(1 - n)
  fg <- ifelse(n > 0.5, "white", "black")


  old <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old))

  plot(hexes[, 1], hexes[, 2], type = "n", )
  polygon(hexes, border = "grey50", col = bg)
  text(x$x_, x$y_, x$count_, cex = 0.5, col = fg)
}


# D. B. Carr, R. J. Littlefield, W. L. Nicholson, and J. S. Littlefield.
# Scatterplot matrix techniques for large N. Journal of the American Statistical
# Association, 82(398):424–436, 1987.
#
# /\
# ||
# \/
#
# "Binning algorithms are available for various lattices in dimensions 2-8
# (Conway and Sloane 1982). The following subroutine is a fast FORTRAN
# implementation of hexagonal binning. The key observation is that hexagon
# centers fall on two staggered lattices whose cells are rectangles. Presuming
# the long side of the rectangle is in the y direction, dividing the y
# coordinates by square root (3) [SQRT(3)] makes the cells square. Thus the
# algorithm uses two lattices with square cells. The first lattice has points
# at the integers with [0, 0] as the lower left value. The second lattice is
# shifted so that the lower left value is at [.5 , .5]. The x and y vectors
# are scaled into [0, SIZE] and [0, SIZE / SQRT(3)], respectively. SIZE
# determines the portions of the lattices that are used. For each data point,
# binning consists of finding one candidate lattice point from each lattice
# and then selecting the nearest of the two candidates."
