% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{compute_density_vec}
\alias{compute_density_vec}
\title{Compute density (vector)}
\usage{
compute_density_vec(x, w = NULL, bw = NULL, width = NULL, range = NULL,
  reflect = TRUE)
}
\description{
Compute density (vector)
}
\examples{
x <- runif(10)
d <- compute_density_vec(x)
plot(d$x_, d$density_, type = "l")
lines(stats::density(x, bw = 0.02), col = "red")

x <- runif(1e6)
system.time(stats::density(x, bw = 0.02 / 3))
system.time(compute_density_vec(x, bw = 0.02 / 3))
}
\keyword{internal}

