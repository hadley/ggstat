#' Restore S3 class properties.
#'
#' When an S3 class is built on top of an atomic vector, it's often resonable
#' to do some computation on the underlying atomic vector. This generic allows
#' you to restore important attributes from the original vector to the new
#' vector, so that it continues to behave as expected.
#'
#' @param old Old S3 vector
#' @param new New S3 vector. Should be same \code{\link{typeof}()} as
#'   \code{old}.
#' @export
#' @examples
#' restore(Sys.Date(), 1)
#' restore(Sys.time(), 1)
#'
#' dt <- difftime(Sys.time(), Sys.time() - 100)
#' restore(dt, 10)
restore <- function(old, new) {
  UseMethod("restore")
}

#' @export
restore.Date <- function(old, new) {
  class(new) <- "Date"
  new
}

#' @export
restore.POSIXct <- function(old, new) {
  class(new) <- c("POSIXct", "POSIXt")
  attr(new, "TZ") <- attr(old, "TZ")
  new
}

#' @export
restore.difftime <- function(old, new) {
  class(new) <- c("difftime")
  attr(new, "units") <- attr(old, "units")
  new
}

#' @export
restore.numeric <- function(old, new) {
  new
}
