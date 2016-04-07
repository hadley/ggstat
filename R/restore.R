#' Restore S3 class properties.
#'
#' When an S3 class is built on top of an atomic vector, it's often resonable
#' compute directly on those underlying values. This generic allows you to
#' restore important attributes from the original vector to the new
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
restore.default <- function(old, new) {
  mostattributes(new) <- attributes(old)
  new
}
