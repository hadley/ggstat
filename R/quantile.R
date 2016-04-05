#' Compute quantiles (vectorised).
#'
#' @export
#' @keywords internal
#' @examples
#' compute_quantile_vec(1:100)
#'
#' # Specify weights with second argument
#' compute_quantile_vec(1:100, 100:1)
compute_quantile_vec <- function(x, w = NULL, probs = seq(0, 1, 0.25)) {
  if (!is.numeric(x)) stop("x must be numeric", call. = FALSE)

  if (is.null(w)) {
    q <- quantile(x, probs, na.rm = FALSE, names = FALSE)
  } else {
    q <- weightedQuantile(x, w, probs)
  }

  q <- restore(x, q)

  data.frame(
    x_ = q,
    quantile_ = probs
  )
}
