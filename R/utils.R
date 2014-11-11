notify_guess <- function(x, explanation = NULL) {
  msg <- paste0(
    "Guessing ", deparse(substitute(x)), " = ", format(x, digits = 3),
    if (!is.null(explanation)) paste0(" # ", explanation)
  )
  message(msg)
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


is_numeric <- function(x) {
  typeof(x) %in% c("double", "integer") && !is.factor(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x


eval_vector <- function(data, x) {
  if (is.atomic(x)) return(rep(x, nrow(data)))

  eval(x[[2]], data, environment(x))
}

plot_init <- function(x, y,
                      xlim = range(x, na.rm = TRUE),
                      ylim = range(y, na.rm = TRUE), ...) {
  old <- par(mar = c(1.5, 1.5, 0, 0), cex = 0.8)
  on.exit(par(old))

  plot.default(xlim, ylim, type = "n", xlab = "", ylab = "", axes = FALSE)
  axis(1, lwd = 0, lwd.ticks = 1, col = "grey80", col.axis = "grey60", padj = -1)
  axis(2, lwd = 0, lwd.ticks = 1, col = "grey80", col.axis = "grey60", padj = 1)
  grid(lty = "solid", col = "grey80")
}

row_apply <- function(df, f, ...) {

  row_slice <- function(df, i) {
    out <- pluck(df, i)
    `as.data.frame!`(out, 1)
    out
  }

  lapply(1:nrow(df), function(i) f(row_slice(df, i), ...))
}

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}
