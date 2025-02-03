#' Get Bandwidth
#'
#' Get the bandwidth of a `"tc_estimates"` object.
#' The length of the filter is then equal to \eqn{2 \times \text{bandwidth}(x) + 1}.
#'
#' @param x a `"tc_estimates"` object.
#' @export
bandwidth <- function(x) {
	UseMethod("bandwidth")
}
#' @export
bandwidth.henderson <- function(x) {
	sfilter <- x$parameters$tc_coef@sfilter
	upper_bound(sfilter)
}
#' @export
bandwidth.clf <- function(x) {
	sfilter <- x$parameters$tc_coef@sfilter
	upper_bound(sfilter)
}
#' @export
bandwidth.robust_henderson<- function(x) {
	U <- x$parameters$U
	length <- nrow(U)
	(length - 1) / 2
}
