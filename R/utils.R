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

#' Get Smoothness of Trend-Cycle estimates
#'
#' Compute the smoothness of trend-cycle estimates using Picard and Matthews (2006) definition.
#' @param x a `"tc_estimates"` object.
#'
#' @details
#' There are several definitions or criteria of smoothness. The measure of smoothness used here is defined as:
#' \deqn{
#' 100 \times \sqrt{
#' \frac{
#' \sum_t [(TC_t - TC_{t-1})/TC_{t-1}]^2
#' }{
#' \sum_t [(SA_t - SA_{t-1})/SA_{t-1}]^2
#' }
#' }
#' }
#' This represents a measure of the month-to-month percentage change in the trend-cycle.
#' The smaller the smoothness number is, the smoother the trend-cycle.
#' @references
#'
#' Picard, Frédéric et Steve Matthews (2016).
#' *The Addition of Trend-Cycle Estimates to Selected Publications at Statistics Canada*.
#' Proceedings of the Survey Methods Section, Statistical Society of Canada (SSC) Annual Meeting. <https://ssc.ca/sites/default/files/imce/pdf/p icard_ssc2016.pdf>.
#' @export
smoothness <- function(x) {
	sa <- x$x
	tc <- x$tc
	100 * sqrt(
		sum((base::diff(tc, 1) / stats::lag(tc, -1))^2) /
			sum((base::diff(sa, 1) / stats::lag(sa, -1))^2)
	)
}
