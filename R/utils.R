#' Get Filter Bandwidth
#'
#' Retrieves the bandwidth of the filter used in a `"tc_estimates"` object.
#'
#' @inheritParams tc_estimates
#'
#' @details
#' The bandwidth corresponds to the number of observations on each side of the central
#' value for a symmetric trend-cycle filter.
#' The total filter length is equal to \eqn{2 \times \text{bandwidth}(x) + 1}{2 * bandwidth(x) + 1}.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- french_ipi[, "manufacturing"]
#' tc_clf <- henderson_smoothing(x)
#' bandwidth(tc_clf)
#'
#' @returns An `integer` representing the bandwidth of the trend-cycle filter.
#'
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

#' Measure Smoothness of Trend-Cycle Estimates
#'
#' Computes the smoothness ratio of trend-cycle estimates relative to the seasonally
#' adjusted series following Picard and Matthews (2016).
#'
#' @inheritParams tc_estimates
#'
#' @details
#' Smoothness is evaluated as the ratio of month-to-month growth rate variances:
#' \deqn{
#' 100 \times \sqrt{
#' \frac{
#' \sum_t \left( \frac{TC_t - TC_{t-1}}{TC_{t-1}} \right)^2
#' }{
#' \sum_t \left( \frac{SA_t - SA_{t-1}}{SA_{t-1}} \right)^2
#' }
#' }
#' }{100 * sqrt( sum( ((TC_t - TC_{t-1}) / TC_{t-1})^2 ) / sum( ((SA_t - SA_{t-1}) / SA_{t-1})^2 ) )}
#'
#' This metric quantifies the relative variability of the trend-cycle compared to
#' the seasonally adjusted series. A lower value indicates a smoother trend-cycle.
#'
#' @references
#' Picard, F., & Matthews, S. (2016). The Addition of Trend-Cycle Estimates
#' to Selected Publications at Statistics Canada. *Proceedings of the Survey
#' Methods Section, Statistical Society of Canada (SSC) Annual Meeting*.
#' \url{https://ssc.ca/sites/default/files/imce/pdf/picard_ssc2016.pdf}
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- french_ipi[, "manufacturing"]
#' tc_clf <- clf_smoothing(x)
#' tc_henderson <- henderson_smoothing(x)
#' smoothness(tc_clf)
#' smoothness(tc_henderson)
#'
#' @returns A numeric value representing the smoothness index of the trend-cycle estimates.
#'
#' @export
smoothness <- function(x) {
	sa <- x$x
	tc <- x$tc
	100 * sqrt(
		sum((base::diff(tc, 1) / stats::lag(tc, -1))^2) /
			sum((base::diff(sa, 1) / stats::lag(sa, -1))^2)
	)
}

#' @noRd
#' @keywords internal
dates_to_num <- function(date, frequency){
	if (length(date) == 2) {
		return (date[1] + (date[2] - 1) / frequency)
	}
	date
}
