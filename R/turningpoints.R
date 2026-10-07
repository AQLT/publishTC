#' Detect Turning Points and Unwanted Ripples in a Time Series
#'
#' Identifies turning points (upturns and downturns) and unwanted short-term
#' ripples in a time series object using local extremum criteria.
#'
#' @param x An input time series (object of class `"ts"`).
#' @param start,end Numeric vectors specifying the start and end of the time interval
#'   (e.g., `c(2020, 1)`) in which to search for turning points.
#' @param digits Integer specifying the number of decimal digits used when comparing
#'   values to determine turning points. Defaults to `NULL` (no rounding).
#' @param k,m Integers specifying the required number of preceding (\eqn{k}) and
#'   succeeding (\eqn{m}) observations to define a local extremum. See details.
#'
#' @details
#' Turning points are identified following the operational definition adapted from
#' Zellner, Hong, and Min (1991), typically with \eqn{k = 3} and \eqn{m = 1}:
#'
#' * An **upturn** occurs at time \eqn{t} if:
#' \deqn{y_{t-k} \ge \dots \ge y_{t-1} < y_t \le y_{t+1} \le \dots \le y_{t+m}}
#'
#' * A **downturn** occurs at time \eqn{t} if:
#' \deqn{y_{t-k} \le \dots \le y_{t-1} > y_t \ge y_{t+1} \ge \dots \ge y_{t+m}}
#'
#' An **unwanted ripple** is defined as any occurrence where two turning points
#' of the same type (two upturns or two downturns) occur within a 10-month window
#' (i.e., short cycles lasting less than 11 months).
#'
#' @references
#' Zellner, A., Hong, C., & Min, C. (1991). Forecasting Turning Points in International
#' Growth Rates using Bayesian Exponential Smoothing Methods.
#' *Journal of Econometrics*, 49(1–2), 275–304.
#' \doi{10.1016/0304-4076(91)90099-H}
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' tc <- henderson_smoothing(french_ipi[, "manufacturing"])
#' turning_points(tc)
#' unwanted_ripples(tc)
#'
#' @returns
#' * `turning_points()` returns a named list with two components: `"upturn"` and
#'   `"downturn"`, each containing a vector of dates (or empty vectors if none found).
#' * `upturn()` and `downturn()` return a vector of dates corresponding to the
#'   detected upturns or downturns, respectively.
#' * `unwanted_ripples()` returns an integer representing the number of unwanted
#'   ripples detected in the series.
#'
#' @export
#' @importFrom stats ts start end time window
#' @importFrom zoo rollapply
#' @rdname turning_points
turning_points <- function(
		x, start = NULL, end = NULL,
		digits = 6, k = 3, m = 1){
	if (is.null(x))
		return(NULL)
	if (is_tc_estimates(x)) {
		x <- x$tc
	}
	list(upturn = upturn(x, start = start, end = end, digits = digits, k = k, m = m),
		 downturn = downturn(x, start = start, end = end, digits = digits, k = k, m = m))
}
#' @rdname turning_points
#' @export
upturn <- function(
		x, start = NULL, end = NULL,
		digits = 6, k = 3, m = 1){
	if(is.null(x))
		return(NULL)
	if(!is.null(digits))
		x <- round(x, digits = digits)
	res <- rollapply(
		x, width=k+m+1,
		function(x){
			# (x[1]>=x[2]) & (x[2]>=x[3]) &
			#   (x[3]<x[4]) & (x[4]<=x[5])
			diff_ <- diff(x)
			all(c(diff_[seq(1, k)] <= 0, # decrease
				  diff_[k+1]> 0, # increase
				  diff_[-seq(1, k+1)] >= 0))
		})
	res <- window(res, start = start, end = end, extend = TRUE)
	res <- time(res)[which(res)]
	res
}
#' @rdname turning_points
#' @export
downturn <- function(
		x, start = NULL, end = NULL,
		digits = 6, k = 3, m = 1){
	if(is.null(x))
		return(NULL)
	if(!is.null(digits))
		x <- round(x, digits = digits)
	res <- rollapply(
		x, width=k+m+1,
		function(x){
			# (x[1]<=x[2]) & (x[2]<=x[3]) &
			#   (x[3]>x[4]) & (x[4]>=x[5])
			diff_ <- diff(x)
			all(c(diff_[seq(1, k)] >= 0, # increase
				  diff_[k+1]< 0, # decrease
				  diff_[-seq(1, k+1)] <= 0))
		})
	res <- window(res, start = start, end = end, extend = TRUE)
	res <- time(res)[which(res)]
	res
}
#' @rdname turning_points
#' @export
unwanted_ripples <- function(
		x, start = NULL, end = NULL,
		digits = 6, k = 3, m = 1){
	if (is.null(x))
		return(NULL)
	if (is_tc_estimates(x)) {
		x <- x$tc
	}
	tp <- turning_points(x, start = start, end = end, digits = digits, k = k, m = m)
	# sum(sapply(tp, function(tp_s){
	# 	sum(diff(tp_s) <= 10 / frequency(x))
	# }))
	tp <- sort(unlist(tp))
	sum(diff(tp) <= 10 / frequency(x))

}
