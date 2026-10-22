#' @noRd
#' @export
print.tc_estimates <- function(x, ...) {
	print(x["tc"])
	invisible(x)
}

#' Trend-Cycle Estimates Class
#'
#' Constructor and S3 methods for objects of class `"tc_estimates"`.
#'
#' @param tc Time series object (`"ts"`) representing the estimated trend-cycle component.
#' @param sa Time series object (`"ts"`) representing the original or seasonally adjusted series.
#' @param parameters A list of parameters used to compute the trend-cycle estimates
#'   (e.g., filter coefficients, variance, or degree of polynomial).
#' @param extra_class Character string specifying an additional class to append to
#'   the returned object.
#' @param x An object of class `"tc_estimates"`.
#' @param object An object of class `"tc_estimates"`.
#' @param ... Additional arguments passed to or from other methods (currently unused).
#'
#' @details
#' Objects of class `"tc_estimates"` store trend-cycle estimates along with the
#' original series and metadata required for summary statistics, confidence intervals,
#' and implicit forecasts.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' summary(tc)
#' print(tc)
#'
#' @returns
#' `tc_estimates()` returns an object of class `c("tc_estimates", extra_class)`,
#' which is a list containing the following components:
#' * `tc`: The estimated trend-cycle time series.
#' * `x`: The original (seasonally adjusted) time series.
#' * `parameters`: A list of parameters used during estimation.
#'
#' The `summary()` method returns a list of class `"summary.tc_estimates"` containing:
#' * `I/C ratio`: The overall Irregular-to-Trend-Cycle ratio (see [icr()]).
#' * `I/C ratios`: The I/C ratio per period (see [icrs()]).
#' * `MCD`: The Month of Cyclical Dominance statistic (see [mcd()]).
#' * `Length`: The length of the symmetric trend-cycle filter.
#'
#' @export
#' @rdname tc_estimates
tc_estimates <- function(tc, sa, parameters = NULL, extra_class = NULL, ...) {
	res <- list(
		tc = tc,
		x = sa,
		parameters = parameters
	)
	class(res) <- c("tc_estimates", extra_class)
	res
}

#' @name tc_estimates
#' @export
is_tc_estimates <- function(x) {
	inherits(x, "tc_estimates")
}

#' @name tc_estimates
#' @export
summary.tc_estimates <- function(object, ...) {
	list(
		"I/C ratio" = icr(object),
		"I/C ratios" = icrs(object),
		MCD = mcd(object),
		Length = 2 * bandwidth(object) + 1
	)
}
