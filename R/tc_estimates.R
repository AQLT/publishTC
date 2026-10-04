#' @export
print.tc_estimates <- function(x, ...) {
	print(x["tc"])
}

#' Trend-Cycle Estimates Class
#'
#' @param tc the trend-cycle estimates.
#' @param sa the original time series (usually seasonnally adjusted).
#' @param parameters a list of parameters used to compute the trend-cycle estimates (for example the filter coefficients).
#' @param extra_class additional class to add to the output object.
#' @param x,object a `"tc_estimates"` object.
#' @param ... other unused parameters.
#'
#' @return `tc_estimates()` returns an object of class `c("tc_estimates", extra_class)` which is a list with the following components:
#' - `tc`: the trend-cycle estimates.
#' - `x`: the original time series.
#' - `parameters`: a list of parameters used to compute the trend-cycle estimates.
#'
#' The `summary()` method for `tc_estimates` objects returns a list with the following components:
#' - `I/C ratio`: the I/C ratio of the trend-cycle estimates (see [icr()]).
#' - `I/C ratios`: the I/C ratio per period of the trend-cycle estimates (see [icrs()]).
#' - `MCD`: the Month of Cyclical Dominance (see [mcd()]).
#' - `Length`: the length of the trend-cycle filter used for the final estimates
#' (i.e. the number of observations used to estimate the trend-cycle at the center of the series).
#'
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
