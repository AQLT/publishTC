#' Compute Irregular-to-Trend-Cycle (I/C) Ratios
#'
#' `icr()` computes the overall I/C ratio, while `icrs()` computes the I/C ratios
#' for each lag up to the series frequency.
#'
#' @param x A seasonally adjusted time series or an object of class `"tc_estimates"`.
#' @param tc A time series representing the trend-cycle component. Ignored if `x`
#'   is an object of class `"tc_estimates"`.
#' @param mul Logical indicating whether the decomposition is multiplicative (`TRUE`)
#'   or additive (`FALSE`, the default).
#'
#' @details
#' The I/C ratio measures the relative importance of the irregular component compared
#' to the trend-cycle component in a time series decomposition.
#'
#' For a time series of frequency \eqn{p}, when the decomposition is additive
#' (`mul = FALSE`), the irregular component is defined as \eqn{I_t = X_t - TC_t}.
#' `icrs()` returns a vector containing the ratio of the mean absolute variation
#' of the irregular component to that of the trend-cycle component for each lag \eqn{k}:
#' \deqn{
#' \frac{\bar{I}_k}{\bar{C}_k} = \frac{\sum |I_t - I_{t-k}|}{\sum |TC_t - TC_{t-k}|} \quad \text{for } k \in \{1, 2, \dots, p\}.
#' }
#' If the decomposition is multiplicative (`mul = TRUE`), the irregular component is
#' defined as \eqn{I_t = X_t / TC_t}, and the ratio of relative variations is computed as:
#' \deqn{
#' \frac{\bar{I}_k}{\bar{C}_k} = \frac{\sum |(I_t - I_{t-k}) / I_{t-k}|}{\sum |(TC_t - TC_{t-k}) / TC_{t-k}|} \quad \text{for } k \in \{1, 2, \dots, p\}.
#' }
#' `icr()` returns the overall I/C ratio, which corresponds to the first lag ratio
#' \eqn{\bar{I}_1 / \bar{C}_1}.
#'
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' icr(tc)
#' icrs(tc)
#' @returns `icr()` returns a single numeric value representing the overall I/C ratio,
#'   while `icrs()` returns a named numeric vector of I/C ratios for lags \eqn{1} to \eqn{p}.
#'
#'
#' @export
icr <- function(x, tc, mul = FALSE){
	if (is_tc_estimates(x)) {
		tc <- x$tc
		x <- x$x
	}
	if (mul) {
		si <- x / tc
	} else {
		si <- x - tc
	}
	gc <- abs_mean_var(tc, 1, mul = mul)
	gi <- abs_mean_var(si, 1, mul = mul)
	if (gc == 0) {
		icr <- find_icr(default_length(frequency(x)), frequency(x))
	} else {
		icr <- gi / gc
	}
	return(icr)
}
#' @noRd
#' @keywords internal
default_length <- function(freq) {
	if (freq == 2) {
		length <- 5
	} else {
		length <- freq + 1
	}
	return(length)
}
#' @name icr
#' @export
icrs <- function(x, tc, mul = FALSE){
	if (is_tc_estimates(x)) {
		tc <- x$tc
		x <- x$x
	}
	freq <- frequency(x)
	if (mul) {
		si <- x / tc
	} else {
		si <- x - tc
	}
	gc <- abs_mean_var(tc, freq, mul = mul)
	gi <- abs_mean_var(si, freq, mul = mul)
	icr <- gi / gc
	return(icr)
}
#' @noRd
#' @keywords internal
abs_mean_var <- function(x, nlags = 1, mul = FALSE){
	sapply(seq_len(nlags), function(lag){
		d <- x - stats::lag(x, -lag)
		if (mul) {
			d <- d / stats::lag(x, -lag)
		}
		mean(abs(d), na.rm = TRUE)
	})
}
#' Month of Cyclical Dominance (MCD)
#'
#' Computes the Month of Cyclical Dominance (MCD) for a time series decomposition.
#'
#' @inheritParams icr
#'
#' @details
#' The Month of Cyclical Dominance (MCD) represents the minimum number of months required
#' for the average absolute variation of the trend-cycle component to dominate that
#' of the irregular component. In other words, it indicates the shortest time span needed
#' on average for the cyclical signal to outweigh irregular fluctuations.
#'
#' It is computed as the smallest integer \eqn{k} (where \eqn{k \ge 1}) such that the I/C ratio
#' remains less than or equal to 1 for all lags \eqn{j \ge k}:
#' \deqn{
#' k = \min \left\{ k \in \{1, \dots, p\} : \frac{\bar{I}_j}{\bar{C}_j} \le 1 \text{ for all } j \ge k \right\}
#' }
#' If the I/C ratio never falls below or equal to 1 within the frequency \eqn{p}, the MCD is set to \eqn{p}.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' mcd(tc)
#' icrs(tc)
#'
#' @returns An integer specifying the Month of Cyclical Dominance (MCD).
#'
#' @export
mcd <- function(x, tc, mul = FALSE){
	if (is_tc_estimates(x)) {
		tc <- x$tc
		x <- x$x
	}
	ic <- icrs(x = x, tc = tc, mul = mul)
	inf1 <- ic <= 1
	for (i in seq_along(ic)) {
		has_mcd <- all(inf1[i:length(ic)])
		if (has_mcd) {
			break
		}
	}
	if (!has_mcd)
		return(length(ic))
	i
}

#'@importFrom rjd3filters lp_filter
#'@importFrom rJava .jcall
NULL

#' X-11 Selection of Trend-Cycle Filter
#'
#' Performs X-11 selection for the length of the Henderson filter
#' (`x11_trend_selection()`) and computes the associated I/C ratio used
#' to build Musgrave filters (`find_icr()`).
#'
#' @param x A `"ts"` object representing the time series.
#' @param freq Integer specifying the frequency of the time series used to compute
#'   the I/C ratio (e.g., 12 for monthly, 4 for quarterly).
#' @param length Integer specifying the length of the filter.
#'
#' @details
#' The following procedure is used in X-11 to select the length of the trend filter:
#'
#' 1. Computes the I/C ratio, \eqn{icr}, with a Henderson filter of length equal
#'    to the series frequency plus 1.
#'
#' 2. The selected length depends on the value of \eqn{icr}:
#'    * If \eqn{icr < 1}, the selected length is 9 for monthly data and 5 otherwise.
#'    * If \eqn{1 \le icr < 3.5}, the selected length is \eqn{freq + 1}
#'      (i.e., 13 for monthly data, 5 for quarterly data).
#'    * If \eqn{icr \ge 3.5}, the selected length is 23 for monthly data and 7 otherwise.
#'
#' 3. The value of \eqn{icr} is then mapped to build Musgrave filters (`find_icr()`):
#'    * For quarterly data, if the length is 5, then \eqn{icr = 0.001}; otherwise, \eqn{icr = 4.5}.
#'    * For other frequencies, if the length is less than or equal to 9, then \eqn{icr = 1.0}.
#'    * Else, if the length is less than or equal to 13, then \eqn{icr = 3.5}.
#'    * Else, \eqn{icr = 4.5}.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x11_trend_selection(cars_registrations)
#' find_icr(length = 13, freq = 12)
#'
#' @returns
#' * `x11_trend_selection()` returns a named numeric vector containing the selected length
#'   and the associated I/C ratio.
#' * `find_icr()` returns a single numeric value corresponding to the I/C ratio
#'   associated with the specified filter length and frequency in the X-11 algorithm.
#'
#' @export
#' @rdname x11_trend_selection
x11_trend_selection <- function(x){
	icr <- icr(x, rjd3filters::filter(x, henderson[[as.character(frequency(x)+1)]]))
	freq <- frequency(x)
	if (freq == 4) {
		icr <- icr * 3
	} else if (freq == 2) {
		icr <- icr * 6
	}
	if (freq == 2) {
		length <- 5
	} else if (icr >= 1 && icr < 3.5) {
		length <- freq + 1
	} else if (icr < 1) {
		if (freq == 12) {
			length <- 9
		} else {
			length <- 5
		}
	} else {
		if (freq == 12){
			length <- 23
		} else {
			length <- 7
		}
	}
	icr <-  find_icr(length, freq)
	return (c(icr = icr, length = length))
}
#' @rdname x11_trend_selection
#' @export
find_icr <- function(length, freq = 12){
	.jcall("jdplus/x13/base/core/x11/filter/MusgraveFilterFactory",
		   "D", "findR",
		   as.integer(length), as.integer(freq))
}
utils::globalVariables(c(
	"henderson", "local_param_est",
	"CLF", "CLF_CN",
	"Confint_m", "Confint_p", "tc",
	"y",  "Method"
	))
