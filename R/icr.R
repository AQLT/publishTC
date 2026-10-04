#' Compute IC-Ratio
#'
#' `icr()` compute the overall I/C ratio, while `icrs()` compute the I/C ratios for each period.
#' @param x,tc seasonally adjusted and trend-cycle components.
#' If `x` is a `"tc_estimates"` object then `tc` is ignored.
#' @param mul boolean indicating if the decomposition is multiplicative or additive.
#'
#' @details
#' The I/C ratio is a measure of the relative importance of the irregular component compared to the trend-cycle component in a time series decomposition.
#' For a time series of frequency \eqn{p}, when the decomposition is additive (`mul = FALSE`, the default),
#'  `icrs()` returns a vector with the ratio of the mean absolute variation of the irregular component
#'  to the mean absolute variation of the trend-cycle component:
#' \deqn{
#' \frac{\bar{I_k}}{\bar{C_k}}=\frac{\sum\lvert I_t-I_{t-k}\rvert}{\sum\lvert TC_t-TC_{t-k}\rvert}\text{ for }k\in\{1,2,\dots, p\}.
#' }
#' If the decomposition is multiplicative (`mul = TRUE`), the ratio is computed as:
#' \deqn{
#' \frac{\bar{I_k}}{\bar{C_k}}=\frac{\sum\lvert I_t/I_{t-k}\rvert}{\sum\lvert TC_t/TC_{t-k}\rvert}\text{ for }k\in\{1,2,\dots, p\}.
#' }
#' `icr()` returns the so called overall I/C ratio, which is the \eqn{\bar{I_1}/\bar{C_1}}.
#'
#' The irregular component is computed as the difference (or ratio, depending of the `mul` parameter?) between the original time series and the trend-cycle component:
#' \deqn{I_t = X_t - TC_t\text{ or }I_t = X_t / TC_t.}
#'
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' icr(tc)
#' icrs(tc)
#'
#' @returns `icr()` returns the overall I/C ratio, while `icrs()` returns a vector of I/C ratios for each period.
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
abs_mean_var <- function(x, nlags = 1, mul = FALSE){
	sapply(seq_len(nlags), function(lag){
		d <- x - stats::lag(x, -lag)
		if (mul) {
			d <- d / stats::lag(x, -lag)
		}
		mean(abs(d), na.rm = TRUE)
	})
}
#' Month of Cyclical Dominance
#'
#' @inheritParams icr
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' icr(tc)
#' icrs(tc)
#' mcd(tc)
#'
#' @details The Month of Cyclical Dominance (MCD) is the months needed for the absolute variations of the trend-cycle component to override those of the irregular component
#' smallest it takes on average for the cyclical signal to outweigh the irregular fluctuations.
#' It is computed as the first month where the I/C ratio is less than or equal to 1.
#' Therefore, the MCD is the number \eqn{k} such that \eqn{\bar{I_j}/\bar{C_j}\leq 1} for any \eqn{j\geq k}.
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
			break;
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
#' Perform X-11 selection of the length of Henderson (`x11_trend_selection()`) and
#' compute the associated I/C ratio used to build Musgrave fuilters (`find_icr()`).
#'
#' @param x a `"ts"` object.
#' @param freq frequency of the time series used to compute the I/C ratio.
#' @param length length of the filter.
#'
#' @details The following procedure is used in X-11 to select the length of the trend filter:
#'
#' 1. Computes the I/C ratio, \eqn{icr} with an Henderson filter of length the frequency plus 1.
#'
#' 2. The length depends on the value or \eqn{icr}:
#'
#'    * if \eqn{icr < 1} then the selected length is 9 for monthly data and 5 otherwise;
#'    * if \eqn{1 \leq icr < 3.5} then the selected length is \eqn{freq + 1} where \eqn{freq} is the frequency of data (12 for monthly data, 4 for quarterly data...).
#'    * if \eqn{icr \geq 3.5} then the selected length is 23 for monthly data and 7 otherwise.
#'
#' 3. The value of \eqn{icr} is then fixed to build Musgrave filters (`find_icr()`) :
#'
#'    * for quarterly data, if the length is 5 then \eqn{icr = 0.001}, otherwide \eqn{icr = 4.5};
#'    * if the length if less or equal to 9 then \eqn{icr = 1};
#'    * else if the length if less or equal to 13 then \eqn{icr = 3.5};
#'    * else \eqn{icr = 4.5}.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x11_trend_selection(cars_registrations)
#' find_icr(13)
#'
#' @returns `x11_trend_selection()` returns a named vector with the selected length and the associated I/C ratio.
#' `find_icr()` returns the I/C ratio associated with the specified length and frequency used in the X-11 algorithm.
#'
#' @export
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
