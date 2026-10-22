#' Outlier Detection with RegARIMA Model
#'
#' Wrapper around [rjd3x13::regarima_outliers()] to detect Additive Outliers (AO) or
#' Level Shifts (LS) in a seasonally adjusted series.
#'
#' @param y A time series object (class `"ts"`).
#' @param order An integer vector of length 3 specifying the non-seasonal ARIMA orders
#'   \eqn{(p, d, q)}.
#' @param mean Logical. If `TRUE`, the model includes a constant term.
#' @param ao,ls Logical flags indicating whether Additive Outliers (AO) and Level Shifts (LS)
#'   should be detected, respectively.
#'
#' @details
#' This function uses an underlying RegARIMA model via \pkg{rjd3x13} to automatically identify
#' structural breaks and temporary shocks in the input series, returning their locations
#' for downstream robust trend-cycle estimation.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x13_regarima_outliers(cars_registrations)
#'
#' @returns A list with two components:
#' * `ao`: A vector of dates where Additive Outliers were detected (or `NULL` if none).
#' * `ls`: A vector of dates where Level Shifts were detected (or `NULL` if none).
#'
#' @export
x13_regarima_outliers <- function(
		y, order = c(0, 1, 1),
		mean = FALSE, ao = TRUE, ls = TRUE
) {
	mod <- rjd3x13::regarima_outliers(
		y,
		order = order,
		seasonal = c(0, 0, 0),
		ao = ao, ls = ls, tc = FALSE, so = FALSE,
		mean = mean
	)
	out <- mod$model$variables
	if (is.null(out))
		return(list(ao = NULL, ls = NULL))
	ao_var <- grep("ao", out, value = TRUE,ignore.case = TRUE)
	ls_var <- grep("ls", out, value = TRUE,ignore.case = TRUE)
	if (length(ao_var) == 0) {
		ao_var <- NULL
	} else {
		ao_var <- sapply(strsplit(ao_var, ".", fixed = TRUE), function(x){
			time(y)[as.numeric(x[2])]
		})
	}
	if (length(ls_var) == 0) {
		ls_var <- NULL
	} else {
		ls_var <- sapply(strsplit(ls_var, ".", fixed = TRUE), function(x){
			time(y)[as.numeric(x[2])]
		})
	}
	list(
		ao = ao_var,
		ls = ls_var
	)
}
