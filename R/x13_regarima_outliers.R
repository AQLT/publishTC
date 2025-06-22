#' Outlier detection with RegARIMA Model
#'
#' Wrapper around [rjd3x13::regarima_outliers()] to detect Additive Outliers (AO) or
#' Level Shifts (LS) in a seasonnally adjusted series.
#'
#' @param y A `ts` object.
#' @param order Orders of the ARIMA model.
#' @param mean Logical, if `TRUE` the model includes a constant term.
#' @param ao,ls Boolean to indicate if additive outliers (AO) or level shifts (LS) should be detected.
#'
#' @return A list with two elements: `ao` and `ls`, which are vectors of time points where the respective outliers were detected. If no outliers were detected, the corresponding element will be `NULL`.
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
		ao_var <- sapply(strsplit(ao_var, "\\."), function(x){
			time(y)[as.numeric(x[2])]
		})
	}
	if (length(ls_var) == 0) {
		ls_var <- NULL
	} else {
		ls_var <- sapply(strsplit(ls_var, "\\."), function(x){
			time(y)[as.numeric(x[2])]
		})
	}
	list(
		ao = ao_var,
		ls = ls_var
	)
}
