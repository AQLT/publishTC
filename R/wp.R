#' @noRd
#' @exportS3Method base::as.list
as.list.ts <- function(x, ...) {
	if (is.matrix(x)) {
		res <- lapply(seq_len(ncol(x)), function(i) x[, i])
		names(res) <- colnames(x)
		res
	} else {
		list(x)
	}
}

#' Export and Import Time Series to and from CSV
#'
#' `write.ts()` exports a time series object (`"ts"`) or a list of time series to
#' a CSV file, adding a `"time"` column. `read.ts()` imports a CSV file whose first
#' column contains dates and reconstructs the corresponding `ts` object.
#'
#' @param x A time series object (class `"ts"`) or a list of `"ts"` objects to export.
#' @param file A character string specifying the path to the CSV file to write or read.
#' @param frequency An integer specifying the number of observations per unit of time
#'   (e.g., 12 for monthly, 4 for quarterly). If `NULL` (the default), it is automatically
#'   inferred from the date column.
#' @param list Logical. If `TRUE`, `read.ts()` returns a list of time series objects;
#'   otherwise, it returns a single `"ts"` or `"mts"` object.
#' @param ... Additional arguments passed to [utils::write.csv()] or [utils::read.csv()].
#'
#' @details
#' `write.ts()` formats the time index into a dedicated date column (e.g., `YYYY-MM-DD` or
#' fractional years) alongside the data series. When reading back with `read.ts()`, the time
#' index is parsed to reconstruct regular R time series objects.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' file <- tempfile(fileext = ".csv")
#' write.ts(AirPassengers, file)
#' read.ts(file)
#'
#' @returns
#' * `write.ts()` returns `NULL` invisibly and is called for its side effect (writing a CSV file).
#' * `read.ts()` returns a `"ts"` object, a multiple time series (`"mts"`), or a `list` of `"ts"` objects depending on `list`.
#'
#' @export
write.ts <- function(x, file, ...){
	data <- data.frame(time = time(x), x)
	utils::write.csv(data, file = file, row.names = FALSE, na = "", ...)
}
#' @name write.ts
#' @export
read.ts <- function(file, frequency = NULL, list = FALSE){
	data <- utils::read.csv(file = file)
	if (is.null(frequency))
		frequency <- max(table(trunc(round(data[,1],3))))
	data <- ts(data[, -1],
			   start = data[1,1],
			   frequency = frequency
	)
	if (list)
		data <- as.list(data)
	data
}
