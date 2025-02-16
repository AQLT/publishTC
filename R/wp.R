#' @export
as.list.ts <- function(x, ...) {
	if (is.matrix(x)) {
		res <- lapply(1:ncol(x), function(i) x[, i])
		names(res) <- colnames(x)
		res
	} else {
		list(x)
	}
}
#' Export and Import time series object to/from CSV
#'
#' @param x a time series object
#' @param file a character string giving the name of the file to write to.
#' @param frequency an integer giving the number of observations per unit of time.
#' By default it is guessed from the data.
#' @param list boolean, if `TRUE`, the function returns a list of time series objects.
#' @export
write.ts <- function(x, file){
	data <- data.frame(time = time(x), x)
	utils::write.csv(data, file = file, row.names = FALSE, na = "")
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
