#' @export
plot.tc_estimates <- function(x, y = NULL, xlim = NULL, ylim = NULL, ...){
	tc <- x$tc
	sa <- x$x

	h <- bandwidth(x)
	tc_final <- window(tc, end = time(tc)[length(tc) - h])
	tc_prov <- window(tc, start = time(tc)[length(tc) - h])

	complete_data <- ts.union(sa, tc_final, tc_prov)
	plot(complete_data, type = "l", plot.type = "single",
		 lty = c(1 ,1, 2), ...)
}


#' @importFrom ggplot2 autoplot
#' @method autoplot tc_estimates
#' @export
autoplot.tc_estimates <- function(object, ...){
	x <- object
	tc <- x$tc
	sa <- x$x

	h <- bandwidth(x)
	tc_final <- window(tc, end = time(tc)[length(tc) - h])
	tc_prov <- window(tc, start = time(tc)[length(tc) - h])

	complete_data <- ts.union(sa, tc_final, tc_prov)
	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = sa)) +
		ggplot2::geom_line(ggplot2::aes(y = tc_final)) +
		ggplot2::geom_line(ggplot2::aes(y = tc_prov), lty = 2)
}
