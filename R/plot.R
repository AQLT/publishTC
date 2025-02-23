#' Default `"tc_estimates"` plot
#'
#'
#' @param object,x `"tc_estimates"` object.
#' @param y unused parameter.
#' @param ... other (unused) parameters.
#'
#' @param col_sa,col_tc color of the seasonally adjusted and trend-cycle components.
#' @param xlab,ylab x and y axis labels.
#' @param lty_last_tc line type of the last values of the trend-cycle component.
#' @param legend_tc,legend_sa legend of the trend-cycle and seasonally adjusted components.
#' @export
plot.tc_estimates <- function(
		x, y = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		xlab = "", ylab = "",
		lty_last_tc = 2,
		...){
	tc <- x$tc
	sa <- x$x

	h <- bandwidth(x)
	tc_final <- window(tc, end = time(tc)[length(tc) - h])
	tc_prov <- window(tc, start = time(tc)[length(tc) - h])

	complete_data <- ts.union(sa, tc_final, tc_prov)
	plot(complete_data, type = "l", plot.type = "single",
		 lty = c(1 ,1, lty_last_tc),
		 col = c(col_sa, col_tc, col_tc),
		 xlab = xlab, ylab = ylab, ...)
}


#' @importFrom ggplot2 autoplot
#' @method autoplot tc_estimates
#' @name plot.tc_estimates
#' @export
autoplot.tc_estimates <- function(
		object,
		col_tc = "#E69F00",
		col_sa = "black",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		lty_last_tc = 2, ...){
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
		ggplot2::geom_line(ggplot2::aes(y = sa, color = legend_sa)) +
		ggplot2::geom_line(ggplot2::aes(y = tc_final, color = legend_tc), na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = tc_prov), color = col_tc, lty = lty_last_tc, na.rm = TRUE) +
		ggplot2::scale_color_manual(values = c(col_sa, col_tc)) +
		ggplot2::theme(legend.title = ggplot2::element_blank())
}

#' @export
autoplot.ts <- function (object, ...) {

	data <- data.frame(time = as.numeric(time(object)),
					   object,
					   check.names = FALSE)
	name_methods <- colnames(data)[-1]
	data <- do.call(
		rbind,
		lapply(2:ncol(data), function(i) {
			data.frame(data[,1], as.numeric(data[,i]), colnames(data)[i])
		})
	)
	colnames(data) <- c("time", "value", "Method")
	data$Method <- factor(data$Method, levels = name_methods, ordered = TRUE)
	ggplot2::ggplot(data = data) +
		ggplot2::geom_line(mapping = ggplot2::aes(
			x = time, y = value, colour = Method), na.rm = TRUE)
}
# autoplot.tc_estimates <- function(object,
# 								  col_tc = "#E69F00",
# 								  col_sa = "black", ...){
# 	x <- object
# 	tc <- x$tc
# 	sa <- x$x
#
# 	h <- bandwidth(x)
# 	tc_final <- window(tc, end = time(tc)[length(tc) - h])
# 	tc_prov <- window(tc, start = time(tc)[length(tc) - h])
#
# 	complete_data <- ts.union(sa, tc_final, tc_prov)
#
# 	dygraph(complete_data) %>%
# 		dySeries("sa", label = "Seasonally adjusted") %>%
# 		dySeries("tc_final", label = "Trend-cycle") %>%  # Série en trait plein
# 		dySeries("tc_prov",
# 				 color = col_tc,
# 				 strokeWidth = 1.5,
# 				 strokePattern = "dotted",
# 				 showInLegend = FALSE)
# 	data <- data.frame(time = as.numeric(time(complete_data)),
# 					   complete_data)
# 	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
# 		ggplot2::geom_line(ggplot2::aes(y = sa), color = col_sa) +
# 		ggplot2::geom_line(ggplot2::aes(y = tc_final), color = col_tc) +
# 		ggplot2::geom_line(ggplot2::aes(y = tc_prov), color = col_tc, lty = 2)
# }
