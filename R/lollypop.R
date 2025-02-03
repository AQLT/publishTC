#' Lollypop plot
#'
#' @param tc,sa Seasonally adjusted and trend-cycle components.
#' If `tc` is a `"tc_estimates"` object then `sa` is optional
#'
#' @param color_points,cex_points color and size of the points associated to the seasonnaly adjusted component.
#' @param xlim,ylim x and y limits of the plot.
#' If `NULL` (the default), then the limits determined automatically.
#' @export
lollypop <- function(tc, sa, xlim = NULL, ylim = NULL,
					 color_points = "black",
					 cex_points = 1,
					 ...) {
	UseMethod("lollypop")
}

#' @importFrom graphics points segments
#' @importFrom stats coef frequency start time ts ts.union window end qt
#' @export
lollypop.default <- function(tc, sa, xlim = NULL, ylim = NULL,
					 color_points = "black",
					 cex_points = 1,
					 ...){
	complete_data <- ts.union(tc, sa)
	colnames(complete_data) <- c("tc", "sa")
	if (is.null(xlim))
		xlim <- range(time(complete_data))
	if (is.null(ylim))
		ylim <- range(complete_data, na.rm = TRUE)
	plot(complete_data[, "tc"], type = "l",
		 ylim = ylim,
		 xlim = xlim)
	points(complete_data[, "sa"], pch = 16,
		   bg = color_points, cex = cex_points)
	segments(x0 = time(complete_data),
			 y0 = complete_data[, "tc"],
			 y1 = complete_data[, "sa"])
}
#' @export
lollypop.tc_estimates <- function(tc, sa, xlim = NULL, ylim = NULL,
							 color_points = "black",
							 cex_points = 1,
							 ...){
	lollypop.default(sa = sa[["x"]], tc = sa[["tc"]], xlim = xlim, ylim = ylim,
					 color_points = color_points, cex_points = cex_points, ...)
}
#' @name lollypop
#' @export
gglollypop <- function(tc, sa,
					 color_points = "black",
					 cex_points = 1,
					 ...) {
	UseMethod("gglollypop")
}
#' @export
gglollypop.default <- function(tc, sa,
					   color_points = "black",
					   cex_points = 1,
					   ...){
	complete_data <- ts.union(tc, sa)
	colnames(complete_data) <- c("tc", "sa")
	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = tc)) +
		ggplot2::geom_point(ggplot2::aes(y = sa), pch = 16,
							color = color_points, size = cex_points) +
		ggplot2::geom_segment(ggplot2::aes(y = tc, yend = sa)) +
		ggplot2::labs(x = NULL, y = NULL)
}
#' @export
gglollypop.tc_estimates <- function(tc, sa,
								  color_points = "black",
								  cex_points = 1,
								  ...){
	gglollypop.default(sa = sa[["x"]], tc = sa[["tc"]],
					 color_points = color_points, cex_points = cex_points, ...)
}
