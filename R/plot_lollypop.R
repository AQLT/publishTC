#' Lollypop plot
#'
#' @param object `"tc_estimates"` object.
#' If `object` is a `"tc_estimates"` object then `sa` is optional.
#'
#' @param color_points,cex_points color and size of the points associated to the seasonnaly adjusted component.
#' @param xlim,ylim x and y limits of the plot.
#' If `NULL` (the default), then the limits determined automatically.
#'
#' @param pch_points point type of the seasonally adjusted component.
#' @inheritParams plot.tc_estimates
#' @param ... other parameters.
#' @export
lollypop <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		color_points = col_sa,
		cex_points = 1,
		pch_points = 16,
		xlab = "",
		ylab = "",
		...) {
	UseMethod("lollypop")
}

#' @importFrom graphics points segments
#' @importFrom stats coef frequency start time ts ts.union window end qt
#' @export
lollypop.default <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		color_points = col_sa,
		cex_points = 1,
		pch_points = 16,
		xlab = "",
		ylab = "",
		lty_last_tc = 2,
		...,
		sa = NULL,
		h = 6){

	tc <- object
	tc_final <- window(tc, end = time(tc)[length(tc) - h])
	tc_prov <- window(tc, start = time(tc)[length(tc) - h])
	complete_data <- ts.union(tc_final, tc_prov, sa)
	if (is.null(xlim))
		xlim <- range(time(complete_data))
	if (is.null(ylim))
		ylim <- range(window(complete_data, start = xlim[1], end = xlim[2]), na.rm = TRUE)
	plot(complete_data[, c("tc_final", "tc_prov")], type = "l",
		 ylim = ylim,
		 xlim = xlim,
		 col = col_tc,
		 ylab = ylab, xlab = xlab,
		 lty = c(1, lty_last_tc),
		 ...)
	points(complete_data[, "sa"], pch = pch_points,
		   bg = color_points, cex = cex_points)
	segments(x0 = time(complete_data),
			 y0 = complete_data[, "tc"],
			 y1 = complete_data[, "sa"],
			 col = col_sa)
}
#' @export
lollypop.tc_estimates <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		color_points = col_sa,
		cex_points = 1,
		pch_points = 16,
		xlab = "",
		ylab = "",
		lty_last_tc = 2,
		...){
	lollypop.default(sa = object[["x"]], object = object[["tc"]], xlim = xlim, ylim = ylim,
					 col_tc = col_tc, col_sa = col_sa,
					 color_points = color_points, cex_points = cex_points, pch_points = pch_points,
					 xlab = xlab, ylab = ylab,
					 h = bandwidth(object),
					 lty_last_tc = lty_last_tc,
					 ...)
}
#' @name lollypop
#' @export
gglollypop <- function(
		object,
		col_tc = "#E69F00",
		col_sa = "black",
		color_points = col_sa,
		cex_points = 1,
		pch_points = 16,
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		lty_last_tc = 2,
		...) {
	UseMethod("gglollypop")
}
#' @export
gglollypop.default <- function(
		object,
		col_tc = "#E69F00",
		col_sa = "black",
		color_points = col_sa,
		cex_points = 1,
		pch_points = 16,
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		lty_last_tc = 2,
		...,
		sa = NULL,
		h = 6){
	tc <- object
	tc_final <- window(tc, end = time(tc)[length(tc) - h])
	tc_prov <- window(tc, start = time(tc)[length(tc) - h])
	complete_data <- ts.union(tc_final, tc_prov, sa)
	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = tc_final, color = legend_tc), na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = tc_prov), color = col_tc, lty = lty_last_tc, na.rm = TRUE) +
		ggplot2::geom_point(ggplot2::aes(y = sa, color = legend_sa), pch = pch_points,
							size = cex_points, na.rm = TRUE) +
		ggplot2::geom_segment(ggplot2::aes(y = tc_final, yend = sa), color = col_sa, na.rm = TRUE) +
		ggplot2::geom_segment(ggplot2::aes(y = tc_prov, yend = sa), color = col_sa, na.rm = TRUE) +
		ggplot2::scale_color_manual(values = c(col_sa, col_tc)) +
		ggplot2::theme(legend.title = ggplot2::element_blank()) +
		ggplot2::labs(x = NULL, y = NULL)
}
#' @export
gglollypop.tc_estimates <- function(
		object,
		col_tc = "#E69F00",
		col_sa = "black",
		color_points = col_sa,
		cex_points = 1,
		pch_points = 16,
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		lty_last_tc = 2,
		...){
	gglollypop.default(sa = object[["x"]], object = object[["tc"]],
					   col_sa = col_sa, col_tc = col_tc,
					   color_points = color_points, cex_points = cex_points, pch_points = pch_points,
					   legend_tc = legend_tc, legend_sa = legend_sa,
					   h = bandwidth(object),
					   lty_last_tc = lty_last_tc,
					   ...)
}
