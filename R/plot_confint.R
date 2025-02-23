#' Confidence Intervals plot
#'
#' @param object `"tc_estimates"`.
#' The confidence intervals are computed using the [confint()] function.
#' @param col_confint color of the confidence interval.
#' @param legend_tc,legend_sa,legend_confint legend of the trend-cycle and seasonally adjusted components and for the confidence intervals.
#'
#' @inheritParams lollypop
#' @inheritParams plot.tc_estimates
#' @inheritParams confint-tc
#' @export
confint_plot <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_confint = "grey",
		xlab = "",
		ylab = "",
		level = 0.95,
		...) {
	UseMethod("confint_plot")
}

#' @importFrom graphics lines polygon
#' @importFrom stats confint
#' @export
confint_plot.default <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_confint = "grey",
		xlab = "",
		ylab = "",
		level = 0.95,
		...,
		sa = NULL){
	complete_data <- ts.union(object, sa)
	colnames(complete_data)[1] <- "tc"
	colnames(complete_data)[2:3] <- c("Confint_m", "Confint_p")
	if (is.null(xlim))
		xlim <- range(time(complete_data))
	if (is.null(ylim))
		ylim <- range(window(complete_data, start = xlim[1], end = xlim[2]), na.rm = TRUE)
	plot(1, xlim = xlim, ylim = ylim, ylab = ylab, xlab = xlab, ...)
	polygon(
		x = c(time(complete_data), rev(time(complete_data))),
		y = c(complete_data[,2], rev(complete_data[,3])),
		col = col_confint,
		border = NA
	)
	lines(complete_data[, "sa"], col = col_sa)
	lines(complete_data[, "tc"], col = col_tc)
}
#' @export
confint_plot.tc_estimates <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_confint = "grey",
		xlab = "",
		ylab = "",
		level = 0.95,
		asymmetric_var = TRUE,
		...){
	confint_plot.default(sa = object[["x"]],
						 object = confint(object, level = level, asymmetric_var = asymmetric_var),
						 col_tc = col_tc, col_sa = col_sa,
						 col_confint = col_confint,
						 xlim = xlim, ylim = ylim,
						 xlab = xlab, ylab = ylab,
						 ...)
}
#' @name confint_plot
#' @export
ggconfint_plot <- function(object,
						   col_tc = "#E69F00",
						   col_sa = "black",
						   col_confint = "grey",
						   legend_tc = "Trend-cycle",
						   legend_sa = "Seasonally adjusted",
						   legend_confint = "Confidence interval",
						   level = 0.95,
						   ...) {
	UseMethod("ggconfint_plot")
}
#' @export
ggconfint_plot.default <- function(
		object,
		col_tc = "#E69F00",
		col_sa = "black",
		col_confint = "grey",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		legend_confint = "Confidence interval",
		level = 0.95,
		...,
		sa = NULL){
	complete_data <- ts.union(object, sa)
	colnames(complete_data)[1] <- "tc"
	colnames(complete_data)[2:3] <- c("Confint_m", "Confint_p")
	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_ribbon(
			ggplot2::aes(ymin = Confint_m, ymax = Confint_p, color = legend_confint),
			fill = col_confint
		) +
		ggplot2::geom_line(ggplot2::aes(y = sa, color = legend_sa)) +
		ggplot2::geom_line(ggplot2::aes(y = tc, color = legend_tc)) +
		ggplot2::scale_color_manual(values = c(col_confint, col_sa, col_tc)) +
		ggplot2::theme(legend.title = ggplot2::element_blank()) +
		ggplot2::labs(x = NULL, y = NULL)
}
#' @export
ggconfint_plot.tc_estimates <- function(
		object,
		col_tc = "#E69F00",
		col_sa = "black",
		col_confint = "grey",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		legend_confint = sprintf("Confidence interval %s%%", 100 * level),
		level = 0.95,
		asymmetric_var = TRUE,
		...){
	ggconfint_plot.default(
		sa = object[["x"]],
		object = confint(object, level = level, asymmetric_var = asymmetric_var),
		col_sa = col_sa, col_tc = col_tc, col_confint = col_confint,
		legend_tc = legend_tc, legend_sa = legend_sa, legend_confint = legend_confint,
		...)
}

# ##' @export
# dyconfint_plot <- function(object, sa = NULL,
# 						   col_tc = "#E69F00",
# 						   col_sa = "black",
# 						   col_confint = "grey",
# 						   ...) {
# 	UseMethod("dyconfint_plot")
# }
# ##' @export
# dyconfint_plot.default <- function(object, sa = NULL,
# 								   col_tc = "#E69F00",
# 								   col_sa = "black",
# 								   col_confint = "grey",
# 								   ...){
# 	complete_data <- ts.union(object, sa)
# 	colnames(complete_data)[1] <- "tc"
# 	colnames(complete_data)[2:3] <- c("Confint_m", "Confint_p")
# 	data <- data.frame(time = as.numeric(time(complete_data)),
# 					   complete_data)
# 	dygraphs::dygraph(complete_data) |>
# 		dygraphs::dySeries("sa", label = "Seasonally adjusted",
# 				 col = col_sa) |>
# 		dygraphs::dySeries(c("Confint_m", "tc", "Confint_p"), label = "Trend-cycle",
# 				 color = col_tc) |>
# 		dygraphs::dyRangeSelector()
# }
# ##' @export
# dyconfint_plot.tc_estimates <- function(object, sa = NULL,
# 										col_tc = "#E69F00",
# 										col_sa = "black",
# 										col_confint = "grey",
# 										...){
# 	dyconfint_plot.default(
# 		sa = object[["x"]],
# 		object = confint(object),
# 		col_sa = col_sa, col_tc = col_tc, col_confint = col_confint,
# 		...)
# }
#
# ggconfint_plot.default <- function(object, sa = NULL,
# 								   col_tc = "#E69F00",
# 								   col_sa = "black",
# 								   col_confint = "grey",
# 								   ...){
# 	complete_data <- ts.union(object, sa)
# 	colnames(complete_data)[1] <- "tc"
# 	colnames(complete_data)[2:3] <- c("Confint_m", "Confint_p")
# 	data <- data.frame(time = as.numeric(time(complete_data)),
# 					   complete_data)
#
#
# }
