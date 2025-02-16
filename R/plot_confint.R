#' Confidence Intervals plot
#'
#' @param object Data with trend-cycle component and the upper and lower bounds of the confidence interval.
#' If `object` is a `"tc_estimates"` then is is computed using the `confint()` function.
#' @param col_confint color of the confidence interval.
#'
#' @inheritParams lollypop
#' @inheritParams plot.tc_estimates
#' @export
confint_plot <- function(object, sa = NULL, xlim = NULL, ylim = NULL,
						 col_tc = "#E69F00",
						 col_sa = "black",
						 col_confint = "grey",
						 xlab = "",
						 ylab = "",
						 ...) {
	UseMethod("confint_plot")
}

#' @importFrom graphics lines polygon
#' @importFrom stats confint
#' @export
confint_plot.default <- function(
		object, sa = NULL, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_confint = "grey",
		xlab = "",
		ylab = "",
		...){
	complete_data <- ts.union(object, sa)
	colnames(complete_data)[1] <- "tc"
	colnames(complete_data)[2:3] <- c("Confint_m", "Confint_p")
	if (is.null(xlim))
		xlim <- range(time(complete_data))
	if (is.null(ylim))
		ylim <- range(complete_data, na.rm = TRUE)
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
confint_plot.tc_estimates <- function(object, sa = NULL, xlim = NULL, ylim = NULL,
									  col_tc = "#E69F00",
									  col_sa = "black",
									  col_confint = "grey",
									  xlab = "",
									  ylab = "",
									  ...){
	confint_plot.default(sa = object[["x"]],
						 object = confint(object),
						 col_tc = col_tc, col_sa = col_sa,
						 col_confint = col_confint,
						 xlim = xlim, ylim = ylim,
						 xlab = xlab, ylab = ylab,
						 ...)
}
#' @name confint_plot
#' @export
ggconfint_plot <- function(object, sa = NULL,
						   col_tc = "#E69F00",
						   col_sa = "black",
						   col_confint = "grey",
						   ...) {
	UseMethod("ggconfint_plot")
}
#' @export
ggconfint_plot.default <- function(object, sa = NULL,
								   col_tc = "#E69F00",
								   col_sa = "black",
								   col_confint = "grey",
								   ...){
	complete_data <- ts.union(object, sa)
	colnames(complete_data)[1] <- "tc"
	colnames(complete_data)[2:3] <- c("Confint_m", "Confint_p")
	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_ribbon(
			ggplot2::aes(ymin = Confint_m, ymax = Confint_p),
			fill = col_confint
		) +
		ggplot2::geom_line(ggplot2::aes(y = sa), color = col_sa) +
		ggplot2::geom_line(ggplot2::aes(y = tc), color = col_tc) +
		ggplot2::labs(x = NULL, y = NULL)
}
#' @export
ggconfint_plot.tc_estimates <- function(object, sa = NULL,
										col_tc = "#E69F00",
										col_sa = "black",
										col_confint = "grey",
										...){
	ggconfint_plot.default(
		sa = object[["x"]],
		object = confint(object),
		col_sa = col_sa, col_tc = col_tc, col_confint = col_confint,
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
