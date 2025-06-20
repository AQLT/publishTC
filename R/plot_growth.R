#' Growth plots
#'
#' Plots the growth rate of the trend-cycle (solid lines) and the seasonally adjusted series (bar-line).
#'
#' @param pct logical. If `TRUE` (the default), the growth rate is expressed in percentage points.
#' @param col_sa_fill fill color of the bar of the seasonally adjusted series.
#'
#' @param sa_bar_line logical. If `TRUE` (the default), the growth rates of the seasonally adjusted series are
#' presented as bar-lines, otherwise they are presented as lines.
#' @param lag lag used for the growth rate.
#' By default, `lag = -1` (i.e. period-to-period growth rate).
#'
#' @inheritParams plot.tc_estimates
#' @inheritParams lollypop
#'
#' @examples
#' tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
#' growthplot(tc_mod, xlim = c(2022, 2024.5))
#' @export
growthplot <- function(
		object,
		pct = TRUE,
		xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		xlab = "",
		ylab = "",
		sa_bar_line = TRUE,
		...,
		lag = -1) {
	UseMethod("growthplot")
}

#' @importFrom graphics points segments
#' @importFrom stats coef frequency start time ts ts.union window end qt
#' @export
growthplot.default <- function(
		object,
		pct = TRUE,
		xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		xlab = "",
		ylab = "",
		sa_bar_line = TRUE,
		...,
		lag = -1,
		sa = NULL){

	tc <- object
	complete_data <- ts.union(tc, sa)
	complete_data <- growth_rate(complete_data, lag = lag, pct = pct)

	if (is.null(ylim) & !is.null(xlim))
		ylim <- get_ylim(complete_data, xlim)

	type <- ifelse(sa_bar_line, "h", "l")

	plot(complete_data[, "sa"], type = type,
		 ylim = ylim,
		 xlim = xlim,
		 col = col_sa,
		 ylab = ylab, xlab = xlab,
		 ...)
	lines(complete_data[, "tc"],
		  col = col_tc)
}
#' @export
growthplot.tc_estimates <- function(
		object,
		pct = TRUE,
		xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		xlab = "",
		ylab = "",
		sa_bar_line = TRUE,
		...,
		lag = -1,
		sa = NULL){
	growthplot.default(
		sa = object[["x"]], object = object[["tc"]],
		pct = pct,
		xlim = xlim, ylim = ylim,
		sa_bar_line = sa_bar_line,
		col_tc = col_tc, col_sa = col_sa,
		xlab = xlab, ylab = ylab,
		lag = lag,
		...)
}
#' @name growthplot
#' @export
gggrowthplot <- function(
		object,
		pct = TRUE,
		xlim = NULL, ylim = NULL,
		sa_bar_line = TRUE,
		col_tc = "#E69F00",
		col_sa = "black",
		col_sa_fill = "grey",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		...,
		lag = -1) {
	UseMethod("gggrowthplot")
}
#' @export
gggrowthplot.default <- function(
		object,
		pct = TRUE,
		xlim = NULL, ylim = NULL,
		sa_bar_line = TRUE,
		col_tc = "#E69F00",
		col_sa = "black",
		col_sa_fill = "grey",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		...,
		lag = -1,
		sa = NULL){
	tc <- object

	complete_data <- ts.union(tc, sa)
	complete_data <- growth_rate(complete_data, lag = lag, pct = pct)

	if (is.null(ylim) & !is.null(xlim))
		ylim <- get_ylim(complete_data, xlim)

	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	p <- ggplot2::ggplot(data = data, ggplot2::aes(x = time))

	if (sa_bar_line) {
		p <- p +
			ggplot2::geom_col(ggplot2::aes(y = sa, color = legend_sa), fill = col_sa_fill, na.rm = TRUE)
	} else {
		p <- p +
			ggplot2::geom_line(ggplot2::aes(y = sa, color = legend_sa), na.rm = TRUE)
	}
	p <- p +
		ggplot2::geom_line(ggplot2::aes(y = tc, color = legend_tc), na.rm = TRUE) +
		ggplot2::scale_color_manual(values = c(col_sa, col_tc)) +
		ggplot2::theme(legend.title = ggplot2::element_blank()) +
		ggplot2::labs(x = NULL, y = NULL)
	if (!is.null(xlim) | !is.null(ylim))
		p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
	p
}
#' @export
gggrowthplot.tc_estimates <- function(
		object,
		pct = TRUE,
		xlim = NULL, ylim = NULL,
		sa_bar_line = TRUE,
		col_tc = "#E69F00",
		col_sa = "black",
		col_sa_fill = "grey",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		...,
		lag = -1){
	gggrowthplot.default(
		sa = object[["x"]], object = object[["tc"]],
		xlim = xlim, ylim = ylim,
		sa_bar_line = sa_bar_line,
		pct = pct,
		col_sa = col_sa, col_tc = col_tc,
		legend_tc = legend_tc, legend_sa = legend_sa,
		lag = lag,
		...)
}

growth_rate <- function(x, lag =-1, pct = TRUE) {
	res <- (x - stats::lag(x, lag)) / stats::lag(x, lag)
	if (is.matrix(x)) {
		colnames(res) <- colnames(x)
	}
	if (pct) {
		res <- 100 * res
	}
	res
}
