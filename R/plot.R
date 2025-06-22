#' Default `"tc_estimates"` plot
#'
#'
#' @param object,x `"tc_estimates"` object.
#' @param y unused parameter.
#' @param xlim,ylim x and y limits of the plot.
#' If `xlim` is defined and not `ylim`, then `ylim` is determined automatically.
#' @param ... other (unused) parameters.
#'
#' @param col_sa,col_tc color of the seasonally adjusted and trend-cycle components.
#' @param xlab,ylab x and y axis labels.
#' @param lty_last_tc line type of the last values of the trend-cycle component.
#' @param legend_tc,legend_sa legend of the trend-cycle and seasonally adjusted components.
#' @param n_last_tc number of last values of the trend-cycle component to be plotted with a different line type
#' (to emphasize that there is higher variability for the last estimates).
#' If `NULL`, then `n_last_tc` is equal to the MCD statistic.
#'
#' @examples
#' tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
#' plot(tc_mod, xlim = c(2022, 2024.5))
#' @export
plot.tc_estimates <- function(
		x, y = NULL, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		xlab = "", ylab = "",
		lty_last_tc = 2,
		n_last_tc = 4,
		...){
	tc <- x$tc
	sa <- x$x

	h <- bandwidth(x)
	if (is.null(n_last_tc))
		n_last_tc <- mcd(x)
	tc_final <- window(tc, end = time(tc)[length(tc) - n_last_tc])
	tc_prov <- window(tc, start = time(tc)[length(tc) - n_last_tc])

	complete_data <- ts.union(sa, tc_final, tc_prov)
	if (is.null(ylim) & !is.null(xlim))
		ylim <- get_ylim(complete_data, xlim)

	plot(complete_data, type = "l", plot.type = "single",
		 xlim = xlim, ylim = ylim,
		 lty = c(1 ,1, lty_last_tc),
		 col = c(col_sa, col_tc, col_tc),
		 xlab = xlab, ylab = ylab, ...)
}


#' @importFrom ggplot2 autoplot
#' @method autoplot tc_estimates
#' @name plot.tc_estimates
#' @export
autoplot.tc_estimates <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		lty_last_tc = 2,
		n_last_tc = 4, ...){
	x <- object
	tc <- x$tc
	sa <- x$x

	h <- bandwidth(x)
	if (is.null(n_last_tc))
		n_last_tc <- mcd(x)
	tc_final <- window(tc, end = time(tc)[length(tc) - n_last_tc])
	tc_prov <- window(tc, start = time(tc)[length(tc) - n_last_tc])

	complete_data <- ts.union(sa, tc_final, tc_prov)

	if (is.null(ylim) & !is.null(xlim))
		ylim <- get_ylim(complete_data, xlim)

	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data)
	p <- ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = sa, color = legend_sa), na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = tc_final, color = legend_tc), na.rm = TRUE)
	if (n_last_tc > 0)
		p <- p +
		ggplot2::geom_line(ggplot2::aes(y = tc_prov), color = col_tc, lty = lty_last_tc, na.rm = TRUE)
	p <- p  +
		ggplot2::scale_color_manual(values = c(col_sa, col_tc)) +
		ggplot2::theme(legend.title = ggplot2::element_blank()) +
		ggplot2::labs(x = NULL, y = NULL)
	if (!is.null(xlim) | !is.null(ylim))
		p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
	p
}

#' @export
autoplot.ts <- function (object, xlim = NULL, ylim = NULL, ...) {

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
	if (is.null(ylim) & !is.null(xlim))
		ylim <- get_ylim(object, xlim)
	colnames(data) <- c("time", "y", "Method")
	data$Method <- factor(data$Method, levels = name_methods, ordered = TRUE)
	p <- ggplot2::ggplot(data = data) +
		ggplot2::geom_line(mapping = ggplot2::aes(
			x = time, y = y, colour = Method), na.rm = TRUE)
	if (!is.null(xlim) | !is.null(ylim))
		p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
	p
}

get_ylim <- function(data, xlim = NULL, na.rm = TRUE) {
	if ( is.null(xlim)) {
		return(range(data, na.rm = na.rm))
	} else {
		start <- xlim[1]
		end <- xlim[2]
		if (is.na(start))
			start <- NULL
		if (is.na(end))
			end <- NULL
		return(range(window(data, start = start, end = end, extend = TRUE), na.rm = na.rm))
	}
}
