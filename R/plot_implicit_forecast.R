#' Implicit Forecasts plot
#'
#' @param col_i_f color of the implicit forecasts.
#'
#' @param legend_tc,legend_sa,legend_i_f legend of the trend-cycle and seasonally adjusted components and for implicit forecasts.
#' @param lty_last_tc,lty_i_f line type of the last values of the trend-cycle component and for the implicit forecasts.
#' @inheritParams lollypop
#' @inheritParams plot.tc_estimates
#'
#' @examples
#' tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
#' implicit_forecasts_plot(tc_mod, xlim = c(2022, 2025))
#' @export
implicit_forecasts_plot <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_i_f = col_sa,
		xlab = "",
		ylab = "",
		lty_last_tc = 2,
		lty_i_f = 3,
		n_last_tc = 4,
		...) {
	UseMethod("implicit_forecasts_plot")
}

#' @export
implicit_forecasts_plot.default <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_i_f = col_sa,
		xlab = "",
		ylab = "",
		lty_last_tc = 2,
		lty_i_f = 3,
		n_last_tc = 4,
		...,
		sa = NULL, i_f = NULL
){
	tc <- object
	i_f <- ts(c(sa[length(sa)], i_f),
			  end = end(i_f),
			  frequency = frequency(i_f))

	tc_final <- window(tc, end = time(tc)[length(tc) - n_last_tc])
	tc_prov <- window(tc, start = time(tc)[length(tc) - n_last_tc])
	complete_data <- ts.union(sa, i_f, tc, tc_final, tc_prov)
	colnames(complete_data)[2] <- c("implicit forecast")

	if (is.null(ylim) & !is.null(xlim))
		ylim <- range(window(complete_data, start = xlim[1], end = xlim[2], extend = TRUE), na.rm = TRUE)

	plot(complete_data, type = "l", plot.type = "single",
		 xlim = xlim, ylim = ylim,
		 lty = c(1, lty_last_tc, 1, lty_last_tc),
		 col = c(col_sa, col_i_f, col_tc, col_tc),
		 xlab = xlab, ylab = ylab, ...)
}
#' @export
implicit_forecasts_plot.tc_estimates <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_i_f = col_sa,
		xlab = "",
		ylab = "",
		lty_last_tc = 2,
		lty_i_f = 3,
		n_last_tc = 4,
		...){

	implicit_forecasts_plot.default(
		sa = object[["x"]],
		i_f = implicit_forecasts(object),
		object = object[["tc"]],
		col_tc = col_tc, col_sa = col_sa,
		col_i_f = col_i_f,
		xlim = xlim, ylim = ylim,
		xlab = xlab, ylab = ylab,
		lty_last_tc = lty_last_tc,
		n_last_tc = ifelse(is.null(n_last_tc), bandwidth(object), n_last_tc) ,
		...)
}
#' @name implicit_forecasts_plot
#' @export
ggimplicit_forecasts_plot <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_i_f = col_sa,
		lty_last_tc = 2,
		lty_i_f = 3,
		n_last_tc = 4,
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		legend_i_f = "Implicit forecasts",
		...) {
	UseMethod("ggimplicit_forecasts_plot")
}
#' @export
ggimplicit_forecasts_plot.default <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_i_f = col_sa,
		lty_last_tc = 2,
		lty_i_f = 3,
		n_last_tc = 4,
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		legend_i_f = "Implicit forecasts",
		...,
		sa = NULL, i_f = NULL){
	tc <- object
	i_f <- ts(c(sa[length(sa)], i_f),
			  end = end(i_f),
			  frequency = frequency(i_f))

	tc_final <- window(tc, end = time(tc)[length(tc) - n_last_tc])
	tc_prov <- window(tc, start = time(tc)[length(tc) - n_last_tc])
	complete_data <- ts.union(sa, i_f, tc, tc_final, tc_prov)

	if (is.null(ylim) & !is.null(xlim))
		ylim <- range(window(complete_data, start = xlim[1], end = xlim[2], extend = TRUE), na.rm = TRUE)

	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data,
					   check.names = FALSE)

	p <- ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = sa, color = legend_sa), na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = i_f, color = legend_i_f), lty = lty_i_f, na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = tc_final, color = legend_tc), na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = tc_prov), color = col_tc, lty = lty_last_tc, na.rm = TRUE) +
		ggplot2::scale_color_manual(values = c(col_sa, col_i_f, col_tc)) +
		ggplot2::theme(legend.title = ggplot2::element_blank()) +
		ggplot2::labs(x = NULL, y = NULL)
	if (!is.null(xlim) | !is.null(ylim))
		p <- ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
	p
}
#' @export
ggimplicit_forecasts_plot.tc_estimates <- function(
		object, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_i_f = col_sa,
		lty_last_tc = 2,
		lty_i_f = 3,
		n_last_tc = 4,
		legend_tc = "Trend-cycle",
		legend_sa = "Seasonally adjusted",
		legend_i_f = "Implicit forecasts",
		...){
	ggimplicit_forecasts_plot.default(
		sa = object[["x"]],
		i_f = implicit_forecasts(object),
		object = object[["tc"]],
		xlim = xlim, ylim = ylim,
		col_tc = col_tc, col_sa = col_sa,
		col_i_f = col_i_f,
		lty_last_tc = lty_last_tc, lty_i_f = lty_i_f,
		legend_tc = legend_tc, legend_sa = legend_sa,
		legend_i_f = legend_i_f,
		n_last_tc = ifelse(is.null(n_last_tc), bandwidth(object), n_last_tc),
		...)
}

#' Compute Implicit Forecasts
#'
#' @param x a `"tc_estimates"` object otherwise uses the [rjd3filters::implicit_forecast()] function.
#' @param ... other unused parameters.
#' @export
implicit_forecasts <- function(x, ...) {
	UseMethod("implicit_forecasts", x)
}
#' @export
implicit_forecasts.default <- function(x, ...) {
	rjd3filters::implicit_forecast(x, ...)
}
#' @export
implicit_forecasts.henderson <- function(x, ...) {
	rjd3filters::implicit_forecast(x = x$x, coefs = x$parameters$tc_coef)
}
#' @export
implicit_forecasts.clf <- function(x, ...) {
	rjd3filters::implicit_forecast(x = x$x, coefs = x$parameters$tc_coef)
}
#' @export
implicit_forecasts.robust_henderson <- function(x, ...) {
	# We rebuild the coefficients to be coherent with the definition of the implicit forecast
	sa <- x$x
	parm <- x$parameters
	n <- length(sa)
	H <- parm$hat_matrix
	U <- parm$U
	Z <- parm$Z
	f_reg <- build_reg(x = sa,
					   ao = parm$out$ao,
					   ao_tc = parm$out$ao_tc,
					   ls = parm$out$ls)
	fun_out <- f_reg$fun_out
	reg <- f_reg$reg
	kernel <- parm$kernel

	degree <- ncol(U) + ncol(Z)
	length <- nrow(U)
	h <- (length - 1) / 2
	bias_r <-  2/(sqrt(pi) * (parm$icr$icr_r))
	if (length(bias_r) == 1) {
		bias_r <- rep(bias_r, h)
	}
	dates_x <- as.numeric(time(sa))
	focus_reg <- window(reg,
						start = dates_x[n - h] ,
						end = dates_x[n],
						extend = TRUE)
	X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[n])
	sym <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
	rfilters <- lapply(0:(h-1), function(q) {
		mmsre_filter(
			ref_filter = sym,
			q = q,
			delta = rev(bias_r)[q + 1],
			U = U,
			Z = Z
		)
	})
	coefs <- finite_filters(sym, rfilters, first_to_last = TRUE)
	rjd3filters::implicit_forecast(x = sa, coefs = coefs)
}
