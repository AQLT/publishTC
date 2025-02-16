#' Confidence Intervals plot
#'
#' @param object Data with trend-cycle component and the upper and lower bounds of the confidence interval.
#' If `object` is a `"tc_estimates"` then is is computed using the `implicit_forecasts()` function.
#' @param col_implicit_forecasts color of the confidence interval.
#'
#' @inheritParams lollypop
#' @inheritParams plot.tc_estimates
#' @export
implicit_forecasts_plot <- function(
		sa, i_f, tc = NULL, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_prev = col_sa,
		col_implicit_forecasts = "grey",
		xlab = "",
		ylab = "",
		...) {
	UseMethod("implicit_forecasts_plot")
}

#' @export
implicit_forecasts_plot.default <- function(
		sa, i_f, tc = NULL, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_prev = col_sa,
		col_implicit_forecasts = "grey",
		xlab = "",
		ylab = "",
		...){
	i_f <- ts(c(sa[length(sa)], i_f),
			  end = end(i_f),
			  frequency = frequency(i_f))
	complete_data <- ts.union(sa, i_f, tc)
	colnames(complete_data)[2] <- c("implicit forecast")
	if (is.null(xlim))
		xlim <- range(time(complete_data))
	if (is.null(ylim))
		ylim <- range(window(complete_data, start = xlim[1], end = ylim[2]), na.rm = TRUE)

	plot(complete_data, type = "l", plot.type = "single",
		 lty = c(1 ,3, 2),
		 col = c(col_sa, col_prev, col_tc),
		 xlab = xlab, ylab = ylab, ...)
}
#' @export
implicit_forecasts_plot.tc_estimates <- function(
		sa, i_f, tc = NULL, xlim = NULL, ylim = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_prev = col_sa,
		xlab = "",
		ylab = "",
		...){

	implicit_forecasts_plot.default(
		sa = sa[["x"]],
		i_f = implicit_forecasts(sa),
		tc = sa[["tc"]],
		col_tc = col_tc, col_sa = col_sa,
		col_prev = col_prev,
		xlim = xlim, ylim = ylim,
		xlab = xlab, ylab = ylab,
		...)
}
#' @name implicit_forecasts_plot
#' @export
ggimplicit_forecasts_plot <- function(
		sa, i_f, tc = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_prev = col_sa,
		...) {
	UseMethod("ggimplicit_forecasts_plot")
}
#' @export
ggimplicit_forecasts_plot.default <- function(
		sa, i_f, tc = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_prev = col_sa,
		...){
	i_f <- ts(c(sa[length(sa)], i_f),
			  end = end(i_f),
			  frequency = frequency(i_f))
	complete_data <- ts.union(sa, tc)
	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data,
					   check.names = FALSE)

	data_prev <- data.frame(time = as.numeric(time(i_f)),
							i_f)
	colnames(data_prev)[2] <- "implicit forecast"
	ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = sa), color = col_sa) +
		ggplot2::geom_line(data = data_prev, ggplot2::aes(y = `implicit forecast`), color = col_prev, lty = 3) +
		ggplot2::geom_line(ggplot2::aes(y = tc), color = col_tc, lty = 2)
}
#' @export
ggimplicit_forecasts_plot.tc_estimates <- function(
		sa, i_f, tc = NULL,
		col_tc = "#E69F00",
		col_sa = "black",
		col_prev = col_sa,
		...){
	ggimplicit_forecasts_plot.default(
		sa = sa[["x"]],
		i_f = implicit_forecasts(sa),
		tc = sa[["tc"]],
		col_tc = col_tc, col_sa = col_sa,
		col_prev = col_prev,
		...)
}

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
	x$parameters$hat_matrix
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
	X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
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
