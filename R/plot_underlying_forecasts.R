#' Plot Underlying Forecasts for Trend-Cycle Estimates
#'
#' Base R (`underlying_forecasts_plot()`) and \pkg{ggplot2} (`ggunderlying_forecasts_plot()`)
#' graphics for plotting trend-cycle estimates alongside their underlying forecasts.
#'
#' @inheritParams implicit_forecasts_plot
#'
#' @details
#' `underlying_forecasts_plot()` produces a base R graphic, whereas
#' `ggunderlying_forecasts_plot()` creates a \pkg{ggplot2} object that can be further customized.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
#' underlying_forecasts_plot(tc_mod, xlim = c(2022, 2025))
#'
#' @returns
#' * `underlying_forecasts_plot()` returns `NULL` invisibly and is called for its side effect (drawing a plot).
#' * `ggunderlying_forecasts_plot()` returns a \code{\link[ggplot2]{ggplot}} object.
#'
#' @export
#' @rdname underlying_forecasts_plot
underlying_forecasts_plot <- function(
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
	UseMethod("underlying_forecasts_plot")
}

#' @noRd
#' @export
underlying_forecasts_plot.default <- function(
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
	colnames(complete_data)[2] <- "underlying forecast"

	if (is.null(ylim) & !is.null(xlim))
		ylim <- get_ylim(complete_data, xlim)

	plot(complete_data, type = "l", plot.type = "single",
		 xlim = xlim, ylim = ylim,
		 lty = c(1, lty_last_tc, 1, lty_last_tc),
		 col = c(col_sa, col_i_f, col_tc, col_tc),
		 xlab = xlab, ylab = ylab, ...)
}
#' @noRd
#' @export
underlying_forecasts_plot.tc_estimates <- function(
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
	if (is.null(n_last_tc))
		n_last_tc <- mcd(object)
	underlying_forecasts_plot.default(
		sa = object[["x"]],
		i_f = rjd3filters::underlying_forecasts(object),
		object = object[["tc"]],
		col_tc = col_tc, col_sa = col_sa,
		col_i_f = col_i_f,
		xlim = xlim, ylim = ylim,
		xlab = xlab, ylab = ylab,
		lty_last_tc = lty_last_tc,
		n_last_tc = ifelse(is.null(n_last_tc), bandwidth(object), n_last_tc) ,
		...)
}
#' @name underlying_forecasts_plot
#' @export
ggunderlying_forecasts_plot <- function(
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
	UseMethod("ggunderlying_forecasts_plot")
}
#' @noRd
#' @export
ggunderlying_forecasts_plot.default <- function(
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
		ylim <- get_ylim(complete_data, xlim)

	data <- data.frame(time = as.numeric(time(complete_data)),
					   complete_data,
					   check.names = FALSE)

	p <- ggplot2::ggplot(data = data, ggplot2::aes(x = time)) +
		ggplot2::geom_line(ggplot2::aes(y = sa, color = legend_sa), na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = i_f, color = legend_i_f), lty = lty_i_f, na.rm = TRUE) +
		ggplot2::geom_line(ggplot2::aes(y = tc_final, color = legend_tc), na.rm = TRUE)
	if (n_last_tc > 0)
		p <- p +
		ggplot2::geom_line(ggplot2::aes(y = tc_prov), color = col_tc, lty = lty_last_tc, na.rm = TRUE)
	p <- p  +
		ggplot2::scale_color_manual(
			values = c(col_sa, col_i_f, col_tc),
			breaks = c(legend_sa, legend_i_f, legend_tc)
		) +
		ggplot2::theme(legend.title = ggplot2::element_blank()) +
		ggplot2::labs(x = NULL, y = NULL)
	if (!is.null(xlim) | !is.null(ylim))
		p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
	p
}
#' @noRd
#' @export
ggunderlying_forecasts_plot.tc_estimates <- function(
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
	if (is.null(n_last_tc))
		n_last_tc <- mcd(object)
	ggunderlying_forecasts_plot.default(
		sa = object[["x"]],
		i_f = rjd3filters::underlying_forecasts(object),
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

#' Underlying Forecasts Methods
#'
#' S3 method of [rjd3filters::underlying_forecasts()] for `"tc_estimates"` objects.
#'
#' @inheritParams implicit_forecasts.henderson
#'
#' @details
#' See [rjd3filters::underlying_forecasts()] for details on the computation of underlying forecasts.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
#' underlying_forecasts(tc_mod)
#'
#' @returns A `matrix` or `mts` object containing the underlying forecasts for the series.
#'
#' @rdname underlying_forecasts.tc_estimates
#' @exportS3Method rjd3filters::underlying_forecasts
underlying_forecasts.henderson <- function(x, ...) {
	rjd3filters::underlying_forecasts(x = x$x, coefs = x$parameters$tc_coef)
}
#' @rdname underlying_forecasts.tc_estimates
#' @exportS3Method rjd3filters::underlying_forecasts
underlying_forecasts.clf <- function(x, ...) {
	rjd3filters::underlying_forecasts(x = x$x, coefs = x$parameters$tc_coef)
}
#' @rdname underlying_forecasts.tc_estimates
#' @exportS3Method rjd3filters::underlying_forecasts
underlying_forecasts.robust_henderson <- function(x, ...) {
	# We rebuild the coefficients to be coherent with the definition of the underlying forecast
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
	rjd3filters::underlying_forecasts(x = sa, coefs = coefs)
}
#' @importFrom rjd3filters underlying_forecasts
#' @export
rjd3filters::underlying_forecasts
