#' Multi-Method Trend-Cycle Estimation
#'
#' Computes trend-cycle estimates using multiple smoothing methods simultaneously
#' and returns a list of `"tc_estimates"` objects.
#'
#' @param methods A list of character strings specifying the smoothing methods to apply.
#'   Defaults to `c("henderson", "henderson_localic", "clf")`.
#' @param ... Additional parameters passed to the underlying smoothing functions.
#'
#' @inheritParams henderson_smoothing
#' @inheritParams henderson_robust_smoothing
#'
#' @details
#' This convenience function applies multiple trend-cycle estimation algorithms to
#' the same input series, facilitating direct comparison between different filtering
#' approaches (e.g., standard Henderson, Henderson with local I/C ratio, or CLF smoothing).
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- french_ipi[, "manufacturing"]
#' outliers <- x13_regarima_outliers(x)
#' all_methods <- smoothing(x, ao = outliers$ao, ao_tc = outliers$ao_tc, ls = outliers$ls)
#'
#' plot(all_methods$henderson, xlim = c(2023, 2025))
#' lines(all_methods$henderson_localic, col_tc = "blue")
#'
#' @returns A named list of objects of class `"tc_estimates"`, where each element
#'   corresponds to one of the specified methods in `methods`.
#'
#' @export
smoothing <- function(
		x,
		methods = c("henderson", "henderson_localic", "henderson_robust",
			"henderson_robust_localic", "clf_cn", "clf_alf"),
		endpoints = "Musgrave",
		length = NULL,
		icr = NULL,
		asymmetric_var = FALSE,
		degree = 3,
		ao = NULL,
		ao_tc = NULL,
		ls = NULL,
		...) {
	methods <- tolower(methods)
	res <- list()
	if ("henderson" %in% methods) {
		res$henderson <- henderson_smoothing(
			x = x, endpoints = endpoints, length = length, icr = icr,
			degree = degree,
			local_icr = FALSE)
	}
	if ("henderson_localic" %in% methods) {
		res$henderson_localic <- henderson_smoothing(
			x = x, endpoints = endpoints, length = length, icr = icr,
			asymmetric_var = asymmetric_var,
			degree = degree,
			local_icr = TRUE)
	}
	if ("henderson_robust" %in% methods) {
		res$henderson_robust <- henderson_robust_smoothing(
			x = x, endpoints = endpoints, length = length, icr = icr,
			asymmetric_var = asymmetric_var,
			degree = degree,
			ao = ao, ao_tc = ao_tc, ls = ls,
			local_icr = FALSE)
	}
	if ("henderson_robust_localic" %in% methods) {
		res$henderson_robust_localic <- henderson_robust_smoothing(
			x = x, endpoints = endpoints, length = length, icr = icr,
			asymmetric_var = asymmetric_var,
			degree = degree,
			ao = ao, ao_tc = ao_tc, ls = ls,
			local_icr = TRUE)
	}
	if ("clf_cn" %in% methods) {
		res$clf_cn <- clf_smoothing(x = x, endpoints = "cut-and-normalize")
	}
	if ("clf_alf" %in% methods) {
		res$clf_alf <- clf_smoothing(x = x, endpoints = "ALF")
	}
	if (!is.null(names(methods)))
		names(res) <- names(methods)
	res
}
#' Plot Multiple Trend-Cycle Visualizations
#'
#' Generates a list of \pkg{ggplot2} graphics displaying various diagnostic and
#' comparative plots for a `"tc_estimates"` object.
#'
#' @param plots A character vector specifying the types of plots to generate.
#'   Available options are `"normal"`, `"confint"`, `"lollypop"`, `"implicit_forecasts"`,
#'   and `"underlying_forecasts"`. Defaults to generating all available plots.
#' @param ... Additional graphical arguments passed to internal plotting functions.
#'
#' @inheritParams confint_plot
#' @inheritParams confint_tc
#'
#' @details
#' The following plot types can be produced:
#' * `"normal"`: Standard plot showing trend-cycle and seasonally adjusted series via [autoplot.tc_estimates()].
#' * `"confint"`: Confidence interval plot via [ggconfint_plot()].
#' * `"lollypop"`: Lollipop chart comparing estimates via [gglollypop()].
#' * `"implicit_forecasts"`: Implicit forecasts plot via [ggimplicit_forecasts_plot()].
#' * `"underlying_forecasts"`: Underlying forecasts plot via [ggunderlying_forecasts_plot()].
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' ggsmoothing_plot(tc)
#'
#' @returns A list of \code{\link[ggplot2]{ggplot}} objects corresponding to the requested `plots`.
#'
#' @export
ggsmoothing_plot <- function(
		object,
		plots = c("normal", "confint", "lollypop", "implicit_forecasts", "underlying_forecasts"),
		level = 0.95,
		...) {
	plots <- tolower(plots)
	res <- list()
	if ("normal" %in% plots) {
		res$normal <- ggplot2::autoplot(
			object = object,
			...
			) +
			ggplot2::ggtitle("Normal plot")
		if (!is.null(names(plots)) && names(plots)[plots %in% "normal"]) {
			res$normal <- res$normal +
				ggplot2::ggtitle(names(plots)[plots %in% "normal"])
		}
	}

	if ("confint" %in% plots) {
		res$confint <- ggconfint_plot(
			object = object, level = level, ...
			) +
			ggplot2::ggtitle("Confidence intervals")
		if (!is.null(names(plots)) && names(plots)[plots %in% "confint"]) {
			res$confint <- res$confint +
				ggplot2::ggtitle(names(plots)[plots %in% "confint"])
		}
	}

	if ("lollypop" %in% plots) {
		res$lollypop <- gglollypop(
			object = object,
			...
			) +
			ggplot2::ggtitle("Lollypop")
		if (!is.null(names(plots)) && names(plots)[plots %in% "lollypop"]) {
			res$lollypop <- res$lollypop +
				ggplot2::ggtitle(names(plots)[plots %in% "lollypop"])
		}
	}

	if ("implicit_forecasts" %in% plots) {
		res$implicit_forecasts <- ggimplicit_forecasts_plot(
			object = object, ...
			) +
			ggplot2::ggtitle("Implicit forecasts")
		if (!is.null(names(plots)) && names(plots)[plots %in% "implicit_forecasts"]) {
			res$implicit_forecasts <- res$implicit_forecasts +
				ggplot2::ggtitle(names(plots)[plots %in% "implicit_forecasts"])
		}
	}

	if ("underlying_forecasts" %in% plots) {
		res$underlying_forecasts <- ggunderlying_forecasts_plot(
			object = object, ...
		) +
			ggplot2::ggtitle("Underlying forecasts")
		if (!is.null(names(plots)) && names(plots)[plots %in% "underlying_forecasts"]) {
			res$underlying_forecasts <- res$underlying_forecasts +
				ggplot2::ggtitle(names(plots)[plots %in% "underlying_forecasts"])
		}
	}
	res
}
