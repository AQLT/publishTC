
#' Smoothing using several methods
#'
#' Smoothing using several methods and returning a list of trend-cycle estimates.
#'
#' @inheritParams henderson_smoothing
#' @inheritParams henderson_robust_smoothing
#' @param methods list of methods to use.
#' @param ... other unused parameters.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- french_ipi[, "manufacturing"]
#' outliers <- x13_regarima_outliers(x)
#' all_methods <- smoothing(x, ao = outliers$ao, ao_tc = outliers$ao_tc, ls = outliers$ls)
#' ggsmoothing_plot(all_methods)
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
#' Produce several plots
#'
#' @inheritParams confint_plot
#' @inheritParams confint-tc
#' @param plots list of plots to use.
#' @param ... other unused parameters.
#'
#' @details
#' The following plots can be produced:
#'
#' - `"normal"` using [autoplot.tc_estimates()] function;
#' - `"confint"` using [ggconfint_plot()] function;
#' - `"lollypop"` using [gglollypop()] function;
#' - `"implicit_forecasts"` using [ggimplicit_forecasts_plot()] function;
#' - `"underlying_forecasts"` using [ggunderlying_forecasts_plot()] function.
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' x <- cars_registrations
#' tc <- henderson_smoothing(x)
#' ggsmoothing_plot(tc)
#'
#' @returns A list of ggplot objects of length equal to the number of plots requested by the parameter `plots`
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
