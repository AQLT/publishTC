
#' Smoothing using several methods
#'
#' @inheritParams henderson_smoothing
#' @inheritParams henderson_robust_smoothing
#' @param methods list of methods to use.
#' @param ... other unused parameters.
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
		res$henderson_robus_localic <- henderson_robust_smoothing(
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
#' @export
ggsmoothing_plot <- function(
		object,
		methods = c("normal", "confint", "lollypop", "implicit_forecasts"),
		level = 0.95,
		...) {
	methods <- tolower(methods)
	res <- list()
	if ("normal" %in% methods) {
		res$normal <- ggplot2::autoplot(
			object = object,
			...
			) +
			ggplot2::ggtitle("Normal plot")
		if (!is.null(names(methods)) && names(methods)[methods %in% "normal"]) {
			res$normal <- res$normal +
				ggplot2::ggtitle(names(methods)[methods %in% "normal"])
		}
	}

	if ("confint" %in% methods) {
		res$confint <- ggconfint_plot(
			object = object, level = level, ...
			) +
			ggplot2::ggtitle("Confidence intervals")
		if (!is.null(names(methods)) && names(methods)[methods %in% "confint"]) {
			res$confint <- res$confint +
				ggplot2::ggtitle(names(methods)[methods %in% "confint"])
		}
	}

	if ("lollypop" %in% methods) {
		res$lollypop <- gglollypop(
			object = object,
			...
			) +
			ggplot2::ggtitle("Lollypop")
		if (!is.null(names(methods)) && names(methods)[methods %in% "lollypop"]) {
			res$lollypop <- res$lollypop +
				ggplot2::ggtitle(names(methods)[methods %in% "lollypop"])
		}
	}

	if ("implicit_forecasts" %in% methods) {
		res$implicit_forecasts <- ggimplicit_forecasts_plot(
			object = object, ...
			) +
			ggplot2::ggtitle("Implicit forecasts")
		if (!is.null(names(methods)) && names(methods)[methods %in% "implicit_forecasts"]) {
			res$implicit_forecasts <- res$implicit_forecasts +
				ggplot2::ggtitle(names(methods)[methods %in% "implicit_forecasts"])
		}
	}
	res
}
