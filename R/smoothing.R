
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
		local_var = TRUE,
		degree = 3,
		ao = NULL,
		ao_tc = NULL,
		ls = NULL,
		...) {

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
			local_var = local_var,
			degree = degree,
			local_icr = TRUE)
	}
	if ("henderson_robust" %in% methods) {
		res$henderson_robust <- henderson_robust_smoothing(
			x = x, endpoints = endpoints, length = length, icr = icr,
			local_var = local_var,
			degree = degree,
			ao = ao, ao_tc = ao_tc, ls = ls,
			local_icr = FALSE)
	}
	if ("henderson_robust_localic" %in% methods) {
		res$henderson_robus_localic <- henderson_robust_smoothing(
			x = x, endpoints = endpoints, length = length, icr = icr,
			local_var = local_var,
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
	res
}
