#' Smoothing using the Cascade Linear Filter
#'
#' @inheritParams henderson_smoothing
#' @param endpoints Method used for the asymmetric filter.
#' If `endpoints = "cut-and-normalize"` (the default) the cut-and-normalise method is used,
#' otherwise the Asymmetric Linear Filter (ALF) filters are used.
#' @param ... other unused parameters.
#' @references
#' Dagum, E. B., & Luati, A. (2008). A Cascade Linear Filter to Reduce Revisions and False Turning Points for Real Time Trend-Cycle Estimation. *Econometric Reviews* 28 (1-3): 40‑59.
#' <https://doi.org/10.1080/07474930802387837>
#'
#'
#' @examplesIf rjd3jars::check_java_version(silent = TRUE)
#' tc <- clf_smoothing(cars_registrations)
#' plot(tc, xlim = c(2022, 2025))
#'
#' @returns An object of class `c("tc_estimates", "clf")`.
#' See [tc_estimates()] for a full description of the returned object.
#'
#'
#' @importFrom utils tail head
#' @export
clf_smoothing <- function(x,
						  endpoints = c("cut-and-normalize", "ALF"),
						  ...) {
	if (toupper(endpoints)[1] == "ALF") {
		endpoints <- "ALF"
		tc_coef <- CLF
	} else {
		endpoints <- "ALF"
		tc_coef <- CLF_CN
	}
	filtered <- rjd3filters::filter(x, tc_coef)
	res <- tc_estimates(
		tc = filtered,
		sa = x,
		parameters = list(
			tc_coef = tc_coef,
			endpoints = endpoints
		),
		extra_class = "clf"
	)
	res
}
