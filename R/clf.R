#' Smoothing using the Cascade Linear Filter
#'
#' @param x input time-series.
#' @param endpoints Method used for the asymmetric filter.
#' If `endpoints = "cut-and-normalize"` (the default) the cut-and-normalise method is used,
#' otherwise the Asymmetric Linear Filter (ALF) filters are used.

#' @references
#' Dagum, E. B., & Luati, A. (2008). A Cascade Linear Filter to Reduce Revisions and False Turning Points for Real Time Trend-Cycle Estimation. *Econometric Reviews* 28 (1-3): 40‑59.
#' <https://doi.org/10.1080/07474930802387837>
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
