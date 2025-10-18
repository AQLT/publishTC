#' Segmented Smoothing around Breakpoints
#'
#' @inheritParams henderson_smoothing
#'
#' @param breaks A list of breakpoints (dates) where the series should be split for smoothing.
#' @param break_method Method to split the series (see details).
#' @param smoothing_method Smoothing method to use on each segment : [henderson_smoothing()] or [clf_smoothing()].
#' @param ... Other parameters passed to the smoothing method.
#'
#' @details
#' Two methods are available to estimate the trend-cycle around breakpoints:
#'
#' 1. `"one-side"`: The trend-cycle is estimated on the all data and the estimates are overwritten after each breakpoint with the estimates obtained by smoothing only the data after the breakpoint.
#'
#' 2. `"two-sides"`: The trend-cycle is estimated on each segment defined by the breakpoints and the estimates are combined to form the final trend-cycle.
#'
#' The `"parameters"` field of the returned object corresponds to the parameters of the last segment (used to build confidence intervals and implicit forecasts).

#' @examples
#' x <- window(publishTC::french_ipi[, "manufacturing"], start = 2015)
#' tc_h <- henderson_smoothing(x, length = 13)$tc
#' breaks <- list(c(2020, 3))
#' tc_h_2s <- segmented_smoothing(x, breaks = breaks, break_method = "two-sides", length = 13)$tc
#' tc_h_rob <- henderson_robust_smoothing(x, ls = c(2020+ (3-1)/12, 2020+ (4-1)/12))$tc
#'
#'  plot(window(x, start = 2019.5, end = 2021),
#'  main = "Smoothing around COVID-19",
#'  xlab = NULL, ylab = NULL,
#'  lty = 3
#'  )
#' lines(tc_h, col = "purple")
#' lines(tc_h_2s, col = "lightblue")
#' lines(tc_h_rob, col = "orange")
#' legend(
#' 	"bottomright",
#' 	legend = c("y", "Henderson", "Two-Sides Segmented", "Robust H. (2 LS)"),
#' 	col= c("black", "purple", "lightblue", "orange"),
#' 	lty = c(3, 1, 1, 1, 1),
#' 	cex = 0.7
#' )
#' @export
segmented_smoothing <- function(
		x,
		breaks = NULL,
		break_method = c("one-side", "two-sides"),
		smoothing_method = c("henderson_smoothing", "clf_smoothing"),
		...
) {
	# match arguments
	break_method <- match.arg(break_method)
	smoothing_method <- match.arg(smoothing_method)
	smoothing_method <- get(smoothing_method, mode = "function")
	if (is.null(breaks) || !is.list(breaks)) {
		warning("The breaks paramater should be a list. Parameter ignored.")
		return(smoothing_method(x, ...))
	}
	breaks <- lapply(breaks, dates_to_num, frequency = frequency(x))

	if (break_method == "one-side") {
		split_series <- lapply(c(list(NULL), breaks), window, x = x)
		all_tc <- lapply(split_series, smoothing_method)
	} else {
		extended_breaks <- c(list(NULL), breaks, list(NULL))
		split_series <- list()
		for (i in seq_len(length(breaks) + 1)) {
			end <- extended_breaks[[i + 1]]
			if (!is.null(end)) {
				end <- end - deltat(x)
			}
			split_series[[i]] <- window(
				x,
				start = extended_breaks[[i]],
				end = end
			)
		}
	}

	all_tc <- lapply(split_series, smoothing_method, ...)
	tc <- window(all_tc[[1]]$tc, start = start(x), end = end(x), extend = TRUE)
	all_tc <- all_tc[-1]
	last_tc <- all_tc[[length(all_tc)]]
	for (i in seq_along(all_tc)) {
		tc_part <- all_tc[[i]]$tc
		window(tc, start = start(tc_part), end = end(tc_part)) <- tc_part
	}
	res <- tc_estimates(
		tc = tc,
		sa = x,
		parameters = last_tc$parameters,
		extra_class = "henderson"
	)
	res
}

