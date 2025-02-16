#' @export
print.tc_estimates <- function(x, ...) {
	print(x["tc"])
}

tc_estimates <- function(tc, sa, parameters = NULL, extra_class = NULL, ...) {
	res <- list(
		tc = tc,
		x = sa,
		parameters = parameters
	)
	class(res) <- c("tc_estimates", extra_class)
	res
}
is_tc_estimates <- function(x) {
	inherits(x, "tc_estimates")
}

