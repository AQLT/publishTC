bandwidth <- function(x) {
	UseMethod("bandwidth")
}
bandwidth.henderson <- function(x) {
	sfilter <- x$parameters$tc_coef@sfilter
	upper_bound(sfilter)
}
bandwidth.clf <- function(x) {
	sfilter <- x$parameters$tc_coef@sfilter
	upper_bound(sfilter)
}
bandwidth.robust_henderson<- function(x) {
	U <- x$parameters$U
	length <- nrow(U)
	(length - 1) / 2
}
