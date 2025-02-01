gen_ls <- function(t, h = 6){
	if (t > -h & t <= h) {
		if (t <= 0) {
			X <- c(rep(-1,h+t), 0, rep(0, h-t))
		} else {
			X <- c(rep(0,h+t), 1, rep(1, h-t))
		}
		X <- matrix(X, ncol = 1)
	} else {
		X <- NULL
	}
	X
}
gen_ao <- function(t, h = 6){
	if (abs(t) <= h) {
		X <- c(rep(0, 6+t), 1, rep(0, 6-t))
		X <- matrix(X, ncol = 1)
	} else {
		X <- NULL
	}
	X
}
gen_ao_tc <- function(t, h = 6){
	X <- gen_ao(t, h)
	if (abs(t) <= h) {
		if (t <= 0 & t != -h) {
			X <- 1-X
		} else {
			X <- X
		}
		X <- matrix(X, ncol = 1)
	} else {
		X <- NULL
	}
	X
}
build_reg <-  function(x, ao, ao_tc, ls, ...){
	dates <- as.numeric(time(x))
	ao_reg <- ao_tc_reg <- ls_reg <- NULL
	if (!is.null(ao)) {
		ao_reg <- ts(sapply(round(ao, 3), `%in%`, x = round(dates, 3)),
					 start = start(x), frequency = frequency(x))
	}
	if (!is.null(ao_tc)) {
		ao_tc_reg <- ts(sapply(round(ao_tc, 3), `%in%`, x = round(dates, 3)),
						start = start(x), frequency = frequency(x))
	}
	if (!is.null(ls)) {
		ls_reg <- ts(sapply(round(ls, 3), `%in%`, x = round(dates, 3)),
					 start = start(x), frequency = frequency(x))
	}
	fun_out <- c(rep(list(gen_ao), length(ao)),
				 rep(list(gen_ao_tc), length(ao_tc)),
				 rep(list(gen_ls), length(ls)))
	reg <- cbind(ao_reg, ao_tc_reg, ls_reg)
	list(fun_out = fun_out, reg = reg)
}
