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
