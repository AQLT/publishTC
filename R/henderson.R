# Local estimates of IC-ratios
# We replicate the direct estimates to have
# estimators of the slope and the concavity
local_daf_est <- function(p=6, q=p, d=3, dest = 1, kernel = "Henderson", X_sup = NULL){
	k <- rjd3filters::get_kernel(kernel, horizon = p)
	k <- c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
	K <- diag(k)
	X <- cbind(rjd3filters::polynomial_matrix(l = -p, u = q, d0 = 0, d1 = d), X_sup)
	e <- matrix(0, ncol = 1, nrow = d+1)
	e[dest + 1] <- 1
	MM <- K %*% X %*% solve(t(X) %*% K %*% X, e)
	rjd3filters::moving_average(MM, lags = -p)
}

local_daf_filter <- function(p=6, d=3, dest = 1, X_sup = NULL, ...){
	all_mm <- lapply(seq(p, 0), local_daf_est, p = p, d = d, dest = dest, X_sup = X_sup, ...)
	rjd3filters::finite_filters(all_mm[[1]], all_mm[-1])
}
#' @importFrom utils tail
#' @export
henderson_smoothing <- function(x,
								endpoints = c("Musgrave", "QL", "CQ", "CC", "DAF", "CN"),
								length = NULL, icr = NULL,
								local_icr = FALSE,
								local_var = TRUE,
								degree = 3,
								...) {
	if (is.null(length)) {
		param <- x11_trend_selection(x)
		length <- param["length"]
		if (is.null(icr) & !local_icr) {
			icr <- param["icr"]
		}
	} else {
		if (is.null(icr) & !local_icr) {
			icr <- find_icr(length, frequency(x))
		}
	}
	endpoints <- match.arg(toupper(endpoints)[1],
						   c("MUSGRAVE", "LC", "QL", "CQ", "CC", "DAF", "CN")
						   )
	if (endpoints == "MUSGRAVE") {
		endpoints <- "LC"
	}

	horizon <- (length - 1) / 2
	if (!local_icr || endpoints %in% c("CC", "DAF", "CN")) {
		lp_coef <- lp_filter(horizon = horizon,
							 ic = icr,
							 endpoints = endpoints,
							 ...)
		param_f <- NULL
		icr <- list(icr_l = icr, icr_r = icr)
	} else {
		local_param_est <- local_param_filter(x, icr = icr,
											  endpoints = endpoints,
											  horizon = horizon,
											  degree = degree,
											  local_var = local_var,
											  ...)
		lp_coef <- local_param_est$trend_f
		icr <- local_param_est$icr
		param_f <- local_param_est$param_f
	}
	filtered <- rjd3filters::filter(x, lp_coef)
	res <- list(
		tc = filtered,
		x = x,
		parameters = list(
			tc_coef = lp_coef,
			icr = icr,
			param_f = param_f
		)
	)
	class(res) <- c("tc_estimates", "henderson")
	res
}
local_param_filter <- function(x, icr = NULL,
							   endpoints = c("Musgrave", "QL", "QL", "CQ", "CC", "DAF", "CN"),
							   horizon = 6,
							   degree = 3,
							   local_var = TRUE,
							   min_icr = 10^-6,
							   ...){
	kernel <- "Henderson"
	endpoints <- match.arg(toupper(endpoints)[1],
						   c("MUSGRAVE", "LC", "QL", "CQ", "CC", "DAF", "CN")
	)
	if (endpoints == "MUSGRAVE") {
		endpoints <- "LC"
	}
	if (endpoints %in% c("CC", "DAF", "CN")) {
		return(henderson_smoothing(x, endpoints = endpoints,
								   length = horizon * 2 + 1,
								   icr = icr,
								   degree = degree,
								   ...))
	}

	icr_param <- check_icr(icr, horizon)
	icr_l <- icr_param$icr_l
	icr_r <- icr_param$icr_r

	local_param_f <- NULL
	dest <- switch (endpoints,
					LC = 1,
					QL = 2,
					CQ = 3
	)
	if (degree < dest) {
		degree <- dest
	}
	length <- horizon * 2 + 1
	local_param_f <- local_param_est[[
		as.character(length)
	]][[
		sprintf("d=%i", degree)
	]][[
		sprintf("dest=%i", dest)
	]]
	if (kernel != "Henderson" | is.null(local_param_f)) {
		local_param_f <- local_daf_filter(p = horizon, d = degree,
										  dest = dest, kernel = kernel)
	}
	sym_filter <- lp_filter(horizon = horizon)@sfilter
	if (is.null(icr_r)) {
		last_param <- tail(rjd3filters::filter(tail(x, horizon * 2 + 1),
											   local_param_f),
						   horizon)
		var <- rjd3filters::var_estimator(x, sym_filter)
		if (local_var) {
			icr_r <- 2/(sqrt(pi) * (last_param / sqrt(var)))
			icr_r[abs(icr_r) <= min_icr] <- min_icr
			default_f <- lapply(1:horizon, function(i){
				q <- horizon - i
				lp_filter(
					horizon = horizon,
					endpoints = endpoints,
					ic = icr_r[i],
					kernel = kernel
				)[, sprintf("q=%i", q)]
			})
			var <- sapply(default_f, rjd3filters::var_estimator, x = x)
		}
		icr_r <- 2/(sqrt(pi) * (last_param / sqrt(var)))
	}

	if (is.null(icr_l)) {
		last_param <- head(rjd3filters::filter(head(x, horizon * 2 + 1),
											   local_param_f),
						   horizon)

		var <- rjd3filters::var_estimator(x, sym_filter)
		if (local_var) {
			icr_l <- 2/(sqrt(pi) * (last_param / sqrt(var)))
			icr_l[abs(icr_l) <= min_icr] <- min_icr
			default_f <- lapply(1:horizon, function(i){
				lp_filter(
					horizon = horizon,
					endpoints = endpoints,
					ic = icr_l[i],
					kernel = kernel
				)@lfilters[[i]]
			})
			var <- sapply(default_f, rjd3filters::var_estimator, x = x)
		}
		icr_l <- 2/(sqrt(pi) * (last_param / sqrt(var)))
	}
	default_f <- lp_filter(horizon = horizon,
						   endpoints = endpoints,
						   ic = find_icr(length, frequency(x)),,
						   kernel = kernel)
	lfilters <- default_f@lfilters

	icr_l[abs(icr_l) <= min_icr] <- min_icr
	icr_r[abs(icr_r) <= min_icr] <- min_icr
	lfilters <- lapply(1:horizon, function(i){
		lp_filter(
			horizon = horizon,
			endpoints = endpoints,
			ic = icr_l[i],
			kernel = kernel
		)@lfilters[[i]]
	})
	rfilters <- lapply(1:horizon, function(i){
		q <- horizon - i
		lp_filter(
			horizon = horizon,
			endpoints = endpoints,
			ic = icr_r[i],
			kernel = kernel
		)[, sprintf("q=%i", q)]
	})
	list(trend_f = finite_filters(sym_filter, rfilters = rfilters, lfilters = lfilters),
		 param_f = local_param_f,
		 icr = list(icr_l = icr_l, icr_r = icr_r)
		 )
}

# Not used (in case we implement left/right icr)
check_icr <- function(icr, horizon) {
	if (is.null(icr) ||
		(is.list(icr) && length(icr) > 2) ||
		(is.vector(icr) && (!length(icr) %in% c(horizon, 2* horizon)))
	){
		return(list(icr_l = NULL, icr_r = NULL))
	}

	if (is.list(icr)) {
		if (length(icr == 2)) {
			icr_l <- icr[[1]]
			icr_r <- icr[[2]]
		} else {
			icr_l <- NULL
			icr_r <- icr[[1]]
		}
	} else {
		if (length(icr) == horizon) {
			icr_l <- NULL
			icr_r <- icr
		} else {
			icr_l <- icr[1:horizon]
			icr_r <- icr[(horizon + 1):(2*horizon)]
		}
	}
	return(list(icr_l = icr, icr_r = icr))
}
