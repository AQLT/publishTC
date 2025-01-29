# Local estimates of IC-ratios
# We replicate the direct estimates to have
# estimators of the slope and the concavity
local_daf_est <- function(p=6, q=p, d=3, dest = 1, kernel = "Henderson"){
	k <- rjd3filters::get_kernel(kernel, horizon = p)
	k <- c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
	K <- diag(k)
	X <- rjd3filters::polynomial_matrix(l = -p, u = q, d0 = 0, d1 = d)
	e <- matrix(0, ncol = 1, nrow = d+1)
	e[dest + 1] <- 1
	MM <- K %*% X %*% solve(t(X) %*% K %*% X, e)
	rjd3filters::moving_average(MM, lags = -p)
}

local_daf_filter <- function(p=6, d=3, dest = 1, ...){
	all_mm <- lapply(p:0, local_daf_est, p = p, d = d, dest = dest, ...)
	rjd3filters::finite_filters(all_mm[[1]], all_mm[-1])
}
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
							   horizon = 6, kernel = "Henderson",
							   degree = 3,
							   local_var = TRUE,
							   ...){
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

	local_param_f <- NULL
	if (!is.null(icr) || length(icr) != horizon) {
		dest <- switch (endpoints,
						LC = 1,
						QL = 2,
						CQ = 3
		)
		if (degree < dest) {
			degree <- dest
		}
		length <- horizon * 2 + 1
		icr <- find_icr(length, frequency(x))
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

		last_param <- tail(rjd3filters::filter(tail(x, horizon * 2 + 1),
											   local_param_f),
						   horizon)
		sym_filter <- lp_filter(horizon = horizon)@sfilter
		var <- rjd3filters::var_estimator(x, sym_filter)
		if (local_var) {
			icr <- 2/(sqrt(pi) * (last_param / sqrt(var)))
			default_f <- lapply(1:horizon, function(i){
				q <- horizon - i
				lp_filter(
					horizon = horizon,
					endpoints = endpoints,
					ic = icr[i],
					kernel = kernel
				)[, sprintf("q=%i", q)]
			})
			var <- sapply(default_f, rjd3filters::var_estimator, x = x)
		}
		icr <- 2/(sqrt(pi) * (last_param / sqrt(var)))
	}

	rfilters <- lapply(1:horizon, function(i){
		q <- horizon - i
		lp_filter(
			horizon = horizon,
			endpoints = endpoints,
			ic = icr[i],
			kernel = kernel
		)[, sprintf("q=%i", q)]
	})
	list(trend_f = finite_filters(sym_filter, rfilters = rfilters),
		 param_f = local_param_f,
		 icr = icr)
}
