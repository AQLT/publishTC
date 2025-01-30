#' @importFrom rjd3filters polynomial_matrix mmsre_filter finite_filters
#' @export
henderson_robust_smoothing <- function(x,
									   endpoints = c("Musgrave", "QL", "CQ", "DAF"),
									   length = NULL,
									   ao = NULL,
									   ao_tc = NULL,
									   ls = NULL,
									   icr = NULL,
									   local_icr = FALSE,
									   local_var = TRUE,
									   degree = 3,
									   ...) {
	kernel <- "Henderson"
	min_icr <- 10^-6
	max_bias <- 2/(sqrt(pi) * (min_icr))
	endpoints <- match.arg(toupper(endpoints)[1],
						   c("MUSGRAVE", "LC", "QL", "CQ", "DAF")
	)
	if (endpoints == "MUSGRAVE") {
		endpoints <- "LC"
	}
	if (endpoints == "DAF") {
		local_icr <- FALSE
	}
	dest <- switch (endpoints,
					LC = 1,
					QL = 2,
					CQ = 3,
					DAF = 3,
	)
	if (degree < dest) {
		degree <- dest
	}
	if (is.null(ao) & is.null(ao_tc) & is.null(ls)) {
		return(henderson_smoothing(x, endpoints = endpoints, length = length, icr = icr, degree = degree, ...))
	}
	if (is.null(length)) {
		param <- x11_trend_selection(x)
		length <- param["length"]
		if (is.null(icr) & !local_icr) {
			icr <- param["icr"]
		}
	} else {
		if (is.null(icr)) {
			icr <- find_icr(length, frequency(x))
		}
	}

	dates_x <- as.numeric(time(x))
	ao_reg <- ao_tc_reg <- ls_reg <- NULL
	if (!is.null(ao)) {
		ao_reg <- ts(sapply(round(ao, 3), `%in%`, x = round(dates_x, 3)),
					 start = start(x), frequency = frequency(x))
	}
	if (!is.null(ao_tc)) {
		ao_tc_reg <- ts(sapply(round(ao_tc, 3), `%in%`, x = round(dates_x, 3)),
						start = start(x), frequency = frequency(x))
	}
	if (!is.null(ls)) {
		ls_reg <- ts(sapply(round(ls, 3), `%in%`, x = round(dates_x, 3)),
					 start = start(x), frequency = frequency(x))
	}
	fun_out <- c(rep(list(gen_ao), length(ao)),
				 rep(list(gen_ao_tc), length(ao_tc)),
				 rep(list(gen_ls), length(ls)))
	reg <- cbind(ao_reg, ao_tc_reg, ls_reg)

	h <- (length - 1) / 2
	default_filter <- lp_filter(horizon = h,
								ic = icr,
								endpoints = endpoints,
								kernel = kernel)
	sym_coef <- default_filter@sfilter
	lb <- lower_bound(sym_coef)
	ub <- upper_bound(sym_coef)
	n <- length(x)
	if (endpoints == "DAF") {
		U <- polynomial_matrix(l = - h, d0 = dest, d1 = dest)
		Z <- NULL
	} else {
		U <- polynomial_matrix(l = - h, d0 = dest - 1, d1 = dest - 1)
		Z <- polynomial_matrix(l = - h, d0 = dest, d1 = dest)
	}
	H <- matrix(0,ncol = n, nrow = n)

	############################
	#### Create hat matrix #####
	############################

	for (i in 1:n) {
		if (i <= abs(lb)) {
			# left filters
			c_coef <- default_filter@lfilters[[i]]
			focus_reg <- window(reg,
								start = dates_x[i + lower_bound(c_coef)] ,
								end = dates_x[i + upper_bound(c_coef)],
								extend = TRUE)
			X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
			if (!is.null(X)) {
				# invert the order of the ligns
				X <- X[nrow(X):1,, drop = FALSE]
				sym <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
				q <- i - 1
				X <- check_matrix_reg(X, q)
				c_coef <- mmsre_filter(
					ref_filter = sym,
					q = q,
					delta = 2 / (sqrt(pi) * icr),
					U = cbind(U, X),
					Z = Z
				)
				c_coef <- rev(c_coef) # reverse the order of the coefficients
			}
		} else if (i > n - ub) {
			# right filters
			c_coef <- default_filter@rfilters[[i - (n-ub)]]
			focus_reg <- window(reg,
								start = dates_x[i + lower_bound(c_coef)] ,
								end = dates_x[i + upper_bound(c_coef)],
								extend = TRUE)
			X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
			if (!is.null(X)) {
				sym <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
				q <- n - i
				X <- check_matrix_reg(X, q)
				c_coef <- mmsre_filter(
					ref_filter = sym,
					q = q,
					delta = 2 / (sqrt(pi) * icr),
					U = cbind(U, X),
					Z = Z
				)
			}

		} else {
			# central symmetric filters
			c_coef <- sym_coef
			focus_reg <- window(reg,
								start = dates_x[i + lower_bound(c_coef)] ,
								end = dates_x[i + upper_bound(c_coef)],
								extend = TRUE)
			X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
			if (!is.null(X)) {
				c_coef <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
			}

		}
		H[i, seq(i + lower_bound(c_coef), length.out = length(c_coef))] <- coef(c_coef)
	}

	##################################
	#### Local estimation of ICR #####
	##################################

	if (local_icr) {
		if (!is.null(icr) || length(icr) != h) {
			icr <- NULL
		}

		if (is.null(icr)) {
			H_tmp <- H
			if (local_var) {
				# remove asymmetric MA
				H_tmp[c(1:abs(lb), (n - ub+1):n),] <- 0
			}
			var <- variance_hat_matrix(x, H_tmp)
		}

		for (i in (n-ub+1):n) {
			c_coef <- default_filter@rfilters[[i - (n-ub)]]
			focus_reg <- window(reg,
								start = dates_x[i + lower_bound(c_coef)] ,
								end = dates_x[i + upper_bound(c_coef)],
								extend = TRUE)
			q <- n - i
			X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
			sym <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
			if (is.null(icr)) {
				local_param_f <- local_daf_est(p = h, q = q, d = degree,
											   dest = dest, kernel = kernel,
											   X_sup = X)
				param_i <- rjd3filters::filter(window(
					x,
					start = dates_x[i + lower_bound(c_coef)] ,
					end = dates_x[i + upper_bound(c_coef)],
					extend = TRUE
				),
				local_param_f)
				param_i <- param_i[abs(lower_bound(local_param_f)) + 1]
				bias_i <- param_i / sqrt(var)
				bias_i[bias_i >= max_bias] <- max_bias

				if (local_var) {
					default_ma <- mmsre_filter(
						ref_filter = sym,
						q = q,
						delta = bias_i,
						U = U,
						Z = Z
					)
					H_asym <- build_robust_hat_matrix_ma(
						reg = reg,
						fun_out = fun_out,
						delta = bias_i,
						default_ma = default_ma,
						U = U,
						Z = Z,
						degree = degree
					)
					tmp_var <- variance_hat_matrix(x, H_asym)
					bias_i <- param_i / sqrt(tmp_var)
				}
			} else {
				bias_i <- 2/(sqrt(pi) * (icr[i - (n-ub)]))
			}
			bias_i[bias_i >= max_bias] <- max_bias
			X <- check_matrix_reg(X, q)
			c_coef <- mmsre_filter(
				ref_filter = sym,
				q = q,
				delta = bias_i,
				U = cbind(U, X),
				Z = Z
			)
			H[i, ] <- 0
			H[i, seq(i + lower_bound(c_coef), length.out = length(c_coef))] <- coef(c_coef)
		}
	}

	filtered <- ts(H %*% x,
				   start = start(x),
				   frequency = frequency(x))


	res <- list(
		tc = filtered,
		x = x,
		parameters = list(
			hat_matrix = H,
			icr = icr
		)
	)
	class(res) <- c("tc_estimates", "robust_henderson")
	res
}

build_robust_hat_matrix_ma <- function(reg, fun_out, delta, default_ma, U, Z, degree) {
	kernel <- "Henderson"
	dates_x <- as.numeric(time(reg))
	n <- length(dates_x)
	H <- matrix(0,ncol = n, nrow = n)
	lb <- lower_bound(default_ma)
	ub <- q <- upper_bound(default_ma)
	h <- max(abs(lb), ub)
	for (i in (abs(lb) + 1):(n-ub)) {
		# right filters
		c_coef <- default_ma
		focus_reg <- window(reg,
							start = dates_x[i + lb] ,
							end = dates_x[i + ub],
							extend = TRUE)
		X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
		if (!is.null(X)) {
			sym <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
			X <- check_matrix_reg(X, q)
			c_coef <- mmsre_filter(
				ref_filter = sym,
				q = q,
				delta = delta,
				U = cbind(U, X),
				Z = Z
			)
		}
		H[i, seq(i + lb, length.out = length(default_ma))] <- coef(c_coef)
	}
	H
}


variance_hat_matrix <- function(x, H) {
	n <- length(x)
	non_estimate <- apply(H == 0, 1, all)
	nobs <- n - sum(non_estimate)
	sc <- H %*% x
	sc[non_estimate] <- NA
	nu1 <- sum(diag(H))
	nu2 <- sum(diag(t(H) %*% H))
	res <- sum((sc - x)^2, na.rm = TRUE)
	res / (nobs - 2*nu1 + nu2)
}
sym_robust_filter <- function(X = NULL, kernel = "Henderson", degree = 3,
							  horizon = 6) {
	kernel <- rjd3filters::get_kernel(kernel, horizon = horizon)
	K <- diag(sapply(-horizon:horizon, function(i) kernel[i]))
	X_full <- cbind(
		rjd3filters::polynomial_matrix(l = -horizon, u = horizon, d0 = 0, d1 = degree),
		X)

	e_1 <- rep(0, ncol(X_full))
	e_1[1] <- 1
	rjd3filters::moving_average(
		K %*% X_full %*% solve(t(X_full) %*% K %*% X_full, e_1),
		lags = - horizon)
}

build_matrix_reg <- function(focus_reg, fun_out, h, current_date, U = NULL) {
	if (!is.matrix(focus_reg))
		focus_reg <- matrix(focus_reg, ncol = 1)
	X <- do.call(cbind, lapply(seq_along(fun_out), function(nb_col){
		if (!any(focus_reg[,nb_col]))
			return(NULL)
		t <- round((time(focus_reg)[which(focus_reg[,nb_col])] - current_date) * frequency(focus_reg))
		fun_out[[nb_col]](t, h = h)
	}))
	if (is.null(X))
		return(NULL)
	check_matrix_reg(X = X, U = U)
}
check_matrix_reg <- function(X, U = NULL, q = NULL) {
	if (is.null(X))
		return(X)
	h <- (nrow(X) - 1) / 2
	if (is.null (q))
		q <- h
	non_constant <- apply(X[1:(h+q+1),,drop = FALSE],
						  2,
						  function(x) length(unique(x))) !=1
	X <- X[,non_constant, drop = FALSE]
	# Check for multicolinearity
	X_aug <- cbind(X, U)
	qr_decomp <- qr(X_aug[1:(h+q+1),,drop = FALSE])
	independent_cols <- qr_decomp$pivot[1:qr_decomp$rank]
	independent_cols <- independent_cols[independent_cols <= ncol(X)]
	X <- X[, independent_cols, drop = FALSE]
	if (ncol(X) == 0)
		return(NULL)
	X
}
#'@importFrom rjd3filters is.moving_average lower_bound upper_bound
hat_matrix <- function(n, coef) {
	if (is.moving_average(coef)) {
		sym_coef <- coef
		lb <- abs(lower_bound(sym_coef))
		ub <- upper_bound(sym_coef)
	} else {
		sym_coef <- coef@sfilter
		lb <- length(coef@lfilters)
		ub <- length(coef@rfilters)
	}
	H <- matrix(0,ncol = n, nrow = n)

	if(is.moving_average(coef)) {
		for (i in seq(1+lb, n - ub)){
			H[i,seq(i + lower_bound(sym_coef), length.out = length(sym_coef))] <- coef(sym_coef)
		}
	} else {
		for (i in 1:n) {
			if (i <= lb) {
				c_coef <- coef@lfilters[[i]]
			} else if (i > n - ub) {
				c_coef <- coef@rfilters[[i - (n-ub)]]
			} else {
				c_coef <- sym_coef
			}
			H[i, seq(i + lower_bound(c_coef), length.out = length(c_coef))] <- coef(c_coef)
		}
	}
	H
}
