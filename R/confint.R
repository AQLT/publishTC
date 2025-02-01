#' @export
#' @importFrom rjd3filters confint_filter
confint.henderson <- function(object, parm, level = 0.95, ...){
	confint_filter(x = object$x, coef = object$parameters$tc_coef, level = level,
				   gaussian_distribution = FALSE, exact_df = TRUE, ...)
}
#' @export
confint.robust_henderson <- function(object, parm, level = 0.95, ...){
	c <- (1 - level) / 2
	c <- c(c, 1 - c)
	filtered <- object$tc
	x <- object$x
	parm <- object$parameters
	n <- length(filtered)
	H <- parm$hat_matrix
	U <- parm$U
	Z <- parm$Z
	f_reg <- build_reg(x = x,
					   ao = parm$out$ao,
					   ao_tc = parm$out$ao_tc,
					   ls = parm$out$ls)
	fun_out <- f_reg$fun_out
	reg <- f_reg$reg
	kernel <- parm$kernel

	degree <- ncol(U) + ncol(Z)
	length <- nrow(U)
	h <- (length - 1) / 2
	bias_r <-  2/(sqrt(pi) * (parm$icr$icr_r))
	bias_l <-  2/(sqrt(pi) * (parm$icr$icr_l))
	if (length(bias_r) == 1) {
		bias_r <- rep(bias_r, h)
	}
	if (length(bias_l) == 1) {
		bias_l <- rep(bias_l, h)
	}
	corr_f <- sqrt(rowSums(H^2))
	H_sym <- H
	# remove asymmetric MA
	H_sym[c(1:h, (n - h+1):n),] <- 0
	var <- ts(variance_hat_matrix(x, H_sym),
			  start = start(filtered), end = end(filtered),
			  frequency = frequency(filtered))
	quantile <- ts(matrix(qt(c, df = df_var_hat_matrix(H_sym)), ncol = 2),
				   start = start(filtered), end = end(filtered),
				   frequency = frequency(filtered))
	dates_x <- as.numeric(time(filtered))
	for (i in (n-h+1):n) {
		focus_reg <- window(reg,
							start = dates_x[i - h ] ,
							end = dates_x[i + h - (i - (n - h))],
							extend = TRUE)
		q <- n - i
		X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
		sym <- sym_robust_filter(X = X, kernel = kernel, degree = degree, horizon = h)
		default_ma <- mmsre_filter(
			ref_filter = sym,
			q = q,
			delta = bias_r[i - (n - h)],
			U = U,
			Z = Z
		)
		H_asym <- build_robust_hat_matrix_ma(
			reg = reg,
			fun_out = fun_out,
			delta =  bias_r[i - (n - h)],
			default_ma = default_ma,
			U = U,
			Z = Z,
			degree = degree
		)
		var[i] <- variance_hat_matrix(x, H_asym)
		quantile[i,] <- qt(c, df = df_var_hat_matrix(H_asym))
	}
	for (i in 1:h) {
		focus_reg <- window(reg,
							start = dates_x[1] ,
							end = dates_x[i + h],
							extend = TRUE)
		q <- i
		X <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
		X <- X_invert <- build_matrix_reg(focus_reg, fun_out, h, U = U, current_date = dates_x[i])
		if (!is.null(X_invert)){
			X_invert <- X_invert[nrow(X_invert):1,, drop = FALSE]
		}
		sym <- sym_robust_filter(X = X_invert,
								 kernel = kernel, degree = degree, horizon = h)
		default_ma <- mmsre_filter(
			ref_filter = sym,
			q = q,
			delta = bias_l[i],
			U = U,
			Z = Z
		)
		default_ma <- rev(default_ma)
		H_asym <- build_robust_hat_matrix_ma(
			reg = reg,
			fun_out = fun_out,
			delta = bias_l[i],
			default_ma = default_ma,
			U = U,
			Z = Z,
			degree = degree,
			lfilter = TRUE
		)
		var[i] <- variance_hat_matrix(x, H_asym)
		quantile[i,] <- qt(c, df = df_var_hat_matrix(H_asym))
	}

	inf <- filtered + quantile[,1] * sqrt(var) * corr_f
	sup <- filtered + quantile[,2] * sqrt(var) * corr_f
	res <- ts.union(filtered, inf, sup)
	colnames(res) <- c("filtered", sprintf("%.1f%%", c * 100))
	res
}


