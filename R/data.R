#' Classical Moving Average
#' @docType data
#' @rdname classical-ma
#' @name classical-ma
"henderson"
#' @docType data
#' @name classical-ma
"CLF"
#' @docType data
#' @name classical-ma
"CLF_CN"
#' @docType data
#' @name classical-ma
"local_param_est"

#' Data set examples
#' @docType data
#' @rdname ts-exemple
#' @name ts-exemple
"cars_registrations"
#' @docType data
#' @name ts-exemple
"french_ipi"
#' @docType data
#' @name ts-exemple
"fred"
#' @docType data
#' @name ts-exemple
"simulated_data"


# H5 <- lp_filter(horizon = 2)@sfilter
# H7 <- lp_filter(horizon = 3)@sfilter
# H9 <- lp_filter(horizon = 4)@sfilter
# H13 <- lp_filter(horizon = 6)@sfilter
# H23 <- lp_filter(horizon = 11)@sfilter
# henderson <- list(H5, H7, H9, H13, H23)
# names(henderson) <- c(5, 7, 9, 13 , 23)
#
# CLF <- list(
# 	c(-0.027, -0.007, 0.031, 0.067, 0.136, 0.188, 0.224, 0.188, 0.136, 0.067, 0.031, -0.007, -0.027),
# 	c(-0.026, -0.007, 0.030, 0.065, 0.132, 0.183, 0.219, 0.183, 0.132, 0.065, 0.030, -0.006),
# 	c(-0.026, -0.007, 0.030, 0.064, 0.131, 0.182, 0.218, 0.183, 0.132, 0.065, 0.031),
# 	c(-0.025, -0.004, 0.034, 0.069, 0.137, 0.187, 0.222, 0.185, 0.131, 0.064),
# 	c(-0.020, 0.005, 0.046, 0.083, 0.149, 0.196, 0.226, 0.184, 0.130),
# 	c(0.001, 0.033, 0.075, 0.108, 0.167, 0.205, 0.229, 0.182),
# 	c(0.045, 0.076, 0.114, 0.134, 0.182, 0.218, 0.230)
# )
# CLF <- rjd3filters::finite_filters(CLF)
# CLF_CN <- lapply(0:6, function(i) {
# 	sfilter <- CLF@sfilter
# 	if (i==0)
# 		return(coef(sfilter))
# 	coef_cut <- rev(rev(coef(sfilter))[seq.int(-1, by = -1, length.out = i)])
# 	coef_norm <- coef_cut / sum(coef_cut)
# 	coef_norm
# })
# CLF_CN <- rjd3filters::finite_filters(CLF_CN)
# local_param_est <- lapply(c(5, 7, 9, 13 , 23), function(length) {
# 	if (length == 5) {
# 		max_d <- 2
# 	} else {
# 		max_d <- 3
# 	}
# 	res <- lapply(2:max_d, function(d){
# 		res <- lapply(1:min(d, 3), function(dest){
# 			local_daf_filter(p = (length - 1) / 2,
# 							 d = d,
# 							 dest = dest)
# 		}
# 		)
# 		names(res) <- sprintf("dest=%s", 1:min(d, 3))
# 		res
#
# 	})
# 	names(res) <- sprintf("d=%s", 2:max_d)
# 	res
# })
# names(local_param_est) <- c(5, 7, 9, 13 , 23)
#
# usethis::use_data(henderson, overwrite = TRUE)
# usethis::use_data(CLF, overwrite = TRUE)
# usethis::use_data(CLF_CN, overwrite = TRUE)
# usethis::use_data(local_param_est, overwrite = TRUE)
