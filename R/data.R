#' Classical Moving Average
#'
#' Classical moving average for trend-cycle extraction.
#'
#' @details
#' `henderson` contains the Henderson moving average of length 5, 7, 9, 13 and 23.
#'
#' `CLF` contains the Cascade Linear Filter (CLF) of length 13 and the associated Asymmetric Linear Filters (ALF).
#'
#' `CLF_CN` contains the Cascade Linear Filter (CLF) of length 13 and the associated cut and normalise asymetric filters.
#'
#'
#' @references
#' Dagum, E. B., & Luati, A. (2008). A Cascade Linear Filter to Reduce Revisions and False Turning Points for Real Time Trend-Cycle Estimation. *Econometric Reviews* 28 (1-3): 40‑59. <https://doi.org/10.1080/07474930802387837>.
#'
#' Henderson, R. (1916). Note on graduation by adjusted average. *Transactions of the actuarial society of America* 17: 43‑48.
#'
#' Quartier-la-Tente, A. (2024). Improving Real-Time Trend Estimates Using Local Parametrization of Polynomial Regression Filters. *Journal of Official Statistics, 40*(4), 685-715. <https://doi.org/10.1177/0282423X241283207>.
#' @docType data
#' @format `henderson` is `list()` of `"moving_average"`.
#' @rdname classical-ma
#' @name classical-ma
"henderson"
#' @format `CLF` is a `"finite_filters"`.
#' @name classical-ma
"CLF"
#' @format `CLF_CN` is a `"finite_filters"`.
#' @name classical-ma
"CLF_CN"
#' @format `local_param_est` is `list()` of `"finite_filters"`.
#' @name classical-ma
"local_param_est"

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

#' Data set examples
#'
#' All data sets examples used in the paper Quartier-la-Tente (2025).
#'
#' - `cars_registrations`: monthly new passenger cars registrations in France, published in October 2024.
#'
#' - `french_ipi`: monthly industrial production index in France for Crude Petroleum, Motor Vehicles and Manufacturing published in October 2024.
#'
#' - `fred`: the series CE16OV (Civilian Employment Level) and RETAILx (Retail and Food Services Sales) from the FRED-MD database published in November 2022.
#'
#' - `simulated_data`: simulated trends of degree 0, 1 and 2 with an Additive Outlier (AO) or Level Shift (LS) in January 2022
#'
#' `etip` corresponds to the Expected trend in production (balance of opinion) in the French Manufacturing industry, published in May 2025 in
#' the monthly business survey in goods-producing industries by the INSEE.
#'
#' @references
#' Quartier-la-Tente, A. (2025). Estimation de la tendance-cycle avec des méthodes robustes aux points atypiques. <https://github.com/AQLT/robustMA>.
#' McCracken, Michael W., et Serena Ng. 2016. FRED-MD: A Monthly Database for Macroeconomic Research. Journal of Business & Economic Statistics 34 (4): 574‑89. <https://doi.org/10.1080/07350015.2015.1086655>.
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
#' @docType data
#' @name ts-exemple
"etip"
#
# etip <- read.ts("../publishTC.wp/data/CONJ_INDUSTRIE/2025_05.csv")[,"tppre"]
# usethis::use_data(etip, overwrite = TRUE)
# list.files("../publishTC.wp/data/CONJ_INDUSTRIE")



