# Robust Trend-Cycle Estimation using the Henderson Filter

Estimates the trend-cycle component using Henderson moving averages
robust to outliers (additive outliers and level shifts).

## Usage

``` r
henderson_robust_smoothing(
  x,
  endpoints = c("Musgrave", "QL", "CQ", "DAF"),
  length = NULL,
  ao = NULL,
  ao_tc = NULL,
  ls = NULL,
  icr = NULL,
  local_icr = FALSE,
  asymmetric_var = FALSE,
  degree = 3,
  ...
)
```

## Arguments

- x:

  An input time series (object of class `"ts"` or `"mts"`).

- endpoints:

  Character string specifying the method used for asymmetric filters. By
  default, `"musgrave"` is used.

- length:

  An integer specifying the length of the symmetric filter. If `NULL`,
  the length is selected automatically using
  [`x11_trend_selection()`](https://aqlt.github.io/publishTC/reference/x11_trend_selection.md).

- ao:

  Vector of dates for Additive Outliers (AO) whose effects are allocated
  to the irregular component.

- ao_tc:

  Vector of dates for Additive Outliers (AO) whose effects are allocated
  to the trend-cycle component.

- ls:

  Vector of dates for Level Shifts (LS) whose effects are allocated to
  the trend-cycle component.

- icr:

  Numeric value specifying the Irregular-to-Trend (I/C) ratio used for
  asymmetric filters.

- local_icr:

  Logical. If `TRUE`, the I/C ratio is estimated locally (as described
  in Quartier-la-Tente, 2024) instead of globally.

- asymmetric_var:

  Logical. When `local_icr = TRUE`, if `TRUE`, the variance is estimated
  for each asymmetric filter. If `FALSE` (the default), the variance
  associated with symmetric estimates is used throughout.

- degree:

  Integer. When `local_icr = TRUE`, degree of the polynomial used to
  estimate the local bias parameter.

- ...:

  Additional arguments passed to
  [`rjd3filters::lp_filter()`](https://rjdverse.github.io/rjd3filters/reference/lp_filter.html).

## Value

An object of class `c("tc_estimates", "robust_henderson")`. See
[`tc_estimates()`](https://aqlt.github.io/publishTC/reference/tc_estimates.md)
for a full description of the returned object.

## Details

When outliers are present in a time series, standard linear filters like
the Henderson moving average can spread their impact across adjacent
trend estimates. This function explicitly accounts for specified
Additive Outliers (AO) and Level Shifts (LS) during the filtering
process to prevent distortion of the trend-cycle estimates as described
in Quartier-la-Tente (2025).

In a nutshell, Henderson smoothing is equivalent to a local polynomial
regression of degree 3 (or equivalently 2) estimated with weighted least
squares, using specific weights to obtain the Henderson coefficients.
Adding regressors to this local regression to account for outliers
allows removing their influence on the estimation of polynomial
coefficients and thus on the trend-cycle estimates.

## References

Quartier-la-Tente, A. (2025). Estimation de la tendance-cycle avec des
méthodes robustes aux points atypiques.
<https://github.com/AQLT/robustMA>.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
outliers <- x13_regarima_outliers(x)
tc <- henderson_robust_smoothing(x, ao = outliers$ao, ls = outliers$ls, local_icr = TRUE)

tc_henderson <- henderson_smoothing(x)
plot(tc, xlim = c(2019, 2022))
lines(tc_henderson$tc, col = "blue")
}
```
