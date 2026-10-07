# Trend-Cycle Estimation using the Henderson Filter

Estimates the trend-cycle component using Henderson moving averages.

## Usage

``` r
henderson_smoothing(
  x,
  endpoints = c("Musgrave", "QL", "CQ", "CC", "DAF", "CN"),
  length = NULL,
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

An object of class `c("tc_estimates", "henderson")`. See
[`tc_estimates()`](https://aqlt.github.io/publishTC/reference/tc_estimates.md)
for a full description of the returned object.

## References

Henderson, R. (1916). Note on Graduation by Adjusted Average.
*Transactions of the Actuarial Society of America* 17: 43-48.

Musgrave, J. (1964). A Set of End Weights to End All End Weights. *US
Census Bureau \[Custodian\]*.
<https://www.census.gov/library/working-papers/1964/adrm/musgrave-01.html>.

Quartier-la-Tente, A. (2024). Improving Real-Time Trend Estimates Using
Local Parametrization of Polynomial Regression Filters. *Journal of
Official Statistics, 40*(4), 685-715.
[doi:10.1177/0282423X241283207](https://doi.org/10.1177/0282423X241283207)
.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
tc <- henderson_smoothing(x)
plot(tc, xlim = c(2022, 2025))
}
```
