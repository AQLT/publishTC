# Multi-Method Trend-Cycle Estimation

Computes trend-cycle estimates using multiple smoothing methods
simultaneously and returns a list of `"tc_estimates"` objects.

## Usage

``` r
smoothing(
  x,
  methods = c("henderson", "henderson_localic", "henderson_robust",
    "henderson_robust_localic", "clf_cn", "clf_alf"),
  endpoints = "Musgrave",
  length = NULL,
  icr = NULL,
  asymmetric_var = FALSE,
  degree = 3,
  ao = NULL,
  ao_tc = NULL,
  ls = NULL,
  ...
)
```

## Arguments

- x:

  An input time series (object of class `"ts"` or `"mts"`).

- methods:

  A list of character strings specifying the smoothing methods to apply.
  Defaults to `c("henderson", "henderson_localic", "clf")`.

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

- asymmetric_var:

  Logical. When `local_icr = TRUE`, if `TRUE`, the variance is estimated
  for each asymmetric filter. If `FALSE` (the default), the variance
  associated with symmetric estimates is used throughout.

- degree:

  Integer. When `local_icr = TRUE`, degree of the polynomial used to
  estimate the local bias parameter.

- ao:

  Vector of dates for Additive Outliers (AO) whose effects are allocated
  to the irregular component.

- ao_tc:

  Vector of dates for Additive Outliers (AO) whose effects are allocated
  to the trend-cycle component.

- ls:

  Vector of dates for Level Shifts (LS) whose effects are allocated to
  the trend-cycle component.

- ...:

  Additional parameters passed to the underlying smoothing functions.

## Value

A named list of objects of class `"tc_estimates"`, where each element
corresponds to one of the specified methods in `methods`.

## Details

This convenience function applies multiple trend-cycle estimation
algorithms to the same input series, facilitating direct comparison
between different filtering approaches (e.g., standard Henderson,
Henderson with local I/C ratio, or CLF smoothing).

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- french_ipi[, "manufacturing"]
outliers <- x13_regarima_outliers(x)
all_methods <- smoothing(x, ao = outliers$ao, ao_tc = outliers$ao_tc, ls = outliers$ls)

plot(all_methods$henderson, xlim = c(2023, 2025))
lines(all_methods$henderson_localic, col_tc = "blue")
}
```
