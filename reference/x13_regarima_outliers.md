# Outlier Detection with RegARIMA Model

Wrapper around
[`rjd3x13::regarima_outliers()`](https://rjdverse.github.io/rjd3x13/reference/regarima_outliers.html)
to detect Additive Outliers (AO) or Level Shifts (LS) in a seasonally
adjusted series.

## Usage

``` r
x13_regarima_outliers(
  y,
  order = c(0, 1, 1),
  mean = FALSE,
  ao = TRUE,
  ls = TRUE
)
```

## Arguments

- y:

  A time series object (class `"ts"`).

- order:

  An integer vector of length 3 specifying the non-seasonal ARIMA orders
  \\(p, d, q)\\.

- mean:

  Logical. If `TRUE`, the model includes a constant term.

- ao, ls:

  Logical flags indicating whether Additive Outliers (AO) and Level
  Shifts (LS) should be detected, respectively.

## Value

A list with two components:

- `ao`: A vector of dates where Additive Outliers were detected (or
  `NULL` if none).

- `ls`: A vector of dates where Level Shifts were detected (or `NULL` if
  none).

## Details

This function uses an underlying RegARIMA model via rjd3x13 to
automatically identify structural breaks and temporary shocks in the
input series, returning their locations for downstream robust
trend-cycle estimation.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x13_regarima_outliers(cars_registrations)
}
```
