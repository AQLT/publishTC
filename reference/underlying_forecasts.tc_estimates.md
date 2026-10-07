# Underlying Forecasts Methods

S3 method of
[`rjd3filters::underlying_forecasts()`](https://rjdverse.github.io/rjd3filters/reference/underlying_forecasts.html)
for `"tc_estimates"` objects.

## Usage

``` r
# S3 method for class 'henderson'
underlying_forecasts(x, ...)

# S3 method for class 'clf'
underlying_forecasts(x, ...)

# S3 method for class 'robust_henderson'
underlying_forecasts(x, ...)
```

## Arguments

- x:

  An object of class `"tc_estimates"`.

- ...:

  Additional arguments passed to internal methods (currently unused).

## Value

A `matrix` or `mts` object containing the underlying forecasts for the
series.

## Details

See
[`rjd3filters::underlying_forecasts()`](https://rjdverse.github.io/rjd3filters/reference/underlying_forecasts.html)
for details on the computation of underlying forecasts.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
underlying_forecasts(tc_mod)
}
```
