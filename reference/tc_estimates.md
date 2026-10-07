# Trend-Cycle Estimates Class

Constructor and S3 methods for objects of class `"tc_estimates"`.

## Usage

``` r
tc_estimates(tc, sa, parameters = NULL, extra_class = NULL, ...)

is_tc_estimates(x)

# S3 method for class 'tc_estimates'
summary(object, ...)
```

## Arguments

- tc:

  Time series object (`"ts"`) representing the estimated trend-cycle
  component.

- sa:

  Time series object (`"ts"`) representing the original or seasonally
  adjusted series.

- parameters:

  A list of parameters used to compute the trend-cycle estimates (e.g.,
  filter coefficients, variance, or degree of polynomial).

- extra_class:

  Character string specifying an additional class to append to the
  returned object.

- ...:

  Additional arguments passed to or from other methods (currently
  unused).

- x:

  An object of class `"tc_estimates"`.

- object:

  An object of class `"tc_estimates"`.

## Value

`tc_estimates()` returns an object of class
`c("tc_estimates", extra_class)`, which is a list containing the
following components:

- `tc`: The estimated trend-cycle time series.

- `x`: The original (seasonally adjusted) time series.

- `parameters`: A list of parameters used during estimation.

The [`summary()`](https://rdrr.io/r/base/summary.html) method returns a
list of class `"summary.tc_estimates"` containing:

- `I/C ratio`: The overall Irregular-to-Trend-Cycle ratio (see
  [`icr()`](https://aqlt.github.io/publishTC/reference/icr.md)).

- `I/C ratios`: The I/C ratio per period (see
  [`icrs()`](https://aqlt.github.io/publishTC/reference/icr.md)).

- `MCD`: The Month of Cyclical Dominance statistic (see
  [`mcd()`](https://aqlt.github.io/publishTC/reference/mcd.md)).

- `Length`: The length of the symmetric trend-cycle filter.

## Details

Objects of class `"tc_estimates"` store trend-cycle estimates along with
the original series and metadata required for summary statistics,
confidence intervals, and implicit forecasts.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
tc <- henderson_smoothing(x)
summary(tc)
print(tc)
}
```
