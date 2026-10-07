# Segmented Smoothing around Breakpoints

Estimates the trend-cycle component by segmenting a time series around
known structural breakpoints.

## Usage

``` r
segmented_smoothing(
  x,
  breaks = NULL,
  break_method = c("one-side", "two-sides"),
  smoothing_method = c("henderson_smoothing", "clf_smoothing"),
  ...
)
```

## Arguments

- x:

  An input time series (object of class `"ts"` or `"mts"`).

- breaks:

  A vector or list of breakpoints (dates) where the series should be
  split for smoothing.

- break_method:

  Character string specifying the method used to handle boundaries at
  breakpoints: `"one-side"` or `"two-sides"` (the default). See details.

- smoothing_method:

  Function specifying the smoothing method to apply to each segment.
  Must be
  [`henderson_smoothing()`](https://aqlt.github.io/publishTC/reference/henderson_smoothing.md)
  (the default) or
  [`clf_smoothing()`](https://aqlt.github.io/publishTC/reference/clf_smoothing.md).

- ...:

  Additional arguments passed to `smoothing_method`.

## Value

An object of class `c("tc_estimates", "henderson")` if
`smoothing_method` is
[`henderson_smoothing()`](https://aqlt.github.io/publishTC/reference/henderson_smoothing.md),
or `c("tc_estimates", "clf")` if `smoothing_method` is
[`clf_smoothing()`](https://aqlt.github.io/publishTC/reference/clf_smoothing.md).

## Details

Two methods are available to estimate the trend-cycle around
breakpoints:

1.  `"one-side"`: The trend-cycle is estimated across the entire
    dataset, and the estimates after each breakpoint are overwritten
    with the estimates obtained by smoothing only the data following
    that breakpoint.

2.  `"two-sides"`: The trend-cycle is estimated independently on each
    segment defined by the breakpoints, and the resulting estimates are
    combined to form the final trend-cycle.

The `parameters` field of the returned object corresponds to the
parameters of the last segment (used to compute confidence intervals and
implicit forecasts).

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- window(publishTC::french_ipi[, "manufacturing"], start = 2015)
tc_h <- henderson_smoothing(x, length = 13)$tc
breaks <- list(c(2020, 3))
tc_h_2s <- segmented_smoothing(x, breaks = breaks, break_method = "two-sides", length = 13)$tc
tc_h_rob <- henderson_robust_smoothing(x, ls = c(2020 + (3 - 1) / 12, 2020 + (4 - 1) / 12))$tc

plot(window(x, start = 2019.5, end = 2021),
  main = "Smoothing around COVID-19",
  xlab = NULL, ylab = NULL,
  lty = 3
)
lines(tc_h, col = "purple")
lines(tc_h_2s, col = "lightblue")
lines(tc_h_rob, col = "orange")
legend(
  "bottomright",
  legend = c("y", "Henderson", "Two-Sides Segmented", "Robust H. (2 LS)"),
  col = c("black", "purple", "lightblue", "orange"),
  lty = c(3, 1, 1, 1, 1),
  cex = 0.7
)
}
```
