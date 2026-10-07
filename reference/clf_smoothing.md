# Trend-Cycle Estimation using the Cascade Linear Filter

Estimates the trend-cycle component using the Cascade Linear Filter
(CLF) of length 13.

## Usage

``` r
clf_smoothing(x, endpoints = c("cut-and-normalize", "ALF"), ...)
```

## Arguments

- x:

  An input time series (object of class `"ts"` or `"mts"`).

- endpoints:

  Character string specifying the method used for asymmetric filters. If
  `"cut-and-normalize"` (the default), the cut-and-normalize method is
  used (cut the symmetric filter and normalize the coefficients, as done
  by Statistics Canada); otherwise, Asymmetric Linear Filters (ALF) are
  used.

- ...:

  Other unused parameters.

## Value

An object of class `c("tc_estimates", "clf")`. See
[`tc_estimates()`](https://aqlt.github.io/publishTC/reference/tc_estimates.md)
for a full description of the returned object.

## References

Dagum, E. B., & Luati, A. (2008). A Cascade Linear Filter to Reduce
Revisions and False Turning Points for Real Time Trend-Cycle Estimation.
*Econometric Reviews* 28(1-3), 40–59.
[doi:10.1080/07474930802387837](https://doi.org/10.1080/07474930802387837)

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tc <- clf_smoothing(cars_registrations)
plot(tc, xlim = c(2022, 2025))
}
```
