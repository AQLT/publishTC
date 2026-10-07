# Confidence Intervals for Trend-Cycle Estimates

Computes confidence intervals for trend-cycle estimates of class
`"tc_estimates"`.

## Usage

``` r
# S3 method for class 'henderson'
confint(object, parm, level = 0.95, asymmetric_var = TRUE, ...)

# S3 method for class 'clf'
confint(object, parm, level = 0.95, asymmetric_var = TRUE, ...)

# S3 method for class 'robust_henderson'
confint(object, parm, level = 0.95, asymmetric_var = TRUE, ...)
```

## Arguments

- object:

  An object of class `"tc_estimates"`.

- parm:

  Unused parameter, kept for compatibility with the generic function.

- level:

  The confidence level required (defaults to `0.95`).

- asymmetric_var:

  Logical. If `TRUE` (the default), the variance is estimated for each
  asymmetric filter. If `FALSE`, the variance associated with the
  symmetric filter is used throughout.

- ...:

  Additional arguments (currently unused).

## Value

A `matrix` or `mts` object with the filtered series and the lower and
upper bounds of the confidence interval.

## Details

See
[`rjd3filters::confint_filter()`](https://rjdverse.github.io/rjd3filters/reference/confint_filter.html)
for details on the computation of confidence intervals.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
tc <- henderson_smoothing(x)
confint <- confint(tc)

plot(confint, plot.type = "single",
   col = c("red", "black", "black"),
   lty = c(1, 2, 2), xlab = NULL, ylab = NULL)
lines(x, col = "grey")
legend("topleft", legend = c("x", "Smoothed", "CI (95%)"),
     col= c("grey", "red", "black"), lty = c(1, 1, 2))
}
```
