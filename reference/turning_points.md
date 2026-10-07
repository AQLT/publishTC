# Detect Turning Points and Unwanted Ripples in a Time Series

Identifies turning points (upturns and downturns) and unwanted
short-term ripples in a time series object using local extremum
criteria.

## Usage

``` r
turning_points(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)

upturn(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)

downturn(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)

unwanted_ripples(x, start = NULL, end = NULL, digits = 6, k = 3, m = 1)
```

## Arguments

- x:

  An input time series (object of class `"ts"`).

- start, end:

  Numeric vectors specifying the start and end of the time interval
  (e.g., `c(2020, 1)`) in which to search for turning points.

- digits:

  Integer specifying the number of decimal digits used when comparing
  values to determine turning points. Defaults to `NULL` (no rounding).

- k, m:

  Integers specifying the required number of preceding (\\k\\) and
  succeeding (\\m\\) observations to define a local extremum. See
  details.

## Value

- `turning_points()` returns a named list with two components:
  `"upturn"` and `"downturn"`, each containing a vector of dates (or
  empty vectors if none found).

- `upturn()` and `downturn()` return a vector of dates corresponding to
  the detected upturns or downturns, respectively.

- `unwanted_ripples()` returns an integer representing the number of
  unwanted ripples detected in the series.

## Details

Turning points are identified following the operational definition
adapted from Zellner, Hong, and Min (1991), typically with \\k = 3\\ and
\\m = 1\\:

- An **upturn** occurs at time \\t\\ if: \$\$y\_{t-k} \ge \dots \ge
  y\_{t-1} \< y_t \le y\_{t+1} \le \dots \le y\_{t+m}\$\$

- A **downturn** occurs at time \\t\\ if: \$\$y\_{t-k} \le \dots \le
  y\_{t-1} \> y_t \ge y\_{t+1} \ge \dots \ge y\_{t+m}\$\$

An **unwanted ripple** is defined as any occurrence where two turning
points of the same type (two upturns or two downturns) occur within a
10-month window (i.e., short cycles lasting less than 11 months).

## References

Zellner, A., Hong, C., & Min, C. (1991). Forecasting Turning Points in
International Growth Rates using Bayesian Exponential Smoothing Methods.
*Journal of Econometrics*, 49(1–2), 275–304.
[doi:10.1016/0304-4076(91)90099-H](https://doi.org/10.1016/0304-4076%2891%2990099-H)

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tc <- henderson_smoothing(french_ipi[, "manufacturing"])
turning_points(tc)
unwanted_ripples(tc)
}
```
