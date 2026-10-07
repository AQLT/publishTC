# Compute Irregular-to-Trend-Cycle (I/C) Ratios

`icr()` computes the overall I/C ratio, while `icrs()` computes the I/C
ratios for each lag up to the series frequency.

## Usage

``` r
icr(x, tc, mul = FALSE)

icrs(x, tc, mul = FALSE)
```

## Arguments

- x:

  A seasonally adjusted time series or an object of class
  `"tc_estimates"`.

- tc:

  A time series representing the trend-cycle component. Ignored if `x`
  is an object of class `"tc_estimates"`.

- mul:

  Logical indicating whether the decomposition is multiplicative
  (`TRUE`) or additive (`FALSE`, the default).

## Value

`icr()` returns a single numeric value representing the overall I/C
ratio, while `icrs()` returns a named numeric vector of I/C ratios for
lags \\1\\ to \\p\\.

## Details

The I/C ratio measures the relative importance of the irregular
component compared to the trend-cycle component in a time series
decomposition.

For a time series of frequency \\p\\, when the decomposition is additive
(`mul = FALSE`), the irregular component is defined as \\I_t = X_t -
TC_t\\. `icrs()` returns a vector containing the ratio of the mean
absolute variation of the irregular component to that of the trend-cycle
component for each lag \\k\\: \$\$ \frac{\bar{I}\_k}{\bar{C}\_k} =
\frac{\sum \|I_t - I\_{t-k}\|}{\sum \|TC_t - TC\_{t-k}\|} \quad
\text{for } k \in \\1, 2, \dots, p\\. \$\$ If the decomposition is
multiplicative (`mul = TRUE`), the irregular component is defined as
\\I_t = X_t / TC_t\\, and the ratio of relative variations is computed
as: \$\$ \frac{\bar{I}\_k}{\bar{C}\_k} = \frac{\sum \|(I_t - I\_{t-k}) /
I\_{t-k}\|}{\sum \|(TC_t - TC\_{t-k}) / TC\_{t-k}\|} \quad \text{for } k
\in \\1, 2, \dots, p\\. \$\$ `icr()` returns the overall I/C ratio,
which corresponds to the first lag ratio \\\bar{I}\_1 / \bar{C}\_1\\.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
tc <- henderson_smoothing(x)
icr(tc)
icrs(tc)
}
```
