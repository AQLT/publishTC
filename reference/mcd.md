# Month of Cyclical Dominance (MCD)

Computes the Month of Cyclical Dominance (MCD) for a time series
decomposition.

## Usage

``` r
mcd(x, tc, mul = FALSE)
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

An integer specifying the Month of Cyclical Dominance (MCD).

## Details

The Month of Cyclical Dominance (MCD) represents the minimum number of
months required for the average absolute variation of the trend-cycle
component to dominate that of the irregular component. In other words,
it indicates the shortest time span needed on average for the cyclical
signal to outweigh irregular fluctuations.

It is computed as the smallest integer \\k\\ (where \\k \ge 1\\) such
that the I/C ratio remains less than or equal to 1 for all lags \\j \ge
k\\: \$\$ k = \min \left\\ k \in \\1, \dots, p\\ :
\frac{\bar{I}\_j}{\bar{C}\_j} \le 1 \text{ for all } j \ge k \right\\
\$\$ If the I/C ratio never falls below or equal to 1 within the
frequency \\p\\, the MCD is set to \\p\\.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
tc <- henderson_smoothing(x)
mcd(tc)
icrs(tc)
}
```
