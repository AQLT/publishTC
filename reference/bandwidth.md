# Get Filter Bandwidth

Retrieves the bandwidth of the filter used in a `"tc_estimates"` object.

## Usage

``` r
bandwidth(x)
```

## Arguments

- x:

  An object of class `"tc_estimates"`.

## Value

An `integer` representing the bandwidth of the trend-cycle filter.

## Details

The bandwidth corresponds to the number of observations on each side of
the central value for a symmetric trend-cycle filter. The total filter
length is equal to \\2 \times \text{bandwidth}(x) + 1\\.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- french_ipi[, "manufacturing"]
tc_clf <- henderson_smoothing(x)
bandwidth(tc_clf)
}
```
