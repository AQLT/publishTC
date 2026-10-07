# Measure Smoothness of Trend-Cycle Estimates

Computes the smoothness ratio of trend-cycle estimates relative to the
seasonally adjusted series following Picard and Matthews (2016).

## Usage

``` r
smoothness(x)
```

## Arguments

- x:

  An object of class `"tc_estimates"`.

## Value

A numeric value representing the smoothness index of the trend-cycle
estimates.

## Details

Smoothness is evaluated as the ratio of month-to-month growth rate
variances: \$\$ 100 \times \sqrt{ \frac{ \sum_t \left( \frac{TC_t -
TC\_{t-1}}{TC\_{t-1}} \right)^2 }{ \sum_t \left( \frac{SA_t -
SA\_{t-1}}{SA\_{t-1}} \right)^2 } } \$\$

This metric quantifies the relative variability of the trend-cycle
compared to the seasonally adjusted series. A lower value indicates a
smoother trend-cycle.

## References

Picard, F., & Matthews, S. (2016). The Addition of Trend-Cycle Estimates
to Selected Publications at Statistics Canada. *Proceedings of the
Survey Methods Section, Statistical Society of Canada (SSC) Annual
Meeting*.
<https://ssc.ca/sites/default/files/imce/pdf/picard_ssc2016.pdf>

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- french_ipi[, "manufacturing"]
tc_clf <- clf_smoothing(x)
tc_henderson <- henderson_smoothing(x)
smoothness(tc_clf)
smoothness(tc_henderson)
}
```
