# Classical Moving Averages

Datasets containing coefficients of classical moving averages used for
trend-cycle extraction.

## Usage

``` r
henderson

CLF

CLF_CN

local_param_est
```

## Format

`henderson` is a list of objects of class `"moving_average"`.

`CLF` is an object of class `"finite_filters"`.

`CLF_CN` is an object of class `"finite_filters"`.

`local_param_est` is a nested list of objects of class
`"finite_filters"`. The first level corresponds to the length of the
filter; the second level corresponds to the degree of the local
polynomial model used for the trend-cycle; and the third level
corresponds to the target component (1 for bias, 2 for slope, and 3 for
curvature).

## Details

- `henderson`: Henderson moving averages of lengths 5, 7, 9, 13, and 23.

- `CLF`: Cascade Linear Filter (CLF) of length 13 and associated
  Asymmetric Linear Filters (ALF).

- `CLF_CN`: Cascade Linear Filter (CLF) of length 13 and associated
  cut-and-normalize asymmetric filters.

- `local_param_est`: Moving averages used to locally estimate the bias,
  slope, or curvature associated with trend-cycle estimation (via local
  polynomial approximation).

## References

Dagum, E. B., & Luati, A. (2008). A Cascade Linear Filter to Reduce
Revisions and False Turning Points for Real Time Trend-Cycle Estimation.
*Econometric Reviews*, 28(1-3), 40–59.
[doi:10.1080/07474930802387837](https://doi.org/10.1080/07474930802387837)

Henderson, R. (1916). Note on graduation by adjusted average.
*Transactions of the Actuarial Society of America*, 17, 43–48.

Musgrave, J. (1964). A Set of End Weights to End All End Weights. *US
Census Bureau \[Custodian\]*.
<https://www.census.gov/library/working-papers/1964/adrm/musgrave-01.html>

Quartier-la-Tente, A. (2024). Improving Real-Time Trend Estimates Using
Local Parametrization of Polynomial Regression Filters. *Journal of
Official Statistics*, 40(4), 685–715.
[doi:10.1177/0282423X241283207](https://doi.org/10.1177/0282423X241283207)
