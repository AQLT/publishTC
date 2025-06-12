# publishTC 0.2.2

- New function `x13_regarima_outliers()` to detect AO and LS on a seasonally adjusted series.

- If `n_last_tc = NULL` then the value is defined according to the MCD statistic.

- `ggconfint_plot()` correction of parameters `xlim` and `ylim` which were not used. 

# publishTC 0.2.1

- Improvement of `ggplot2` plots.

# publishTC 0.2.0

- `mcd()` now returns by default the value of the number of period (instead of `NULL`)

- correction of local parameterisation functions when series contains NA.

- For the plots, by default only the 4 last values of the trend-cycle are dotted to emphasize the higher variability of the last values.

- New `growthplot()` function.


