# publishTC 0.2.3

- Handled filtering case when the series is constant.
- Added `split_smoothing()` to smooth series while taking structural breaks into account.
- Fixed legend colors in `ggplot2` plots when item names are modified.
- Added `underlying_forecasts()`, `underlying_forecasts_plot()`, and `ggunderlying_forecasts_plot()`.
- Added `lines.tc_estimates()` S3 method.
- Reviewed and refined documentation.

# publishTC 0.2.2

- Added `x13_regarima_outliers()` to detect AO and LS on seasonally adjusted series.
- Added `smoothness()` to compute a smoothness statistic.
- If `n_last_tc = NULL`, the value is now automatically set based on the MCD statistic.
- Fixed `xlim` and `ylim` parameters in `ggconfint_plot()`, which were previously ignored.
- Added `sa_bar_line` parameter to `growthplot()`.

# publishTC 0.2.1

- Improved `ggplot2` plots layout and customization options.

# publishTC 0.2.0

- `mcd()` now returns the number of periods by default (instead of `NULL`).
- Fixed local parameterization functions when the series contains `NA` values.
- In plots, only the last 4 values of the trend-cycle are now dotted by default to emphasize higher variability at the end of the series.
- Added `growthplot()` function.
