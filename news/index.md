# Changelog

## publishTC 0.2.3

- Handled filtering case when the series is constant.
- Added `split_smoothing()` to smooth series while taking structural
  breaks into account.
- Fixed legend colors in `ggplot2` plots when item names are modified.
- Added
  [`underlying_forecasts()`](https://rjdverse.github.io/rjd3filters/reference/underlying_forecasts.html),
  [`underlying_forecasts_plot()`](https://aqlt.github.io/publishTC/reference/underlying_forecasts_plot.md),
  and
  [`ggunderlying_forecasts_plot()`](https://aqlt.github.io/publishTC/reference/underlying_forecasts_plot.md).
- Added
  [`lines.tc_estimates()`](https://aqlt.github.io/publishTC/reference/plot.tc_estimates.md)
  S3 method.
- Reviewed and refined documentation.

## publishTC 0.2.2

- Added
  [`x13_regarima_outliers()`](https://aqlt.github.io/publishTC/reference/x13_regarima_outliers.md)
  to detect AO and LS on seasonally adjusted series.
- Added
  [`smoothness()`](https://aqlt.github.io/publishTC/reference/smoothness.md)
  to compute a smoothness statistic.
- If `n_last_tc = NULL`, the value is now automatically set based on the
  MCD statistic.
- Fixed `xlim` and `ylim` parameters in
  [`ggconfint_plot()`](https://aqlt.github.io/publishTC/reference/confint_plot.md),
  which were previously ignored.
- Added `sa_bar_line` parameter to
  [`growthplot()`](https://aqlt.github.io/publishTC/reference/growthplot.md).

## publishTC 0.2.1

- Improved `ggplot2` plots layout and customization options.

## publishTC 0.2.0

- [`mcd()`](https://aqlt.github.io/publishTC/reference/mcd.md) now
  returns the number of periods by default (instead of `NULL`).
- Fixed local parameterization functions when the series contains `NA`
  values.
- In plots, only the last 4 values of the trend-cycle are now dotted by
  default to emphasize higher variability at the end of the series.
- Added
  [`growthplot()`](https://aqlt.github.io/publishTC/reference/growthplot.md)
  function.
