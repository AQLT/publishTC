# Plot Underlying Forecasts for Trend-Cycle Estimates

Base R (`underlying_forecasts_plot()`) and ggplot2
(`ggunderlying_forecasts_plot()`) graphics for plotting trend-cycle
estimates alongside their underlying forecasts.

## Usage

``` r
underlying_forecasts_plot(
  object,
  xlim = NULL,
  ylim = NULL,
  col_tc = "#E69F00",
  col_sa = "black",
  col_i_f = col_sa,
  xlab = "",
  ylab = "",
  lty_last_tc = 2,
  lty_i_f = 3,
  n_last_tc = 4,
  ...
)

ggunderlying_forecasts_plot(
  object,
  xlim = NULL,
  ylim = NULL,
  col_tc = "#E69F00",
  col_sa = "black",
  col_i_f = col_sa,
  lty_last_tc = 2,
  lty_i_f = 3,
  n_last_tc = 4,
  legend_tc = "Trend-cycle",
  legend_sa = "Seasonally adjusted",
  legend_i_f = "Implicit forecasts",
  ...
)
```

## Arguments

- object:

  An object of class `"tc_estimates"`.

- xlim, ylim:

  Limits for the x- and y-axes. If `xlim` is specified and `ylim` is
  `NULL`, `ylim` is determined automatically based on the truncated
  series.

- col_sa, col_tc:

  Colors used for the seasonally adjusted and trend-cycle components,
  respectively.

- col_i_f:

  Color of the forecasts lines.

- xlab, ylab:

  Character strings for the x- and y-axis labels.

- lty_last_tc, lty_i_f:

  Line types used for the last values of the trend-cycle component and
  for the forecasts, respectively.

- n_last_tc:

  Number of final values of the trend-cycle component to plot with a
  distinct line type (`lty_last_tc`), emphasizing higher uncertainty in
  recent estimates. If `NULL` (the default), `n_last_tc` is set to the
  Month of Cyclical Dominance (MCD) statistic.

- ...:

  Additional graphical parameters passed to internal plotting functions.

- legend_tc, legend_sa, legend_i_f:

  Character strings specifying the legend labels for the trend-cycle,
  the seasonally adjusted series, and the forecasts, respectively.

## Value

- `underlying_forecasts_plot()` returns `NULL` invisibly and is called
  for its side effect (drawing a plot).

- `ggunderlying_forecasts_plot()` returns a
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object.

## Details

`underlying_forecasts_plot()` produces a base R graphic, whereas
`ggunderlying_forecasts_plot()` creates a ggplot2 object that can be
further customized.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
underlying_forecasts_plot(tc_mod, xlim = c(2022, 2025))
}
```
