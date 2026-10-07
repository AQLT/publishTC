# Plot Confidence Intervals for Trend-Cycle Estimates

Base R (`confint_plot()`) and ggplot2 (`ggconfint_plot()`) graphics for
plotting trend-cycle estimates alongside their confidence intervals.

## Usage

``` r
confint_plot(
  object,
  xlim = NULL,
  ylim = NULL,
  col_tc = "#E69F00",
  col_sa = "black",
  col_confint = "grey",
  xlab = "",
  ylab = "",
  level = 0.95,
  ...
)

ggconfint_plot(
  object,
  xlim = NULL,
  ylim = NULL,
  col_tc = "#E69F00",
  col_sa = "black",
  col_confint = "grey",
  legend_tc = "Trend-cycle",
  legend_sa = "Seasonally adjusted",
  legend_confint = "Confidence interval",
  level = 0.95,
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

- col_confint:

  Color of the confidence interval lines or shaded region.

- xlab, ylab:

  Character strings for the x- and y-axis labels.

- level:

  The confidence level required (defaults to `0.95`).

- ...:

  Additional arguments (currently unused).

- legend_tc, legend_sa, legend_confint:

  Character strings specifying the legend labels for the trend-cycle,
  the seasonally adjusted series, and the confidence intervals,
  respectively.

## Value

- `confint_plot()` returns `NULL` invisibly and is called for its side
  effect (drawing a plot).

- `ggconfint_plot()` returns a
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object.

## Details

`confint_plot()` produces a base R graphic, whereas `ggconfint_plot()`
creates a ggplot2 object that can be further customized. The confidence
intervals are computed internally using
[confint()](https://aqlt.github.io/publishTC/reference/confint_tc.md).

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
confint_plot(tc_mod, xlim = c(2022, 2024.5))
}
```
