# Plotting Methods for Trend-Cycle Estimates

Plotting methods for objects of class `"tc_estimates"`.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`lines()`](https://rdrr.io/r/graphics/lines.html) produce base R
graphics, whereas
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
creates a ggplot2 object.

## Usage

``` r
# S3 method for class 'tc_estimates'
plot(
  x,
  y = NULL,
  xlim = NULL,
  ylim = NULL,
  col_tc = "#E69F00",
  col_sa = "black",
  xlab = "",
  ylab = "",
  lty_last_tc = 2,
  n_last_tc = 4,
  ...
)

# S3 method for class 'tc_estimates'
lines(x, col_tc = "#E69F00", lty_last_tc = 2, n_last_tc = 4, ...)

# S3 method for class 'tc_estimates'
autoplot(
  object,
  xlim = NULL,
  ylim = NULL,
  col_tc = "#E69F00",
  col_sa = "black",
  legend_tc = "Trend-cycle",
  legend_sa = "Seasonally adjusted",
  lty_last_tc = 2,
  n_last_tc = 4,
  ...
)

# S3 method for class 'ts'
autoplot(object, xlim = NULL, ylim = NULL, ...)
```

## Arguments

- x:

  An object of class `"tc_estimates"`.

- y:

  Unused parameter, kept for compatibility with the generic function.

- xlim, ylim:

  Limits for the x- and y-axes. If `xlim` is specified and `ylim` is
  `NULL`, `ylim` is determined automatically based on the truncated
  series.

- col_sa, col_tc:

  Colors used for the seasonally adjusted and trend-cycle components,
  respectively.

- xlab, ylab:

  Character strings for the x- and y-axis labels.

- lty_last_tc:

  Line type for the last values of the trend-cycle component.

- n_last_tc:

  Number of final values of the trend-cycle component to plot with a
  distinct line type (`lty_last_tc`), emphasizing higher uncertainty in
  recent estimates. If `NULL` (the default), `n_last_tc` is set to the
  Month of Cyclical Dominance (MCD) statistic.

- ...:

  Additional graphical parameters passed to internal plotting functions.

- object:

  An object of class `"tc_estimates"`.

- legend_tc, legend_sa:

  Character strings specifying the legend labels for the trend-cycle and
  seasonally adjusted components, respectively.

## Value

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`lines()`](https://rdrr.io/r/graphics/lines.html) return `NULL`
  invisibly and are called for their side effect (drawing a plot).

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  returns a
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object.

## Details

`plot.tc_estimates()` and `autoplot.tc_estimates()` produce a plot
showing both the seasonally adjusted series and the estimated
trend-cycle component. `lines.tc_estimates()` adds only the trend-cycle
component to an existing base R plot.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tc_mod <- henderson_smoothing(french_ipi[, "manufacturing"])
plot(tc_mod, xlim = c(2022, 2024.5))
}
```
