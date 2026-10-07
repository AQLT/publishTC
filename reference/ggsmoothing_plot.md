# Plot Multiple Trend-Cycle Visualizations

Generates a list of ggplot2 graphics displaying various diagnostic and
comparative plots for a `"tc_estimates"` object.

## Usage

``` r
ggsmoothing_plot(
  object,
  plots = c("normal", "confint", "lollypop", "implicit_forecasts",
    "underlying_forecasts"),
  level = 0.95,
  ...
)
```

## Arguments

- object:

  An object of class `"tc_estimates"`.

- plots:

  A character vector specifying the types of plots to generate.
  Available options are `"normal"`, `"confint"`, `"lollypop"`,
  `"implicit_forecasts"`, and `"underlying_forecasts"`. Defaults to
  generating all available plots.

- level:

  The confidence level required (defaults to `0.95`).

- ...:

  Additional graphical arguments passed to internal plotting functions.

## Value

A list of
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) objects
corresponding to the requested `plots`.

## Details

The following plot types can be produced:

- `"normal"`: Standard plot showing trend-cycle and seasonally adjusted
  series via
  [`autoplot.tc_estimates()`](https://aqlt.github.io/publishTC/reference/plot.tc_estimates.md).

- `"confint"`: Confidence interval plot via
  [`ggconfint_plot()`](https://aqlt.github.io/publishTC/reference/confint_plot.md).

- `"lollypop"`: Lollipop chart comparing estimates via
  [`gglollypop()`](https://aqlt.github.io/publishTC/reference/lollypop.md).

- `"implicit_forecasts"`: Implicit forecasts plot via
  [`ggimplicit_forecasts_plot()`](https://aqlt.github.io/publishTC/reference/implicit_forecasts_plot.md).

- `"underlying_forecasts"`: Underlying forecasts plot via
  [`ggunderlying_forecasts_plot()`](https://aqlt.github.io/publishTC/reference/underlying_forecasts_plot.md).

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x <- cars_registrations
tc <- henderson_smoothing(x)
ggsmoothing_plot(tc)
}
```
