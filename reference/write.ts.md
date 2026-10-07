# Export and Import Time Series to and from CSV

`write.ts()` exports a time series object (`"ts"`) or a list of time
series to a CSV file, adding a `"time"` column. `read.ts()` imports a
CSV file whose first column contains dates and reconstructs the
corresponding `ts` object.

## Usage

``` r
write.ts(x, file, ...)

read.ts(file, frequency = NULL, list = FALSE)
```

## Arguments

- x:

  A time series object (class `"ts"`) or a list of `"ts"` objects to
  export.

- file:

  A character string specifying the path to the CSV file to write or
  read.

- ...:

  Additional arguments passed to
  [`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html) or
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html).

- frequency:

  An integer specifying the number of observations per unit of time
  (e.g., 12 for monthly, 4 for quarterly). If `NULL` (the default), it
  is automatically inferred from the date column.

- list:

  Logical. If `TRUE`, `read.ts()` returns a list of time series objects;
  otherwise, it returns a single `"ts"` or `"mts"` object.

## Value

- `write.ts()` returns `NULL` invisibly and is called for its side
  effect (writing a CSV file).

- `read.ts()` returns a `"ts"` object, a multiple time series (`"mts"`),
  or a `list` of `"ts"` objects depending on `list`.

## Details

`write.ts()` formats the time index into a dedicated date column (e.g.,
`YYYY-MM-DD` or fractional years) alongside the data series. When
reading back with `read.ts()`, the time index is parsed to reconstruct
regular R time series objects.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
file <- tempfile(fileext = ".csv")
write.ts(AirPassengers, file)
read.ts(file)
}
```
