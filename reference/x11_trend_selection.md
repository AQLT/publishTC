# X-11 Selection of Trend-Cycle Filter

Performs X-11 selection for the length of the Henderson filter
(`x11_trend_selection()`) and computes the associated I/C ratio used to
build Musgrave filters (`find_icr()`).

## Usage

``` r
x11_trend_selection(x)

find_icr(length, freq = 12)
```

## Arguments

- x:

  A `"ts"` object representing the time series.

- length:

  Integer specifying the length of the filter.

- freq:

  Integer specifying the frequency of the time series used to compute
  the I/C ratio (e.g., 12 for monthly, 4 for quarterly).

## Value

- `x11_trend_selection()` returns a named numeric vector containing the
  selected length and the associated I/C ratio.

- `find_icr()` returns a single numeric value corresponding to the I/C
  ratio associated with the specified filter length and frequency in the
  X-11 algorithm.

## Details

The following procedure is used in X-11 to select the length of the
trend filter:

1.  Computes the I/C ratio, \\icr\\, with a Henderson filter of length
    equal to the series frequency plus 1.

2.  The selected length depends on the value of \\icr\\:

    - If \\icr \< 1\\, the selected length is 9 for monthly data and 5
      otherwise.

    - If \\1 \le icr \< 3.5\\, the selected length is \\freq + 1\\
      (i.e., 13 for monthly data, 5 for quarterly data).

    - If \\icr \ge 3.5\\, the selected length is 23 for monthly data and
      7 otherwise.

3.  The value of \\icr\\ is then mapped to build Musgrave filters
    (`find_icr()`):

    - For quarterly data, if the length is 5, then \\icr = 0.001\\;
      otherwise, \\icr = 4.5\\.

    - For other frequencies, if the length is less than or equal to 9,
      then \\icr = 1.0\\.

    - Else, if the length is less than or equal to 13, then \\icr =
      3.5\\.

    - Else, \\icr = 4.5\\.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
x11_trend_selection(cars_registrations)
find_icr(length = 13, freq = 12)
}
```
