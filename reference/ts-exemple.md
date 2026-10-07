# Example Datasets

Example datasets used in Quartier-la-Tente (2025).

## Usage

``` r
cars_registrations

french_ipi

fred

simulated_data

etip
```

## Format

An object of class `ts` of length 177.

An object of class `mts` (inherits from `ts`, `matrix`, `array`) with
416 rows and 3 columns.

An object of class `mts` (inherits from `ts`, `matrix`, `array`) with
766 rows and 2 columns.

An object of class `mts` (inherits from `ts`, `matrix`, `array`) with 84
rows and 6 columns.

An object of class `ts` of length 590.

## Details

- `cars_registrations`: Monthly new passenger car registrations in
  France, published in October 2024.

- `french_ipi`: Monthly Industrial Production Index (IPI) in France for
  crude petroleum, motor vehicles, and manufacturing, published in
  October 2024.

- `fred`: The series `CE16OV` (Civilian Employment Level) and `RETAILx`
  (Retail and Food Services Sales) from the FRED-MD database, published
  in November 2022.

- `simulated_data`: Simulated trends of degree 0, 1, and 2 with an
  Additive Outlier (AO) or Level Shift (LS) in January 2022.

- `etip`: Expected trend in production (balance of opinion) in the
  French manufacturing industry, published in May 2024 in the monthly
  business survey in goods-producing industries by INSEE.

## References

Quartier-la-Tente, A. (2025). Estimation de la tendance-cycle avec des
méthodes robustes aux points atypiques.
<https://github.com/AQLT/robustMA>. McCracken, Michael W., et Serena Ng.
2016. FRED-MD: A Monthly Database for Macroeconomic Research. Journal of
Business & Economic Statistics 34 (4): 574‑89.
<https://doi.org/10.1080/07350015.2015.1086655>.
