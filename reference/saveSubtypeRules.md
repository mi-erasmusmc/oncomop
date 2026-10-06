# `saveSubtypeRules()` to an RDS file

The function reads the `.csv` files containing the subtype rules to
derive the summary stage.

## Usage

``` r
saveSubtypeRules(
  path = here::here("extras"),
  results_path = system.file("extdata", package = "oncomop")
)
```

## Arguments

- path:

  Character directory where the original .csv files are stored.

- results_path:

  Character directory where the RDS files are to be saved.

## Value

`NULL`, called for its side effects.

## Details

The files are:

- `subtype_mapping`: contains the complete rules to map breast cancer
  subtypes
