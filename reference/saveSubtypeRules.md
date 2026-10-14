# `saveSubtypeRules()` to an RDS file

The function reads the `.csv` files containing the subtype rules to
derive the summary stage.

## Usage

``` r
saveSubtypeRules(
  path = system.file("extdata", package = "oncomop"),
  results_path = tempdir()
)
```

## Arguments

- path:

  Character directory containing the packaged source `.csv` file.

- results_path:

  Existing writable directory where the RDS file is saved.

## Value

`NULL`, called for its side effects.

## Details

The files are:

- `subtype_mapping`: contains the complete rules to map breast cancer
  subtypes
