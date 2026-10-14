# Save TNM staging rules to an RDS file

The function reads the `.csv` files containing the TNM rules to derive
the summary stage.

## Usage

``` r
saveStagesRules(
  path = system.file("extdata", package = "oncomop"),
  results_path = tempdir()
)
```

## Arguments

- path:

  Character directory containing the packaged source `.csv` files.

- results_path:

  Existing writable directory where the RDS files are saved.

## Value

`NULL`, called for its side effects.

## Details

The files that we refer to are:

- `tnm_concepts`: contains the concept ids of each TNM component
  differentiated by type and edition of the UICC classification system;

- `tnm_mapping`: contains the complete rules to map all combinations of
  TNM components to a summary stage I-IV, differentiated by edition of
  staging system and by cancer type.
