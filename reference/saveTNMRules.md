# Save TNM staging rules to RDS file

The function reads the `.csv` files containing the TNM rules to derive
the summary stage.

## Usage

``` r
saveTNMRules(
  path = here::here("extras"),
  results_path = system.file("tnm_files", package = "oncomop")
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

The files that we refer to are:

- `tnm_concepts`: contains the concept ids of each TNM component
  differentiated by type and edition of the UICC classification system;

- `tnm_stage_mapping`: contains the complete rules to map all
  combinations of TNM components to a summary stage I-IV, differentiated
  by edition of staging system and by cancer type;

- `tnm_stage_shortcut_mapping`: contains more general rules that can be
  applied to derive the summary stage in a faster way based on a subset
  of TNM components.
