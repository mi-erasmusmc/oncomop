# Extract a UICC stage rule

Retrieves the UICC stage associated with a TNM combination for breast
cancer using the eighth-edition base stage mapping.

## Usage

``` r
extractStageRule(.T, .N, .M)
```

## Arguments

- .T:

  A character TNM T-category value.

- .N:

  A character TNM N-category value.

- .M:

  A character TNM M-category value.

## Value

A character vector containing the matching UICC stage, if any.
