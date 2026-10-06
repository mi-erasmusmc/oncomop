# Assert that cohort names match selected cancer site(s)

Validates that all names in a cohort table correspond to the selected
cancer site(s). Cohort names must follow the `"<cancer>_cancer"` format.

## Usage

``` r
assertCancerCohortName(cohort, cancer = "breast")
```

## Arguments

- cohort:

  A cohort table from a cdm reference object.

- cancer:

  A character vector of supported cancer sites. Defaults to `"breast"`.

## Value

`NULL`, invisibly, if validation succeeds.

## Details

Throws an error of class `"Invalid cohort names"` if any cohort name
does not correspond to the selected cancer site(s).
