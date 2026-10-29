# Apply CAR filtering for DK-DHR

Utility function to use in oncology studies involving the Danish Data
Partner DK-DHR. The function applies a filter on the
`condition_occurrence` table of the CDM to select only records validated
by the Danish Cancer Registry (CAR).

## Usage

``` r
applyCARFiltering(cdm, dbname, objective = "other")
```

## Arguments

- cdm:

  A cdm instance.

- dbname:

  A character string of the database name (must be "DK-DHR").

- objective:

  A character string specifying the study objective, and consequently
  the type of filtering to apply. Default `"other"`. Use the default if
  no further specifications were given.

## Value

The cdm instance, with filtered condition occurrence records.
