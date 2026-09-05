# Creates cancer-related characteristics cohorts

The function `createCharacteristicsCohorts` relies on the
`CohortConstructor` package to generate cohorts based on one or more
concept sets and additional phenotype specifications. In this study the
cancer-related characteristics that will be analysed are:
cancer-related, characteristics (i.e. cancer stage and grade,
cancer-specific biomarkers, performance status, cancer treatments, and
laboratory tests and procedures)

## Usage

``` r
createCharacteristicsCohorts(
  cdm,
  path = "concept_sets",
  name = "cancer_characteristics",
  characteristics = "all"
)
```

## Arguments

- cdm:

  A cdm instance.

- path:

  The character name of the folder where the concept sets are saved
  within the study package, default `"concept_sets"`.

- name:

  The character name of the new cohort table to be created.

- characteristics:

  A character vector of cancer-related characteristics to include in the
  cohort, default `"all"`. Other possible values are "biomarkers",
  "cancer_cohorts_deck", "cancer_progression", "grade", "radiotherapy",
  "stage", "surgery" and "treatments_procedures"

## Value

A cdm instance including with the newly created cohort table.
