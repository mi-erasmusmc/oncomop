# Create cancer cohorts based on phenotype specifications

The function `createCancerCohorts` relies on the `CohortConstructor`
package to generate cohorts based on one or more concept sets and
additional phenotype specifications. In this study there are seven
cancer types of interest: bladder cancer (1937), breast cancer (4415),
colorectal cancer (1444), esophageal cancer (1414), lung cancer (1413),
prostate cancer (1489) and skin melanoma (1451). To each cancer type are
applied the following phenotype specifications:

- Study period: from 01-01-2010 to the end of observation period.

- Inclusion criteria:

  1.  The date of the first diagnosis of cancer must fall within the
      study period.

  2.  The patient's age must be \\\geq 18\\ at the time of cancer
      diagnosis.

- Exclusion criteria:

  1.  Exclude patients with death before and at index date.

  2.  Exclude males from patients with breast cancer.

  3.  Exclude females from patients with prostate cancer.

## Usage

``` r
createCancerCohorts(cdm, path = "cancer_cohorts", name, cancer = "all")
```

## Arguments

- cdm:

  A cdm instance.

- path:

  The character name of the folder where the concept sets are saved
  within the study package, default `"concept_sets"`.

- name:

  The character name of the new cohort table to be created.

- cancer:

  A character vector of cancer types to include in the cohort, default
  `"all"`. Other possible values are `"bladder_cancer"`,
  `"breast_cancer"`, `"colorectal_cancer"`, `"lung_cancer"`,
  `"melanoma_of_skin"`, `"oesophageal_cancer"`, `"prostate_cancer"`.

## Value

A cdm instance including with the newly created cohort table.
