
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oncomop

<!-- badges: start -->

<!-- badges: end -->

A tool to determine stages and subtypes in cancer cohorts using UICC
guidelines. The aim is facilite the characterisation of patients using
OMOP analytical tools that rely on a ‘dplyr’ pipe-based workflow such as
CohortConstructor, PatientProfiles and CohortCharacteristics.

To start install the package from CRAN or remotes:

``` r

install.packages("oncomop")

remotes::install_github("ohdsi/oncomop")
```

## Adding a stage or subtype

A cohort should be created in a CDM reference using tools such
CohortConstructor and CDMConnector. From there, oncomop can be used in a
pipe to add a column with the cancer stage or subtype for each subject
record.

``` r

CohortConstructor::conceptCohort(
  cdm,
  conceptSet = 4308306,
  name = "breast_cancer"
)

cdm$breast_cancer |> 
  oncomop::addStages(
    cdm,
    cancer,
    window,
    edition,
    type
    )

cdm$breast_cancer |> 
  oncomop::addSubtypes(
    cdm,
    cancer,
    window
    )
```

## The rules

Oncomop includes a set of predefined rules based on UICC guidelines to
analyze eight different cancer types. In this article you can find a
more detailed description or simply call `oncomop::ruleset()` to see the
rules data.

``` r

omcomop::ruleset(
  cancer = "breast",
  edition = "9th",
  type = "clinical"
  )
```
