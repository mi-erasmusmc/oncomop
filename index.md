# oncomop

`oncomop` is designed to facilitate patient characterisation with OMOP
analytical tools that follow a `dplyr` pipe-based workflow, such as
PatientProfiles and CohortCharacteristics. It determines cancer stage in
cohorts using UICC guidelines.

To install the package, use `renv` or `remotes`:

``` r


# install.packages("renv")
renv::install("mi-erasmusmc/oncomop")

# install.packages("remotes")
remotes::install_github("mi-erasmusmc/oncomop")
```

## Create a cancer cohort

`oncomop` derives a stage using the intersection of T, N, and M concept
sets. The example below derives the clinical eighth-edition stage for a
breast-cancer cohort.

Use CDMConnector to create a reference to an OMOP database and generate
a cancer cohort as input to the `oncomop` package:

``` r

cdm$breast_cancer <- omopgenerics::newCodelist(
  list(breast_cancer = 4308306L), # Infiltrating ductal carcinoma
  cdm = cdm
) |>
  CohortConstructor::conceptCohort(
    cdm = cdm,
    conceptSet = _,
    name = "breast_cancer"
  )
```

``` r

#> # A tibble: 3 x 4
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <int>      <int> <date>            <date>
#> 1                    1          1 2020-01-15        2023-12-31
#> 2                    1          2 2021-06-20        2023-12-31
#> 3                    1          3 2022-09-10        2023-12-31
```

### Derive the stage for each record in the cohort

Execute
[`oncomop::addStages()`](https://mi-erasmusmc.github.io/oncomop/reference/addStages.md)
and set `showIntersect = TRUE` to display the columns containing the T,
N, and M date intersections:

``` r

cdm$breast_cancer |>
  oncomop::addStages(
    cdm = cdm,
    cancer = "breast",
    window = list(c(-7, 60)), # Search from 7 days before to 60 days after the index date
    edition = "8th",
    type = "clinical",
    order = "last",
    showIntersect = TRUE
  )
```

``` r

#> # A tibble: 3 x 13
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date t1         t1mi       t4d        n0         n1         n3a        m0         m1         cancer_stage
#>                  <int>      <int> <date>            <date>         <date>     <date>     <date>     <date>     <date>     <date>     <date>     <date>     <chr>
#> 1                    1          1 2020-01-15        2023-12-31     2020-01-15 NA         NA         2020-01-15 NA         NA         2020-01-15 NA         IA
#> 2                    1          2 2021-06-20        2023-12-31     NA         2021-06-20 NA         NA         NA         2021-06-20 2021-06-20 NA         IIIC
#> 3                    1          3 2022-09-10        2023-12-31     NA         NA         2022-09-10 NA         2022-09-10 NA         NA         2022-09-10 IV
```

### Return the cancer stage only

If `showIntersect = FALSE`, `oncomop` will add a single `cancer_stage`
column:

``` r

cdm$breast_cancer |>
  oncomop::addStages(
    cdm = cdm,
    cancer = "breast",
    window = list(c(-7, 60)), # Search from 7 days before to 60 days after the index date
    edition = "8th",
    type = "clinical",
    order = "last"
  )
```

The result is the original cohort with a derived `cancer_stage` column:

``` r

#> # A tibble: 3 x 5
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date cancer_stage
#>                  <int>      <int> <date>            <date>          <chr>
#> 1                    1          1 2020-01-15        2023-12-31      IA
#> 2                    1          2 2021-06-20        2023-12-31      IIIC
#> 3                    1          3 2022-09-10        2023-12-31      IV
```

## The rules

`oncomop` includes a set of predefined, UICC-based rules for eight
cancer types. Call `oncomop::ruleset()` to inspect the available rules.

``` r


oncomop::ruleset(
  cancer = "breast",
  edition = "9th",
  type = "clinical"
  )
```
