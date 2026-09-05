# Visualize interactive UpSet plot of staging codelists with duplicates

An UpSet plot is a good alternative to a Venn diagram to visualize
intersections of more than \\3\\ sets: it shows which sets have common
elements with which other set and the size of the intersection, as well
as the original size of every set.

The function `shinyConceptDuplicates()` launches a simple shiny app with
filters for subcategory, edition and classification to display a dynamic
UpSet plot of staging codelists intersection.

## Usage

``` r
shinyConceptDuplicates(cdm, concept_folder = "stages")
```

## Arguments

- cdm:

  A cdm instance, needed to extract concept sets.

- concept_folder:

  The character string indicating the folder under `inst/concept_sets`
  where the concept sets of interest are saved in json files, default
  `"stages"`.

## Value

Launches a Shiny app.
