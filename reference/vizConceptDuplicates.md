# Visualize static UpSet plot of staging codelists with duplicates

An UpSet plot is a good alternative to a Venn diagram to visualize
intersections of more than \\3\\ sets: it shows which sets have common
elements with which other set and the size of the intersection, as well
as the original size of every set.

The function `vizConceptDuplicates()` allows to filter available staging
codelists by subcategory, edition or classification and plots the
intersections of codelists of interest.

## Usage

``` r
vizConceptDuplicates(
  cdm,
  concept_folder = "stages",
  input_subcategory = NULL,
  input_edition = NULL,
  input_classification = NULL
)
```

## Arguments

- cdm:

  A cdm instance, needed to extract concept sets.

- concept_folder:

  The character string indicating the folder under `inst/concept_sets`
  where the concept sets of interest are saved in json files, default
  `"stages"`.

- input_subcategory:

  The specific subcategory to filter by (`"T0"`, `"N0"`, `"M0"`, `...`),
  default `NULL` (corresponds to all subcategories).

- input_edition:

  The staging system edition to filter by (`"7th"`, `"8th"`,
  `"unspecified`), default `NULL` (corresponds to all editions).

- input_classification:

  The classification type to filter by (`"clinical"`, `"pathological"`,
  `"unspecified"`), default `NULL` (corresponds to all classifications).

## Value

Prints the plot and returns `NULL`.
