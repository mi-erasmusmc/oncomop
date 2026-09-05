# Extract concept descendants from packaged concept sets

Imports concept set JSON files from `inst/concept_sets/<path>` and
returns a codelist where each entry contains all descendants of each
concept in the source concept sets.

## Usage

``` r
extractInnerConcepts(cdm, path, cancerName)
```

## Arguments

- cdm:

  A CDM reference created with `CDMConnector`.

- path:

  A character string with the folder name under `inst/concept_sets/`
  containing the concept set JSON files.

## Value

An `omopgenerics` codelist object where names are concept names and
values are vectors of descendant concept IDs.
