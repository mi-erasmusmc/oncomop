# Check if codelists have common elements

This function performs a simple boolean check to determine whether the
codelists generated from a number of concept sets have any element in
common.

## Usage

``` r
assertUniqueConcepts(cdm, concept_folder = "stages")
```

## Arguments

- cdm:

  A cdm instance, needed to extract concept sets.

- concept_folder:

  A character string with the folder name under `inst/concept_sets/`
  containing the concept set JSON files, default `stages`.

## Value

Invisible boolean: `TRUE` if the codelists elements are unique, `FALSE`
otherwise.

## Details

The intersection of codelists is calculated as follows: given an array
of codelists, the function uses the outer product of the array with the
array itself to calculate the length of the intersection of every
possible combination of two codelists. The result is a symmetric matrix
where the element in position \\(i, j)\\ is the number of common codes
between codelist \\i\\ and codelist \\j\\, and the elements on the
diagonal represent the size of every codelist.

To simply assess the presence of common codes, it is enough to set the
diagonal to \\0\\ and check if there is any other element \\\> 0\\ in
the matrix.
