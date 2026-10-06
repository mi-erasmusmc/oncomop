# Add cancer subtypes to a cohort

This function filters and appends specific cancer subtype information to
the cohort data based on the provided cancer site, timeframe window, and
intersection logic.

## Usage

``` r
addSubtype(
  cohort,
  cdm,
  cancer,
  window = list(c(-90, 90)),
  showIntersect = FALSE
)
```

## Arguments

- cohort:

  A cohort table with cancer patients from a cdm reference object.

- cdm:

  A cdm reference object.

- cancer:

  In character, the affected site, a choice of: "bladder", "breast",
  "colorectal", "lung", "melanoma", "oesophagus" and "prostate".

- window:

  to look up stages codes.

- showIntersect:

  If TRUE, the cohort will show the date intersects for each matching
  code. Default FALSE.
