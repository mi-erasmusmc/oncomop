# `addStages()` to a cohort

It uses a codelist to date intersect with a cancer cohort. Imposes a
predefined or custom set of rules to identify summary stages.

## Usage

``` r
addStages(
  cohort,
  cdm,
  cancer,
  window = list(c(0, 0)),
  edition = "8th",
  type = "base",
  order = "last",
  showTnm = FALSE
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

- edition:

  A choice of "unspecified", "7th" and "8th".

- type:

  A choice from "base", "clinical" or "pathological".

- order:

  A choice from "first" or "last". If more that one code intersected,
  the order defines which code to intersect in the window.

- showTnm:

  If TRUE, the cohort will show the date intersects for each matching
  code. Default FALSE.

## Value

A cohort table containing the identified cancer stages.
