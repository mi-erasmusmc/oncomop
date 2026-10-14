# Changelog

## oncomop 0.3.3

- Updating cancer sites labeles to match onconet.

## oncomop 0.3.2

- Removing ‘cdm’ argument from addStages.

## oncomop 0.3.1

- `extractStageRules()` now is exported.

- GitHub action for R CMD check standard.

- `PatientProfiles` \>= 1.6.0 required in description.

- `readRDS` functions updated to pass all tests.

## oncomop 0.3.0

- `.addStages` now detects the T, N and M closest to index.

## oncomop 0.2.5

- [`assertCancerCohortName()`](https://mi-erasmusmc.github.io/oncomop/reference/assertCancerCohortName.md)
  bug fixed related to the name expectation.

## oncomop 0.2.4

- `.addSubtype` now includes an alert and error message if the concept
  ids from ‘OMOP genomics’ are not in the vocabulary.

## oncomop 0.2.3

- [`addSubtype()`](https://mi-erasmusmc.github.io/oncomop/reference/addSubtype.md)
  integration.

## oncomop 0.2.2

- Fixed test.

## oncomop 0.2.1

- Fix to pass checks.

## oncomop 0.2.0

- Onconet test release.

## oncomop 0.1.1

- `addSubtypes.R` file in wrong place.

## oncomop 0.1.0

- [`oncomop::addStages()`](https://mi-erasmusmc.github.io/oncomop/reference/addStages.md)
  initial release.
