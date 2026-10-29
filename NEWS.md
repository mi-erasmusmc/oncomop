# oncomop 0.4.1

# oncomop 0.4.0

* Fixed typo in HER2 positive rule for subtypes. 

* Added ovarian cancer ruleset. 

* Fixed bug related to columns in tnm_mapping.rds

# oncomop 0.3.6

* `addStages()` updated documentation and bindings fixes. 

# oncomop 0.3.5

* `addStages()` declares 'no_stage_found' if no matching rule found.

# oncomop 0.3.4

* `addStages()` logs and returns invisible if no rules or concepts found for combination of parameters.  

# oncomop 0.3.3

* Updating cancer sites labeles to match onconet.

# oncomop 0.3.2

* Removing 'cdm' argument from addStages.

# oncomop 0.3.1

* `extractStageRules()` now is exported. 

* GitHub action for R CMD check standard. 

* `PatientProfiles` >= 1.6.0 required in description. 

* `readRDS` functions updated to pass all tests. 

# oncomop 0.3.0

* `.addStages` now detects the T, N and M closest to index. 

# oncomop 0.2.5

* `assertCancerCohortName()` bug fixed related to the name expectation.

# oncomop 0.2.4

* `.addSubtype` now includes an alert and error message if the concept ids from 'OMOP genomics' are not in the vocabulary.

# oncomop 0.2.3

* `addSubtype()` integration.

# oncomop 0.2.2

* Fixed test.

# oncomop 0.2.1

* Fix to pass checks. 

# oncomop 0.2.0

* Onconet test release. 

# oncomop 0.1.1

* `addSubtypes.R` file in wrong place.

# oncomop 0.1.0

* `oncomop::addStages()` initial release.
