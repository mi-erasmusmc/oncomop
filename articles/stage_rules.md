# stage_rules

``` r

library(oncomop)

tnm_files_data <- system.file(
  "tnm_files",
  package = "oncomop"
  ) |>
    list.files(
      full.names = TRUE
    ) |>
    oncomop:::readStagesRDS()
```

8th classification

``` r


names(tnm_files_data$tnm_stage_mapping)

tnm_files_data$tnm_stage_mapping$edition |>
  unique() 

tnm_files_data$tnm_stage_mapping$site |>
  unique() 

tnm_files_data$tnm_stage_mapping |> 
  dplyr::filter(
    edition == "8th",
    site == "breast"
  ) |> 
    dplyr::group_by(
      edition,
      site,
      stage_grouping_scope,
      T,
      N,
      M
    ) |> 
      dplyr::summarise(
        n = dplyr::n()
        ) |> View()
```
