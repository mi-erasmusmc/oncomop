checkConceptIntersection <- function(
    cdm,
    concept_folder = "stages"
    ) {

  codelist <- extractInnerConcepts(
    cdm,
    path = concept_folder
  )

  stage_concepts <- codelist |>
    filterStageConcepts()

  intersection_matrix <- outer(
    names(stage_concepts),
    names(stage_concepts),
    Vectorize(function(x, y) {
      length(intersect(stage_concepts[[x]], stage_concepts[[y]]))
    })
  )
  diag(intersection_matrix) <- 0

  return(any(intersection_matrix > 0))

}

vizConceptIntersection <- function(
    cdm,
    concept_folder = "stages",
    input_subcategory = NULL,
    input_edition = NULL,
    input_classification = NULL
    ) {

  codelist <- extractInnerConcepts(
    cdm,
    path = concept_folder
  )

  stage_concepts <- codelist |>
    filterStageConcepts()

  categories_names <- names(stage_concepts)

  # Create matrix of metadata
  meta_stages <- data.frame(
    concept_list = categories_names,
    edition = dplyr::case_when(
      stringr::str_detect(categories_names, "\\b7th\\b") ~ "7th",
      stringr::str_detect(categories_names, "\\b8th\\b") ~ "8th",
      TRUE ~ "unspecified"
    ),
    classification = dplyr::case_when(
      stringr::str_detect(categories_names, "\\bclinical\\b") ~ "clinical",
      stringr::str_detect(categories_names, "\\bpathological\\b") ~ "pathological",
      TRUE ~ "unspecified"
    ),
    subcategory = stringr::str_extract(categories_names, "\\b[TMN][0-9]+\\b")
  )
  meta_stages$category <- substr(meta_stages$subcategory, 1, 1)

  data <- meta_stages

  # Filter by subcategory
  if (!is.null(input_subcategory)) {
    data <- data |>
      dplyr::filter(
        subcategory == input_subcategory
      )
  }
  # Filter by edition
  if (!is.null(input_edition)) {
    data <- data |>
      dplyr::filter(
        edition == input_edition
      )
  }
  # Filter by classification
  if (!is.null(input_classification)) {
    data <- data |>
      dplyr::filter(
        classification == input_classification
      )
  }

  if (nrow(data) == 0) {
    cli::cli_abort(c(
      "x" = "No codelists match the specified parameters.",
      "i" = "Try changing edition, classification, or subcategory."
    ))
  }

  if (nrow(data) == 1) {
    cli::cli_abort(c(
      "x" = "Only one codelist matches the specified parameters.",
      "i" = "At least two are required to compute intersections. Try changing edition, classification, or subcategory."
    ))
  }

  df <- stack(stage_concepts)
  colnames(df) <- c("code", "codelist")

  incidence <- xtabs(~ code + codelist, data = df)
  incidence[incidence > 0] <- 1

  upset_mat <- incidence[, data$concept_list, drop = FALSE]

  upset_df <- as.data.frame(upset_mat > 0)
  upset_df[] <- lapply(
    upset_df,
    as.integer
  )

  df_to_plot <- upset_df[
    rowSums(upset_df) > 0,
    ,
    drop = FALSE
  ]

  UpSetR::upset(
    df_to_plot,
    nsets = ncol(df_to_plot),
    matrix.color = "#1a359b",
    main.bar.color = "#1a359b",
    sets.bar.color = "#1a359b",
    shade.color = "#58acdf",
    order.by = "freq"
    )
}
