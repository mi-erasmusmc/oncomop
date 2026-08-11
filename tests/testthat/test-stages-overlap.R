test_that("Overlap of TNM codes", {

  testName <- "stages_patients_one_patient"

  cdm <- TestGenerator::patientsCDM(
    testName = testName,
    vocabulary = "v20260227_complete",
    cdmVersion = "5.4"
  )

  cdm <- createCancerCohorts(
    cdm,
    path = "cancer_cohorts",
    name = "cancer_cohorts"
  )

  codelist1 <- file.path(
    conceptSetsPath(),
    "stages"
  ) |>
    omopgenerics::importConceptSetExpression() |>
    CodelistGenerator::asCodelist(cdm)

  codelist2 <- extractInnerConcepts(
    cdm,
    path = "stages"
  )

  codelist3 <- codelist2 |>
    filterStageConcepts()

  # Sample overlap of codelists
  overlap <- length(intersect(codelist2$`AJCC/UICC 7th M0 Category`, codelist2$`AJCC/UICC 8th M0 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(codelist2$`AJCC/UICC clinical M0 Category`, codelist2$`AJCC/UICC 7th clinical M0 Category`))
  expect_false(overlap == 0)  # OVERLAP
  overlap <- length(intersect(codelist2$`AJCC/UICC clinical M0 Category`, codelist2$`AJCC/UICC 8th clinical M0 Category`))
  expect_false(overlap == 0)  # OVERLAP

  # Sample overlap of stage_concepts -> codelist3 -> parent_concepts
  overlap <- length(intersect(codelist3$`AJCC/UICC 7th M1 Category`, codelist3$`AJCC/UICC 8th M1 Category`))
  expect_equal(overlap, 0)    # no overlap
  overlap <- length(intersect(codelist3$`AJCC/UICC N2 Category`, codelist3$`AJCC/UICC 7th N2 Category`))
  expect_false(overlap == 0)  # OVERLAP
  overlap <- length(intersect(codelist3$`AJCC/UICC N2 Category`, codelist3$`AJCC/UICC 8th N2 Category`))
  expect_false(overlap == 0)  # OVERLAP

  # ----- Up till here is contained already in test-addStage.R -----

  # Focus on stage_concepts ---

  stage_categories <- names(codelist3)
  # Get names of clinical/pathological/unspecified categories
  # clinical_categories <- names(stage_categories[str_detect(stage_categories, stringr::regex("\\bclinical\\b"))])
  # pathological_categories <- names(stage_categories[str_detect(stage_categories, stringr::regex("\\bpathological\\b"))])
  # unspecified_categories <- setdiff(stage_categories, c(clinical_categories, pathological_categories))
  #
  # # Get names of 7th/8th edition categories
  # edition7_categories <- names(stage_concepts[str_detect(stage_categories, stringr::regex("\\b7th\\b"))])
  # edition8_categories <- names(stage_concepts[str_detect(stage_categories, stringr::regex("\\b8th\\b"))])
  # edition_unspeficied <- setdiff(stage_categories, c(edition7_categories, edition8_categories))

  # df_stages <- stack(stage_concepts)                               # 2718 code appearances
  # colnames(df_stages) <- c("code", "codelist")
  #
  # example_stage <- 1633268
  # df_stages |>
  #   dplyr::filter(code == example_stage)

  meta_stages <- data.frame(
    concept_list = stage_categories,
    edition = dplyr::case_when(
      stringr::str_detect(stage_categories, "\\b7th\\b") ~ "7th",
      stringr::str_detect(stage_categories, "\\b8th\\b") ~ "8th",
      TRUE ~ "unspecified"
    ),
    classification = dplyr::case_when(
      stringr::str_detect(stage_categories, "\\bclinical\\b") ~ "clinical",
      stringr::str_detect(stage_categories, "\\bpathological\\b") ~ "pathological",
      TRUE ~ "unspecified"
    ),
    stage = stringr::str_extract(stage_categories, "\\b[TMN][0-9]+\\b")
  ) #|>
  # dplyr::mutate(
  #   macro_stage = stringr::str_extract(stage, "^[TMN]")
  # )

  # Add other columns
  meta_stages$type <- substr(meta_stages$stage, 1, 1)

  df <- stack(codelist3)
  colnames(df) <- c("code", "codelist")
  incidence <- xtabs(~ code + codelist, data = df)
  incidence[incidence > 0] <- 1
  pairwise_overlap <- crossprod(incidence)

  # Option 1 heatmap

  sel1 <- meta_stages$concept_list[meta_stages$classification == "unspecified" & meta_stages$edition == "7th" & meta_stages$type == "T"]
  sel2 <- meta_stages$concept_list[meta_stages$classification == "clinical" & meta_stages$edition == "7th" & meta_stages$type == "T"]
  mat <- pairwise_overlap[sel1, sel2]
  heat_df <- reshape2::melt(as.matrix(mat))
  colnames(heat_df) <- c("codelist1", "codelist2", "value")

  ggplot2::ggplot(heat_df, aes(codelist1, codelist2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = value), size = 3) +
    #ggplot2::scale_fill_viridis_c() +
    ggplot2::scale_fill_distiller(palette = "Blues") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                   axis.title = element_blank())

  # Option heatmap 2
  heat_df2 <- heat_df[heat_df$value > 0, ]

  ggplot2::ggplot( heat_df2, ggplot2::aes(codelist1, codelist2, fill = value) ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = value), size = 3) +
    #ggplot2::scale_fill_viridis_c() +
    ggplot2::scale_fill_distiller(palette = "Blues") +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme( axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), axis.title = ggplot2::element_blank() )

  # Option heatmap 3 (upper triangle, except it's not a triangle)
  heat_df$row <- match(heat_df$codelist1, sel1)
  heat_df$col <- match(heat_df$codelist2, sel2)

  heat_df_upper <- heat_df[heat_df$row <= heat_df$col, ]
  heat_df_upper <- heat_df_upper[heat_df_upper$value > 0, ]

  ggplot2::ggplot( heat_df_upper, ggplot2::aes(codelist1, codelist2, fill = value) ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = value), size = 3) +
    #ggplot2::scale_fill_viridis_c() +
    ggplot2::scale_fill_distiller(palette = "Blues") +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme( axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_blank() )

  # UpSet plot

  sel <- unique(meta_stages$concept_list[meta_stages$type == "M"]) # meta_stages$edition == "7th" &
  # see how it looks with all the 96 codelists
  # sel <- unique(meta_stages$concept_list)
  # answer: bad

  upset_mat <- incidence[, sel, drop = FALSE]
  upset_df <- as.data.frame(upset_mat > 0)
  upset_df[] <- lapply(
    upset_df,
    as.integer
  )
  head(upset_df)
  colSums(upset_df)
  # rows: concept codes
  # columns: selected codelists
  str(upset_df)
  dim(upset_df)
  colnames(upset_df)
  str(upset_df)
  df_to_plot <- upset_df[
    rowSums(upset_df) > 0,
    ,
    drop = FALSE
  ]
  UpSetR::upset(df_to_plot,
                nsets = ncol(df_to_plot),
                #nintersects = 20,
                order.by = "freq")

  ComplexUpset::upset(
    df_to_plot,
    colnames(df_to_plot)
  )

  ComplexUpset::upset(
    df_to_plot,
    colnames(df_to_plot),
    name = "Codelists",
    width_ratio = 0.2
  ) +
    ggplot2::theme_minimal()


  # Option Venn diagrams
  sel <- unique(
    meta_stages$concept_list[
      meta_stages$stage == "M0" # add stuff like meta_stages$edition == "7th" &
    ]
  )

  venn_df <- as.data.frame(incidence[, sel, drop = FALSE] > 0)

  venn_df <- venn_df[ rowSums(venn_df) > 0, , drop = FALSE ]

  # To show only codes that actually overlap
  # venn_df_shared <- venn_df[ rowSums(venn_df) > 1, , drop = FALSE ]

  # To shorten the labels
  # meta_sel <- meta_stages[match(sel, meta_stages$concept_list), ]
  # short_labels <- meta_sel$stage
  # colnames(venn_df) <- short_labels

  v <- venneuler::venneuler(venn_df)

  labels <- names(venn_df) |>
    stringr::str_remove(("^AJCC/UICC ")) |>
    stringr::str_remove((" Category$")) # |>
  #stringr::str_replace("AJCC/UICC\\s+", "AJCC/UICC\n")
  v$labels <- labels
  v$centers <- v$centers + 1

  graphics::par(cex = 0.9)

  plot(
    v,
    main = "7th Clinical T Categories",
    cex.main = 1.2
  )

  # Option textbox "intersection" that gives us directly the codes in common,
  # in the form of plain text so it's easy to copy
  # ...

  # Option Shiny app


  # For now, build just a basically interactive UpsetPlot.
  # the app should look like this:
  # ------------------------------------------------------------
  # |  [Title]
  # |  Codelists to intersect: [Edition] [Category (clinical/pathological)] [TNM]
  # | default: "all" for all the codelists ...? No, it's a mess, make some rules that it cannot happen
  # |   _______________   _____________________________
  # |  |              |  |                            |
  # |  |    Venn      |  |            UpSet           |
  # |  |   Diagram    |  |            plot            |
  # |  |              |  |                            |
  # |  |              |  |                            |
  # |  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾   ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
  # | (Ignore the Venn diagram for now)
  # |   _____________________________
  # |  |        text box with       |
  # |  |      overlapping codes     |
  # |  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

  selectInput( "classification", "Classification",
               choices = c("All", "clinical", "pathological") )

  selectInput( "type", "Stage type", choices = c("All", "T", "N", "M") )

  selectInput( "edition", "Edition", choices = c("All", "7th", "8th") )

  # code_counts <- sort(table(df_stages$code), decreasing = TRUE)    # 1300 unique codes
  # overlapping_codes <- code_counts[code_counts > 1]                # 906 codes in more than one codelist
  # head(overlapping_codes)
  # overlap_map <- split(df_stages$codelist, df_stages$code)
  # overlap_map <- overlap_map[names(overlapping_codes)]
  #
  # overlap_map[["1633468"]]
  #
  # table(code_counts)
  #
  # incidence <- xtabs(~ code + codelist, data = df_stages)
  # incidence[incidence > 0] <- 1
  # pairwise_overlap <- crossprod(incidence)
  # pairwise_overlap[1:2, 1:2]
  #
  # heatmap(as.matrix(pairwise_overlap), Rowv = NA, Colv = NA, scale = "none")
  #
  #
  # cdm$cancer_cohorts |>
  #   addStages(
  #     cdm,
  #     stageConcepts = stage_concepts,
  #     name = "cancer_stage_concepts"
  #   ) |>
  #   collect() |>
  #   pull(stages) |>
  #   expect_equal(
  #     "ajcc_uicc_7th_clinical_m1_category_m90_to_90"
  #   )

})




# Expectations

stage_pattern <- "(?<=ajcc_uicc_(?:(?<edition>[78]th)_)?(?:(?<classification>clinical|pathological)_)?)(?<stage>[tnm])"
# add [0-9]*[a-z]* after [tnm] to keep also numbers and letters (e.g. "t2", "n3a", "m1b")

stringr::str_extract(
  c("ajcc_uicc_t2_category_m90_to_90", "ajcc_uicc_pathological_n0_category_m90_to_90",
    "ajcc_uicc_7th_m0_category_m90_to_90", "ajcc_uicc_8th_m0_category_m90_to_90",
    "ajcc_uicc_7th_clinical_n0_category_m90_to_90", "ajcc_uicc_7th_n1a_category_m90_to_90"),
  stage_pattern
)

matches <- stringr::str_match(
  c("ajcc_uicc_t2_category_m90_to_90", "ajcc_uicc_pathological_n0_category_m90_to_90",
    "ajcc_uicc_7th_m0_category_m90_to_90", "ajcc_uicc_8th_m0_category_m90_to_90"),
  stage_pattern
)

colnames(matches)

# collect() |>
# pull(stages) |>
# expect_equal(
#   "ajcc_uicc_7th_clinical_m1_category_m90_to_90"
# )
