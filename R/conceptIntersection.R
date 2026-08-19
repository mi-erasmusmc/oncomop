#' Check if concept sets have common elements
#'
#' @description
#' This function performs a simple boolean check to determine whether the codelists
#' generated from a number of concept sets have any element in common.
#'
#' @details
#' The intersection of codelists is calculated as follows: given an array of codelists,
#' the function uses the outer product of the array with the array itself to calculate
#' the length of the intersection of every possible combination of two codelists.
#' The result is a symmetric matrix where the element in position \eqn{(i, j)} is the number
#' of common codes between codelist \eqn{i} and codelist \eqn{j}, and the elements on
#' the diagonal represent the size of every codelist.
#'
#' To simply assess the presence of common codes, it is enough to set the diagonal to \eqn{0}
#' and check if there is any other element \eqn{> 0} in the matrix.
#'
#' @param cdm A cdm instance, needed to extract concept sets.
#' @param concept_folder The character string indicating the folder under `inst/concept_sets`
#' where the concept sets of interest are saved in json files, default `stages`.
#' @returns `TRUE` if the codelists have any elements in common, `FALSE` if they don't.
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


#' Visualize static UpSet plot of staging codelists intersections
#'
#' @description
#' An UpSet plot is a good alternative to a Venn diagram to visualize intersections
#' of more than \eqn{3} sets: it shows which sets have common elements with which other set
#' and the size of the intersection, as well as the original size of every set.
#'
#' The function `vizConceptIntersection()` allows to filter available staging codelists by subcategory,
#' edition or classification and plots the intersections of codelists of interest.
#'
#' @param cdm A cdm instance, needed to extract concept sets.
#' @param concept_folder The character string indicating the folder under `inst/concept_sets`
#' where the concept sets of interest are saved in json files, default `"stages"`.
#' @param input_subcategory The specific subcategory to filter by (`"T0"`, `"N0"`, `"M0"`, `...`),
#' default `NULL` (corresponds to all subcategories).
#' @param input_edition The staging system edition to filter by (`"7th"`, `"8th"`, `"unspecified`),
#' default `NULL` (corresponds to all editions).
#' @param input_classification The classification type to filter by (`"clinical"`, `"pathological"`,
#' `"unspecified"`), default `NULL` (corresponds to all classifications).
#'
#' @returns Prints the plot and returns `NULL`.
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

  p <- suppressWarnings(
    UpSetR::upset(
      df_to_plot,
      nsets = ncol(df_to_plot),
      matrix.color = "#1a359b",
      main.bar.color = "#1a359b",
      sets.bar.color = "#1a359b",
      shade.color = "#58acdf",
      order.by = "freq"
    )
  )

  print(p)
}


#' Visualize interactive UpSet plot of staging codelists intersections
#'
#' @description
#' An UpSet plot is a good alternative to a Venn diagram to visualize intersections
#' of more than \eqn{3} sets: it shows which sets have common elements with which other set
#' and the size of the intersection, as well as the original size of every set.
#'
#' The function `shinyConceptIntersection()` launches a simple shiny app with filters for subcategory,
#' edition and classification to display a dynamic UpSet plot of staging codelists intersection.
#'
#' @param cdm A cdm instance, needed to extract concept sets.
#' @param concept_folder The character string indicating the folder under `inst/concept_sets`
#' where the concept sets of interest are saved in json files, default `"stages"`.
#'
#' @returns Launches a Shiny app.
shinyConceptIntersection <- function(
    cdm,
    concept_folder = "stages"
    ) {

  ui <- shiny::fluidPage(

    # Shiny app style
    shiny::tags$head(

      shiny::tags$style(shiny::HTML("

      h2 {
      font-family: Calibri, sans-serif;
      }

      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 12px;
        padding: 18px;
      }

      .control-label {
        font-weight: 600;
        margin-top: 8px;
      }

      .selectize-input {
        border-radius: 8px;
        min-height: 42px;
      }

      .selectize-dropdown {
        border-radius: 8px;
      }

      .filter-row > div {
        padding-left: 6px !important;
        padding-right: 6px !important;
      }

    "))
    ),

    shiny::titlePanel("Cancer staging concept intersections"),

    shiny::div(

      style = "
    max-width: 1500px;
    margin-left: auto;
    margin-right: auto;
    ",

      # filters card
      shiny::div(

        style = "
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 12px;
        padding: 12px 16px 4px 16px
        margin-bottom: 16px;
      ",

        shiny::fluidRow(

          class = "filter-row",

          shiny::column(
            width = 3,
            # Edition
            shiny::selectizeInput(
              inputId = "edition",
              label = "Edition:",
              choices = c("7th",
                          "8th",
                          "Unspecified"),
              selected = c("7th",
                           "8th"),
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Select editions"
              )
            )
          ),

          shiny::column(
            width = 4,
            # Classification
            shiny::selectizeInput(
              inputId = "classification",
              label = "Classification:",
              choices = c("Clinical",
                          "Pathological",
                          "Unspecified"),
              selected = c("Clinical",
                           "Pathological"),
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Select classifications"
              )
            )
          ),

          shiny::column(
            width = 5,
            shiny::selectizeInput(
              inputId = "subcategory",
              label = "Subcategory:",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                plugins = list("remove_button"),
                placeholder = "Select T/N/M subcategories"
              )
            )
          )
        )
      ),

      shiny::fluidRow(

        shiny::column(
          width = 12,

          # plot card
          shiny::div(

            style = "
          background: white;
          border: 1px solid #dee2e6;
          border-radius: 12px;
          padding: 16px;
          margin-top: 10px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.08);
          ",

            # UpSet plot
            shiny::plotOutput(
              outputId = "upset_plot",
              height = "900px"
            )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Extract codelists
    codelist <- extractInnerConcepts(
      cdm,
      path = concept_folder
    )

    stage_concepts <- codelist |>
      filterStageConcepts()

    stage_categories <- names(stage_concepts)

    # Create matrix with stages meta-data
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
      subcategory = stringr::str_extract(stage_categories, "\\b[TMN][0-9]+\\b")
    )
    # Add other columns
    meta_stages$category <- substr(meta_stages$subcategory, 1, 1)

    df <- stack(stage_concepts)
    colnames(df) <- c("code", "codelist")

    incidence <- xtabs(~ code + codelist, data = df)
    incidence[incidence > 0] <- 1


    # Update subcategories based on what's in the data
    shiny::observeEvent(TRUE, {

      available_subcategories <- meta_stages |>
        dplyr::pull(subcategory) |>
        unique() |>
        sort()

      shiny::updateSelectizeInput(
        session = session,
        inputId = "subcategory",
        choices = available_subcategories,
        selected = available_subcategories[1],
        server = TRUE
      )

    }, once = TRUE)

    # Generate UpSet plot
    output$upset_plot <- shiny::renderPlot({

      selected_stages <- meta_stages

      # Filter by edition
      if (length(input$edition) > 0) {
        selected_stages <- selected_stages |>
          dplyr::filter(
            edition %in% tolower(input$edition)
          )
      }
      # Filter by classification
      if (length(input$classification) > 0) {
        selected_stages <- selected_stages |>
          dplyr::filter(
            classification %in% tolower(input$classification)
          )
      }
      # Filter by subcategory
      if (length(input$subcategory) > 0) {
        selected_stages <- selected_stages |>
          dplyr::filter(
            subcategory %in% input$subcategory
          )
      }

      # Select the codelists that survived the filters
      sel <- selected_stages$concept_list

      # If no codelists match the selection, display empty plot and error message
      if (length(sel) == 0) {

        graphics::plot.new()

        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = "No concept sets match the selected filters."
        )

        return()
      }

      # Select corresponding columns from incidence matrix
      upset_mat <- incidence[
        ,
        sel,
        drop = FALSE
      ]

      # Convert to data.frame of 0/1 values
      upset_df <- as.data.frame(upset_mat > 0)

      upset_df[] <- lapply(
        upset_df,
        as.integer
      )

      # Remove concepts that aren't in any selected codelist
      df_to_plot <- upset_df[
        rowSums(upset_df) > 0,
        ,
        drop = FALSE
      ]

      # If there are no concepts to plot, display empty plot and error message
      if (nrow(df_to_plot) == 0) {

        graphics::plot.new()

        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = "No concepts found for the selected codelists."
        )

        return()
      }

      suppressWarnings(
        UpSetR::upset(
          df_to_plot,
          nsets = ncol(df_to_plot),
          matrix.color = "#003399",
          main.bar.color = "#003399",
          sets.bar.color = "#003399",
          shade.color = "#58acdf",
          point.size = 2.5,
          line.size = 0.7,
          text.scale = c(
            1.8, # intersection size title
            1.6, # intersection size ticks
            1.8, # set size title
            1.6, # set size tick labels
            2,   # set names
            1.6  # numbers above bars
          ),
          order.by = "freq"
        )
      )

    })

  }

  shiny::shinyApp(ui = ui, server = server)

}
