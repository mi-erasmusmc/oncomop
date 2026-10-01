# Copyright 2024 DARWIN EU (C)
#
# This file is a modified version of functionality in 
# PatientProfiles developed by Marti Catala and Ed Burns
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

addSubtypeIntersect <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetDate = "event_start_date",
                                    order = "first",
                                    inObservation = TRUE,
                                    nameStyle = "{concept_name}_{window_name}",
                                    name = NULL) {

  if (missing(order) & rlang::is_interactive()) {
    messageOrder(order)
  }

  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetDate,
    targetEndDate = NULL,
    inObservation = inObservation,
    order = order,
    value = "date",
    nameStyle = nameStyle,
    name = name
  )
}

.addConceptIntersect <- function(x,
                                 conceptSet,
                                 indexDate = "cohort_start_date",
                                 censorDate = NULL,
                                 window,
                                 targetStartDate = "event_start_date",
                                 targetEndDate = "event_end_date",
                                 inObservation = TRUE,
                                 order = "first",
                                 value,
                                 allowDuplicates = FALSE,
                                 nameStyle = "{value}_{concept_name}_{window_name}",
                                 name,
                                 type = "auto") {

  cdm <- omopgenerics::cdmReference(x)

  # initial checks
  conceptSet <- omopgenerics::validateConceptSetArgument(conceptSet = conceptSet, cdm = cdm)
  omopgenerics::assertChoice(targetStartDate, choices = c("event_start_date", "event_end_date"), length = 1)
  omopgenerics::assertChoice(targetEndDate, choices = c("event_start_date", "event_end_date"), length = 1, null = TRUE)
  omopgenerics::assertLogical(inObservation, length = 1)
  omopgenerics::assertCharacter(value)
  conceptSet <- validateConceptNames(conceptSet)

  tablePrefix <- omopgenerics::tmpPrefix()

  nameStyle <- nameStyle |>
    stringr::str_replace(
      pattern = "\\{concept_name\\}",
      replacement = "\\{id_name\\}"
    )
  conceptsTable <- getConceptsTable(conceptSet)
  nm <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(cdm = cdm, name = nm, table = conceptsTable)
  conceptSetId <- conceptSetId(conceptSet)
  cdm[[nm]] <- subsetTable(cdm[[nm]], value) |>
    dplyr::compute(name = nm, temporary = FALSE)
  attr(x, "cdm_reference") <- cdm
  x <- x |>
    .addIntersect(
      tableName = nm,
      value = value,
      filterVariable = "concept_set_id",
      filterId = conceptSetId$concept_set_id,
      idName = conceptSetId$concept_set_name,
      window = window,
      order = order,
      indexDate = indexDate,
      censorDate = censorDate,
      targetStartDate = targetStartDate,
      targetEndDate = targetEndDate,
      inObservation = inObservation,
      allowDuplicates = allowDuplicates,
      nameStyle = nameStyle,
      name = name,
      type = type
    )
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))

  return(x)
}
validateConceptNames <- function(cs) {
  orig <- names(cs)
  new <- tolower(orig)
  new <- gsub(pattern = "[^A-Za-z0-9_]", replacement = "_", x = new)
  new <- gsub(pattern = "_+", replacement = "_", x = new)
  idDiff <- new != orig
  diffNew <- new[idDiff]
  diffOrig <- orig[idDiff]
  if (length(diffOrig) > 0) {
    mes <- paste0("- '", diffOrig, "' -> '", diffNew, "'")
    c("The following conceptSet names have been modified:", mes) |>
      paste0(collapse = "\n") |>
      rlang::inform()
  }
  if (length(unique(new)) != length(new)) {
    cli::cli_abort(c(x = "Concept set names are not unique after formatting."))
  }
  names(cs) <- new
  return(cs)
}
getConceptsTable <- function(conceptSet) {
  purrr::map(conceptSet, dplyr::as_tibble) |>
    dplyr::bind_rows(.id = "concept_set_name") |>
    dplyr::inner_join(conceptSetId(conceptSet), by = "concept_set_name") |>
    dplyr::select("concept_id" = "value", "concept_set_id")
}
conceptSetId <- function(conceptSet) {
  dplyr::tibble(
    "concept_set_name" = names(conceptSet),
    "concept_set_id" = as.integer(seq_along(conceptSet))
  )
}

subsetTable <- function(x, value) {
  cdm <- omopgenerics::cdmReference(x)
  intersectOptions <- c("flag", "count", "date", "days")
  x <- x |>
    dplyr::left_join(
      cdm[["concept"]] |>
        dplyr::select("concept_id", "domain_id") |>
        dplyr::mutate(domain_id = tolower(.data$domain_id)),
      by = "concept_id"
    ) |>
    dplyr::compute()
  supportedDomains <- list(
    "measurement" = "measurement"
  )
  x <- checkDomainsAndTables(x, supportedDomains)
  domains <- x |>
    dplyr::distinct(.data$domain_id) |>
    dplyr::pull()
  if (length(domains) == 0) {
    res <- cdm[["concept"]] |>
      dplyr::select("concept_id") |>
      dplyr::filter(
        is.na(.data$concept_id) & !is.na(.data$concept_id)
      ) |>
      dplyr::mutate(
        "event_start_date" = as.Date("2000-01-01"),
        "event_end_date" = as.Date("2000-01-01"),
        "concept_set_id" = 0L,
        "person_id" = 0L
      ) |>
      utils::head(0)
    if (!value %in% intersectOptions) {
      res <- res |>
        dplyr::mutate(!!value := "")
    }
    return(res)
  }
  if (!value %in% intersectOptions) {
    type <- purrr::map(domains, \(x) {
      nm <- supportedDomains[[x]]
      if (!nm %in% names(cdm)) {
        type <- character()
      } else if (value %in% colnames(cdm[[nm]])) {
        type <- cdm[[nm]] |>
          dplyr::select(dplyr::all_of(value)) |>
          utils::head(1) |>
          dplyr::pull() |>
          dplyr::type_sum()
      } else {
        type <- character()
      }
      return(type)
    }) |>
      unlist() |>
      unique()
    if (length(type) == 1) {
      val <- switch(type,
                    "chr" = NA_character_,
                    "date" = as.Date(NA),
                    "dttm" = as.Date(NA),
                    "lgl" = NA,
                    "drtn" = NA_real_,
                    "dbl" = NA_real_,
                    "int" = NA_integer_,
                    "int64" = bit64::as.integer64(NA),
                    NA_character_)
      extraColumn <- TRUE
    } else if (length(type) == 0) {
      cli::cli_abort(c(x = "Column `{value}` not found in any of the tables."))
    } else {
      cli::cli_abort(c(x = "Different types found for column `{value}`."))
    }
  } else {
    extraColumn <- FALSE
  }

  purrr::map(domains, \(domain) {
    tableName <- supportedDomains[[domain]]
    sel <- c(
      "event_start_date" = startDateColumn(tableName),
      "event_end_date" = endDateColumn(tableName),
      "concept_id" = standardConceptIdColumn(tableName),
      "value_as_concept" = valueIdColumn(),
      "person_id"
    )
    if (extraColumn & value %in% colnames(cdm[[tableName]])) {
      sel <- c(sel, stats::setNames(value, value))
    }
    res <- cdm[[tableName]] |>
      dplyr::select(dplyr::all_of(sel)) |>
      dplyr::inner_join(
        x |> dplyr::select("concept_id", "concept_set_id"),
        by = "concept_id"
      )
    if (extraColumn & !value %in% colnames(cdm[[tableName]])) {
      res <- dplyr::mutate(res, !!value := .env$val)
    }
    res
  }) |>
    purrr::reduce(dplyr::union_all)
}
checkDomainsAndTables <- function(x, supportedDomains) {
  cdm <- omopgenerics::cdmReference(x)
  supDom <- names(supportedDomains)
  counts <- x |>
    dplyr::group_by(.data$domain_id) |>
    dplyr::tally() |>
    dplyr::collect()
  cnd <- counts |>
    dplyr::filter(!.data$domain_id %in% .env$supDom)
  if (nrow(cnd) > 0) {
    mes <- paste0(cnd$n, " concept(s) from domain {.pkg ", cnd$domain_id, "} eliminated as it is not supported.")
    names(mes) <- rep("!", length(mes))
    mes <- c(mes, i = "Supported domains are: {.pkg {names(supportedDomains)}}.")
    cli::cli_inform(message = mes)
  }
  x <- x |>
    dplyr::filter(.data$domain_id %in% .env$supDom)
  presentTables <- x |>
    dplyr::distinct(.data$domain_id) |>
    dplyr::pull() |>
    purrr::keep(\(dom) supportedDomains[[dom]] %in% names(cdm))
  cnt <- counts |>
    dplyr::filter(.data$domain_id %in% .env$supDom) |>
    dplyr::filter(!.data$domain_id %in% .env$presentTables)
  if (nrow(cnt) > 0) {
    mes <- paste0(cnt$n, " concept(s) from domain {.pkg ", cnt$domain_id, "} eliminated as table {.var ", supportedDomains[[cnt$domain_id]],"} is not present.")
    names(mes) <- rep("!", length(mes))
    cli::cli_inform(message = mes)
  }
  if (length(presentTables) == 0) {
    x <- x |>
      dplyr::filter(
        is.na(.data$domain_id) & !is.na(.data$domain_id)
      )
  } else {
    x <- x |>
      dplyr::filter(.data$domain_id %in% .env$presentTables)
  }
  dplyr::compute(x)
}

addConceptIntersectFlag <- function(x,
                                    conceptSet,
                                    indexDate = "cohort_start_date",
                                    censorDate = NULL,
                                    window = list(c(0, Inf)),
                                    targetStartDate = "event_start_date",
                                    targetEndDate = "event_end_date",
                                    inObservation = TRUE,
                                    nameStyle = "{concept_name}_{window_name}",
                                    name = NULL,
                                    type = "numeric") {
  .addConceptIntersect(
    x = x,
    conceptSet = conceptSet,
    indexDate = indexDate,
    censorDate = censorDate,
    window = window,
    targetStartDate = targetStartDate,
    targetEndDate = targetEndDate,
    inObservation = inObservation,
    order = "first",
    value = "flag",
    nameStyle = nameStyle,
    name = name,
    type = type
  )
}

.addIntersect <- function(
    x,
    tableName,
    value,
    filterVariable = NULL,
    filterId = NULL,
    idName = NULL,
    window = list(c(0, Inf)),
    indexDate = "cohort_start_date",
    censorDate = NULL,
    targetStartDate = startDateColumn(tableName),
    targetEndDate = endDateColumn(tableName),
    inObservation = TRUE,
    order = "first",
    allowDuplicates = FALSE,
    nameStyle = "{value}_{id_name}_{window_name}",
    name = NULL,
    type = "auto",
    call = parent.frame()
) {
  type <- validateColumnType(type, value, call)

  comp <- newTable(name)
  if (!is.list(window)) {
    window <- list(window)
  }
  targetStartDate <- eval(targetStartDate)
  targetEndDate <- eval(targetEndDate)
  x <- omopgenerics::validateCdmTable(table = x, call = call)
  indexDateInput <- materialiseIndexDate(
    indexDate = indexDate, x = x
  )
  x <- indexDateInput$x
  indexDate <- indexDateInput$indexDate
  personVariable <- omopgenerics::getPersonIdentifier(x = x, call = call)
  cdm <- omopgenerics::cdmReference(x)
  omopgenerics::assertCharacter(tableName, length = 1, na = FALSE, call = call)
  omopgenerics::validateCdmArgument(cdm = cdm, requiredTables = tableName, call = call)
  personVariableTable <- omopgenerics::getPersonIdentifier(x = cdm[[tableName]], call = call)
  extraValue <- checkValue(value, cdm[[tableName]], tableName, call = call)
  filterTbl <- checkFilter(filterVariable, filterId, idName, cdm[[tableName]], call = call)
  window <- omopgenerics::validateWindowArgument(window, call = call)
  omopgenerics::assertChoice(order, choices = c("first", "last"), call = call)
  omopgenerics::assertLogical(allowDuplicates, length = 1, call = call)
  if (!is.null(idName)) {
    idName <- omopgenerics::toSnakeCase(idName)
  }
  tablePrefix <- omopgenerics::tmpPrefix()
  overlapTable <- cdm[[tableName]]
  if (!is.null(filterTbl)) {
    filterTbl <- filterTbl |>
      dplyr::rename(!!filterVariable := "id")
    filterTblName <- omopgenerics::uniqueTableName(tablePrefix)
    cdm <- omopgenerics::insertTable(
      cdm = cdm, name = filterTblName, table = filterTbl, overwrite = TRUE
    )
    overlapTable <- overlapTable |>
      dplyr::inner_join(cdm[[filterTblName]], by = filterVariable)
  } else {
    filterTbl <- dplyr::tibble(id_name = "all")
    overlapTable <- dplyr::mutate(overlapTable, "id_name" = "all")
  }

  values <- list(
    "id_name" = filterTbl$id_name,
    "window_name" = names(window),
    "value" = value
  )

  x <- warnOverwriteColumns(x = x, nameStyle = nameStyle, values = values)

  newCols <- expand.grid(
    value = value,
    id_name = filterTbl$id_name,
    window_name = names(window)
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(colnam = as.character(glue::glue(
      nameStyle,
      value = .data$value,
      id_name = .data$id_name,
      window_name = .data$window_name
    ))) |>
    dplyr::mutate(colnam = omopgenerics::toSnakeCase(.data$colnam)) |>
    dplyr::inner_join(
      dplyr::tibble(
        window_name = names(window),
        w1 = purrr::flatten_dbl(purrr::map(window, \(x) x[1])),
        w2 = purrr::flatten_dbl(purrr::map(window, \(x) x[2]))
      ),
      by = "window_name"
    )

  nameStyle <- stringr::str_replace(nameStyle, "\\{value\\}", "\\{.value\\}")

  overlapTable <- overlapTable |>
    dplyr::select(
      !!personVariable := dplyr::all_of(personVariableTable),
      "start_date" = dplyr::all_of(targetStartDate),
      "end_date" = dplyr::all_of(targetEndDate %||% targetStartDate),
      "value_as_concept" = dplyr::all_of("value_as_concept"),
      dplyr::all_of(extraValue),
      "id_name"
    ) |>
    dplyr::mutate(end_date = dplyr::coalesce(.data$end_date, .data$start_date))

  result <- x |>
    dplyr::select(
      dplyr::all_of(personVariable),
      "index_date" = dplyr::all_of(indexDate),
      "censor_date" = dplyr::any_of(censorDate)
    ) |>
    dplyr::distinct()

  resultKey <- c(
    personVariable,
    "index_date",
    if (!is.null(censorDate)) "censor_date"
  )
  joinKey <- c(personVariable, indexDate)
  if (!is.null(censorDate)) {
    joinKey <- c(
      joinKey,
      rlang::set_names("censor_date", censorDate)
    )
  }

  if (any(value %in% c("count", "flag"))) {
    idsObs <- omopgenerics::uniqueId(n = 2, exclude = colnames(x))
    qInObservation <- newCols$colnam |>
      rlang::set_names() |>
      purrr::map(\(col) {
        w1 <- newCols$w1[newCols$colnam == col]
        w2 <- newCols$w2[newCols$colnam == col]
        if (is.infinite(w1)) {
          if (is.infinite(w2)) {
            res <- NULL
          } else {
            res <- 'dplyr::if_else(.data${idsObs[1]} <= {sprintf("%.0f", w2)}, .data[["{col}"]], NA)'
          }
        } else if (is.infinite(w2)) {
          res <- 'dplyr::if_else(.data${idsObs[2]} >= {sprintf("%.0f", w1)}, .data[["{col}"]], NA)'
        } else {
          res <- 'dplyr::if_else(.data${idsObs[1]} <= {sprintf("%.0f", w2)} & .data${idsObs[2]} >= {sprintf("%.0f", w1)}, .data[["{col}"]], NA)'
        }
        glue::glue(res)
      }) |>
      unlist() |>
      rlang::parse_exprs()
    if (length(qInObservation) > 0) {
      renamePersonId <- rlang::set_names("person_id", personVariable)
      renameDates <- rlang::set_names(
        c("observation_period_start_date", "observation_period_end_date"), idsObs
      )
      individualsWithnObservation <- x |>
        dplyr::select(dplyr::all_of(c(personVariable, indexDate))) |>
        dplyr::distinct() |>
        dplyr::inner_join(
          cdm$observation_period |>
            dplyr::select(dplyr::all_of(c(renamePersonId, renameDates))),
          by = personVariable
        ) |>
        dplyr::compute(name = omopgenerics::uniqueTableName(prefix = tablePrefix))
      individualsInObservation <- individualsWithnObservation |>
        dplyr::filter(
          .data[[indexDate]] >= .data[[idsObs[1]]] &
            .data[[indexDate]] <= .data[[idsObs[2]]]
        ) |>
        dplyr::mutate(
          !!idsObs[1] := as.integer(clock::date_count_between(start = .data[[indexDate]], end = .data[[idsObs[1]]], precision = "day")),
          !!idsObs[2] := as.integer(clock::date_count_between(start = .data[[indexDate]], end = .data[[idsObs[2]]], precision = "day"))
        ) |>
        dplyr::compute(name = omopgenerics::uniqueTableName(prefix = tablePrefix))
    }
  }

  if (isTRUE(inObservation)) {
    sel <- c("person_id", "observation_period_start_date", "observation_period_end_date") |>
      rlang::set_names(c(personVariable, "start_obs", "end_obs"))
    result <- result |>
      dplyr::inner_join(
        cdm$observation_period |>
          dplyr::select(dplyr::all_of(sel)),
        by = personVariable
      ) |>
      dplyr::filter(
        .data$start_obs <= .data$index_date & .data$index_date <= .data$end_obs
      )
  }

  result <- result |>
    dplyr::inner_join(overlapTable, by = personVariable)

  if (!is.null(censorDate)) {
    result <- result |>
      dplyr::filter(
        is.na(.data$censor_date) |
          .data$start_date <= .data$censor_date
      )
  }

  if (isTRUE(inObservation)) {
    result <- result |>
      dplyr::filter(
        .data$start_obs <= .data$end_date & .data$start_date <= .data$end_obs
      )
  }

  result <- result |>
    dplyr::mutate(
      "start" = clock::date_count_between(start = .data$index_date, end = .data$start_date, precision = "day"),
      "end" = clock::date_count_between(start = .data$index_date, end = .data$end_date, precision = "day")
    ) |>
    dplyr::select(!dplyr::any_of(c(
      "start_date", "end_date", "start_obs", "end_obs"
    ))) |>
    dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))

  resultCountFlag <- NULL
  resultDateTimeOther <- NULL
  # Start loop for different windows

  for (i in seq_along(window)) {
    win <- window[[i]]
    if (is.infinite(win[1])) {
      if (is.infinite(win[2])) {
        resultW <- result
      } else {
        resultW <- result |> dplyr::filter(.data$start <= !!win[2])
      }
    } else {
      if (is.infinite(win[2])) {
        resultW <- result |> dplyr::filter(.data$end >= !!win[1])
      } else {
        resultW <- result |>
          dplyr::filter(.data$end >= !!win[1] & .data$start <= !!win[2])
      }
    }

    resultW <- resultW |>
      dplyr::select(-"end") |>
      dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))

    # add count or flag
    if ("count" %in% value | "flag" %in% value) {
      if (identical("flag", value)) {
        resultCF <- resultW |>
          dplyr::distinct(dplyr::across(dplyr::all_of(c(resultKey, "id_name")))) |>
          dplyr::mutate(flag = 1)
      } else {
        resultCF <- resultW |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(resultKey, "id_name")))) |>
          dplyr::summarise(count = as.numeric(dplyr::n()), .groups = "drop")
        if ("flag" %in% value) {
          resultCF <- resultCF |> dplyr::mutate(flag = 1)
        }
        if (!("count" %in% value)) {
          resultCF <- resultCF |> dplyr::select(-"count")
        }
      }
      resultCF <- resultCF |>
        dplyr::mutate("window_name" = !!tolower(names(window)[i]))

      if (i == 1) {
        resultCountFlag <- resultCF |>
          dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
      } else {
        resultCountFlag <- resultCountFlag |>
          dplyr::union_all(resultCF) |>
          dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
      }
    }
    # add date, time or other
    if (length(value[!(value %in% c("count", "flag"))]) > 0) {
      if (length(extraValue) > 0) {
        resultDTO <- resultW |>
          dplyr::select(dplyr::all_of(c(resultKey, "id_name", extraValue, "days" = "start"))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(resultKey, "id_name"))))
        if (order == "first") {
          resultDTO <- resultDTO |>
            dplyr::filter(.data$days == min(.data$days, na.rm = TRUE))
        } else {
          resultDTO <- resultDTO |>
            dplyr::filter(.data$days == max(.data$days, na.rm = TRUE))
        }
        if (allowDuplicates) {
          qs <- extraValue |>
            rlang::set_names() |>
            purrr::map_chr(\(x) paste0('stringr::str_flatten(as.character(.data[["', x, '"]]), collapse = "; ")')) |>
            rlang::parse_exprs()
          resultDTO <- resultDTO |>
            dplyr::group_by(.data$days, .add = TRUE) |>
            dplyr::summarise(!!!qs, .groups = "drop")
        }
        if ("date" %in% value) {
          resultDTO <- resultDTO |>
            dplyr::mutate(date = as.Date(clock::add_days(x = .data$index_date, n = .data$days)))
        }
      } else {
        resultDTO <- resultW |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(resultKey, "id_name", "value_as_concept"))))
        if (order == "first") {
          resultDTO <- resultDTO |>
            dplyr::summarise(
              days = min(.data$start, na.rm = TRUE),
              .groups = "drop"
            )
        } else {
          resultDTO <- resultDTO |>
            dplyr::summarise(
              days = max(.data$start, na.rm = TRUE), .groups = "drop"
            )
        }
        if ("date" %in% value) {
          resultDTO <- resultDTO |>
            dplyr::mutate(date = as.Date(clock::add_days(x = .data$index_date, n = .data$days)))
        }
      }

      resultDTO <- resultDTO |>
        dplyr::mutate("window_name" = !!tolower(names(window)[i]))
      if (!("days" %in% value)) {
        resultDTO <- dplyr::select(resultDTO, -"days")
      }
      if (i == 1) {
        resultDateTimeOther <- resultDTO |>
          dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
      } else {
        resultDateTimeOther <- resultDateTimeOther |>
          dplyr::union_all(resultDTO) |>
          dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
      }
    }
  }

  if (any(c("flag", "count") %in% value)) {
    values <- value[value %in% c("count", "flag")]
    resultCountFlagPivot <- resultCountFlag |>
      tidyr::pivot_wider(
        names_from = c("id_name", "window_name"),
        values_from = dplyr::any_of(values),
        names_glue = nameStyle,
        values_fill = 0
      ) |>
      dplyr::rename(!!indexDate := "index_date") |>
      dplyr::rename_all(tolower) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))

    newColCountFlag <- colnames(resultCountFlagPivot)
    newColCountFlag <- newColCountFlag[newColCountFlag %in% newCols$colnam]

    x <- x |>
      dplyr::left_join(
        resultCountFlagPivot,
        by = joinKey
      ) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))

    x <- x |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(newColCountFlag), ~ dplyr::if_else(is.na(.x), 0, .x)
      )) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
  }

  if (length(value[!(value %in% c("count", "flag"))]) > 0) {
    values <- value[!(value %in% c("count", "flag"))]

    if (length(extraValue) > 0 & !allowDuplicates) {
      duplicates <- resultDateTimeOther |>
        dplyr::select(
          dplyr::all_of(resultKey),
          dplyr::all_of(extraValue), "id_name", "window_name"
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(
          resultKey, "id_name", "window_name"
        )))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup() |>
        dplyr::tally() |>
        dplyr::pull()
      if (duplicates > 0) {
        cli::cli_abort(c(
          x = "There are {duplicates} row{?s} in {.strong {tableName}} with same
          {.var {c(personVariable, filterVariable, targetStartDate)}}, solve
          duplications or swicth {.pkg allowDuplicates} to TRUE.",
          i = "NOTE that `allowDuplicates = TRUE` can sort the values
          inconsistently depending on the cdm_source."
        ))
      }
    }

    resultDateTimeOther <- resultDateTimeOther |>
      dplyr::select(
        dplyr::all_of(resultKey), dplyr::all_of("value_as_concept"),
        "id_name", "window_name"
      ) |>
      tidyr::pivot_wider(
        names_from = c("id_name", "window_name"),
        values_from = dplyr::all_of("value_as_concept"),
        names_glue = nameStyle
      ) |>
      dplyr::rename(!!indexDate := "index_date") |>
      dplyr::rename_all(tolower)

    x <- x |>
      dplyr::left_join(
        resultDateTimeOther,
        by = joinKey
      ) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
  }

  # missing columns
  createMissingCols <- newCols |>
    dplyr::filter(!.data$colnam %in% colnames(x)) |>
    dplyr::pull("colnam") |>
    rlang::set_names() |>
    purrr::map_chr(\(x) {
      val <- as.character(newCols$value[newCols$colnam == x])
      switch(val,
             flag = "0",
             count = "0",
             days = "as.numeric(NA)",
             date = "as.Date(NA)",
             "as.character(NA)")
    }) |>
    rlang::parse_exprs()
  if (length(createMissingCols) > 0) {
    x <- x |>
      dplyr::mutate(!!!createMissingCols) |>
      dplyr::compute(name = omopgenerics::uniqueTableName(tablePrefix))
  }

  if (any(value %in% c("count", "flag"))) {
    if (length(qInObservation) > 0) {
      x <- x |>
        dplyr::left_join(individualsInObservation, by = c(personVariable, indexDate)) |>
        dplyr::mutate(!!!qInObservation) |>
        dplyr::select(!dplyr::all_of(idsObs))
    }
  }

  x <- removeMaterialisedIndexDate(x, indexDateInput)

  x <- .convertColumnType(
    x = x,
    columns = newCols$colnam,
    type = type
  )

  x <- x |>
    dplyr::compute(name = comp$name, temporary = comp$temporary)

  omopgenerics::dropSourceTable(
    cdm = cdm, name = dplyr::starts_with(tablePrefix)
  )

  return(x)
}

#' Get the name of the start date column for a certain table in the cdm
#'
#' @param tableName Name of the table.
#'
#' @return Name of the start date column in that table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' startDateColumn("condition_occurrence")
#' }
#'
startDateColumn <- function(tableName) {
  if (tableName %in% omopgenerics::omopTables()) {
    col <- omopgenerics::omopColumns(table = tableName, field = "start_date")
  } else {
    col <- "cohort_start_date"
  }
  return(col)
}

valueIdColumn <- function() {
  "value_as_concept_id"
}

#' Get the name of the end date column for a certain table in the cdm
#'
#' @param tableName Name of the table.
#'
#' @return Name of the end date column in that table.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' endDateColumn("condition_occurrence")
#' }
#'
endDateColumn <- function(tableName) {
  if (tableName %in% omopgenerics::omopTables()) {
    col <- omopgenerics::omopColumns(table = tableName, field = "end_date")
  } else {
    col <- "cohort_end_date"
  }
  return(col)
}

standardConceptIdColumn <- function(tableName) {
  if (tableName %in% omopgenerics::omopTables()) {
    col <- omopgenerics::omopColumns(table = tableName, field = "standard_concept")
  } else {
    col <- "cohort_definition_id"
  }
  return(col)
}

sourceConceptIdColumn <- function(tableName) {
  if (tableName %in% omopgenerics::omopTables()) {
    col <- omopgenerics::omopColumns(table = tableName, field = "source_concept")
  } else {
    col <- NA_character_
  }
  return(col)
}

messageOrder <- function(order) {
  cli::cli_inform(c("i" = "`order` argument is populated by default to
                    {.pkg {order}}, which means {order} ever record in the
                    window will be considered. Populate the argument explicitly
                    to silence this message."))
}

addCohortName <- function(cohort) {
  omopgenerics::assertClass(cohort, class = "cohort_table")

  if ("cohort_name" %in% colnames(cohort)) {
    cli::cli_inform(c("!" = "`cohort_name` will be overwrite"))
    cohort <- cohort |> dplyr::select(!"cohort_name")
  }
  cohort |>
    dplyr::left_join(
      attr(cohort, "cohort_set") |>
        dplyr::select("cohort_definition_id", "cohort_name"),
      by = "cohort_definition_id"
    )
}

addConceptName <- function(table,
                           column = NULL,
                           nameStyle = "{column}_name") {
  omopgenerics::assertClass(table, class = "cdm_table")
  if (is.null(column)) {
    column <- purrr::keep(colnames(table), \(col) endsWith(col, "concept_id"))
  }
  omopgenerics::assertCharacter(column)
  notPresent <- purrr::keep(column, \(col) !col %in% colnames(table))
  if (length(notPresent) > 0) {
    cli::cli_inform(c("!" = "{.var {notPresent}} ignored as not present in table."))
  }
  column <- purrr::keep(column, \(col) col %in% colnames(table))
  omopgenerics::validateNameStyle(nameStyle = nameStyle, column = column)
  cdm <- omopgenerics::cdmReference(table)
  eliminate <- glue::glue(nameStyle) |>
    as.character() |>
    purrr::keep(\(col) col %in% colnames(table))
  if (length(eliminate) > 0) {
    cli::cli_inform(c("!" = "{.var {eliminate}} will be overwriten."))
    table <- table |>
      dplyr::select(!dplyr::all_of(eliminate))
  }
  for (col in column) {
    cols <- c("concept_id", "concept_name") |>
      rlang::set_names(c(col, glue::glue(nameStyle, column = col)))
    table <- table |>
      dplyr::left_join(
        cdm$concept |>
          dplyr::select(dplyr::all_of(cols)),
        by = col
      )
  }

  table
}

addCdmName <- function(table, cdm = omopgenerics::cdmReference(table)) {
  name <- omopgenerics::cdmName(cdm)
  if ("cdm_name" %in% colnames(table)) {
    cli::cli_inform(c("!" = "`cdm_name` will be overwrite"))
  }
  table |> dplyr::mutate("cdm_name" = .env$name)
}

newTable <- function(name, call = parent.frame()) {
  omopgenerics::assertCharacter(name, length = 1, null = TRUE, na = TRUE, call = call)
  if (is.null(name) || is.na(name)) {
    x <- list(name = omopgenerics::uniqueTableName(), temporary = TRUE)
  } else {
    x <- list(name = name, temporary = FALSE)
  }
  return(x)
}
uniqueColumnName <- function(cols = character(), n = 1, nletters = 2) {
  x <- rep(list(letters), nletters) |>
    rlang::set_names(paste0("id_", seq_len(nletters)))
  tidyr::expand_grid(!!!x) |>
    tidyr::unite(col = "id", dplyr::starts_with("id_"), sep = "") |>
    dplyr::mutate("id" = paste0("id_", .data$id)) |>
    dplyr::filter(!.data$id %in% .env$cols) |>
    dplyr::sample_n(size = .env$n) |>
    dplyr::pull("id")
}
computeTable <- function(x, name) {
  if (is.null(name) || is.na(name)) {
    x <- x |>
      dplyr::compute(name = omopgenerics::uniqueTableName(), temporary = TRUE)
  } else {
    x <- x |>
      dplyr::compute(name = name, temporary = FALSE)
  }
  return(x)
}

.dateBuildQuery <- function(x, year, month, day) {
  if (inherits(x, "tbl_duckdb_connection")) {
    return(glue::glue("make_date({year}, {month}, {day})"))
  }

  invalid <- if (inherits(x, "data.frame")) {
    ", invalid = 'next'"
  } else {
    ""
  }

  glue::glue(
    "clock::date_build(year = {year}, month = {month}, day = {day}{invalid})"
  )
}

validateColumnType <- function(type,
                               column,
                               call = parent.frame()) {
  choices <- lapply(column, function(column) {
    switch(
      column,
      days = c("auto", "numeric", "integer"),
      count = c("auto", "numeric", "integer"),
      age = c("auto", "numeric", "integer"),
      observation = c("auto", "numeric", "integer"),
      flag = c("auto", "numeric", "integer", "logical"),
      date = "auto",
      c("auto", "numeric", "integer", "logical", "character")
    )
  })
  choices <- Reduce(intersect, choices)
  omopgenerics::assertChoice(type, choices, length = 1, call = call)
  return(type)
}

.convertColumnType <- function(x,
                               columns,
                               type) {
  columns <- columns %||% character()

  if (identical(type, "auto") || length(columns) == 0) {
    return(x)
  }

  switch(
    type,
    numeric = x |>
      dplyr::mutate(dplyr::across(dplyr::all_of(columns), as.numeric)),
    integer = x |>
      dplyr::mutate(dplyr::across(dplyr::all_of(columns), as.integer)),
    logical = x |>
      dplyr::mutate(dplyr::across(dplyr::all_of(columns), as.logical)),
    character = x |>
      dplyr::mutate(dplyr::across(dplyr::all_of(columns), as.character))
  )
}

materialiseIndexDate <- function(indexDate, x, null = FALSE,
                                 call = parent.frame()) {
  if (null) {
    return(list(x = x, indexDate = NULL, temporaryColumn = NULL))
  }

  if (inherits(indexDate, "Date")) {
    if (length(indexDate) != 1 || is.na(indexDate)) {
      cli::cli_abort(
        "indexDate must be a single non-missing date.",
        call = call
      )
    }
    temporaryColumn <- omopgenerics::uniqueId(exclude = colnames(x))
    x <- x |>
      dplyr::mutate(!!temporaryColumn := .env$indexDate)
    return(list(
      x = x,
      indexDate = temporaryColumn,
      temporaryColumn = temporaryColumn
    ))
  }

  list(
    x = x,
    indexDate = validateIndexDate(
      indexDate = indexDate, null = FALSE, x = x, call = call
    ),
    temporaryColumn = NULL
  )
}
validateIndexDate <- function(indexDate, null, x, call) {
  if (null) {
    return(NULL)
  }
  omopgenerics::assertCharacter(indexDate, length = 1, call = call)
  if (!indexDate %in% colnames(x)) {
    cli::cli_abort("indexDate must be a column in x.", call = call)
  }
  xx <- x |>
    dplyr::select(dplyr::all_of(indexDate)) |>
    utils::head(1) |>
    dplyr::pull()
  if (!inherits(xx, "Date") && !inherits(xx, "POSIXt")) {
    cli::cli_abort("x[[{indexDate}]] is not a date column.", call = call)
  }
  return(indexDate)
}

warnOverwriteColumns <- function(x, nameStyle, values = list()) {
  if (length(values) > 0) {
    nameStyle <- tidyr::expand_grid(!!!values) |>
      dplyr::mutate("tmp_12345" = glue::glue(.env$nameStyle)) |>
      dplyr::pull("tmp_12345") |>
      as.character() |>
      unique()
  }
}
checkVariableInX <- function(indexDate, x, nullOk = FALSE, name = "indexDate", call = parent.frame()) {
  omopgenerics::assertCharacter(indexDate, length = 1, null = nullOk, call = call)
  if (!is.null(indexDate) && !(indexDate %in% colnames(x))) {
    cli::cli_abort(glue::glue("{name} ({indexDate}) should be a column in x"), call = call)
  }
  invisible(NULL)
}

checkFilter <- function(filterVariable, filterId, idName, x,
                        call = parent.frame()) {
  if (is.null(filterVariable)) {
    filterId <- NULL
    idName <- NULL
    filterTbl <- NULL
  } else {
    checkVariableInX(
      filterVariable, x, FALSE, "filterVariable", call = call
    )
    omopgenerics::assertNumeric(filterId, na = FALSE, call = call)
    omopgenerics::assertNumeric(utils::head(x, 1) |>
                               dplyr::pull(dplyr::all_of(filterVariable)),
                               call = call)
    if (is.null(idName)) {
      idName <- paste0("id", filterId)
    } else {
      omopgenerics::assertCharacter(idName,
                                    na = FALSE,
                                    length = length(filterId),
                                    call = call)
    }
    filterTbl <- dplyr::tibble(
      id = filterId,
      id_name = idName
    )
  }
  invisible(filterTbl)
}

intersectOptions <- c("flag", "count", "date", "days")

checkValue <- function(value, x, name, call) {
  omopgenerics::assertCharacter(value, na = FALSE, call = call)
  omopgenerics::assertTrue(all(value %in% c(intersectOptions, colnames(x))), call = call)
  valueOptions <- intersectOptions[intersectOptions %in% colnames(x)]
  if (length(valueOptions) > 0) {
    cli::cli_warn(paste0(
      "Variables: ",
      paste0(valueOptions, collapse = ", "),
      " are also present in ",
      name,
      ". But have their own functionality inside the package. If you want to
      obtain that column please rename and run again."
    ))
  }
  invisible(value[!(value %in% intersectOptions)])
}

checkCohortNames <- function(x, targetCohortId, name) {
  if (!("cohort_table" %in% class(x))) {
    cli::cli_abort("cdm[[targetCohortTable]]) must be a 'cohort_table'.")
  }
  targetCohortId <- omopgenerics::validateCohortIdArgument(
    cohortId = {{targetCohortId}}, cohort = x
  )
  set <- omopgenerics::settings(x) |>
    dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  parameters <- list(
    "filter_variable" = "cohort_definition_id",
    "filter_id" = set$cohort_definition_id,
    "id_name" = set$cohort_name
  )
  invisible(parameters)
}

checkStrata <- function(list, table, type = "strata") {
  errorMessage <- paste0(type, " should be a list that point to columns in table")
  if (!is.list(list)) {
    cli::cli_abort(errorMessage)
  }
  if (length(list) > 0) {
    if (!is.character(unlist(list))) {
      cli::cli_abort(errorMessage)
    }
    if (!all(unlist(list) %in% colnames(table))) {
      notPresent <- list |>
        unlist() |>
        unique()
      notPresent <- notPresent[!notPresent %in% colnames(table)]
      cli::cli_abort(paste0(
        errorMessage,
        ". The following columns were not found in the data: ",
        paste0(notPresent, collapse = ", ")
      ))
    }
  }
  if (!is.null(names(list))) {
    cli::cli_inform(c("!" = "names of {type} will be ignored"))
  }
  names(list) <- NULL
  return(list)
}

checkVariablesFunctions <- function(variables, estimates, table, weights = NULL,
                                    customEstimates = list()) {
  errorMessage <- "variables should be a unique named list that point to columns in table"

  # default variables
  if (is.null(variables)) {
    variables <- colnames(table)
    variables <- variables[!grepl("_id", variables) & !variables %in% weights]
  }
  if (!is.list(variables)) {
    variables <- list(variables)
  }

  # default estimates
  if (is.null(estimates)) {
    types <- table |>
      dplyr::select(dplyr::all_of(unique(unlist(variables)))) |>
      variableTypes() |>
      dplyr::group_by(.data$variable_name) |>
      dplyr::group_split() |>
      unclass()
    variables <- types |>
      purrr::map(\(x) unique(x$variable_name))
    estimates <- types |>
      purrr::map(\(x) {
        typ <- unique(x$variable_type)
        nm <- unique(x$variable_name)
        if (typ == "date") {
          est <- c("min", "q25", "median", "q75", "max")
        } else if (typ %in% c("integer", "numeric")) {
          u <- table |>
            dplyr::select(dplyr::all_of(nm)) |>
            dplyr::distinct() |>
            utils::head(4L) |>
            dplyr::pull()
          if (length(u) <= 3) {
            u <- as.character(u)
            binary <- all(u %in% c("0", "1", NA_character_))
          } else {
            binary <- FALSE
          }
          if (binary) {
            est <- c("min", "q25", "median", "q75", "max", "count", "percentage")
          } else {
            est <- c("min", "q25", "median", "q75", "max")
          }
        } else if (typ %in% c("logical", "categorical")) {
          est <- c("count", "percentage")
        }
        est
      })
  }
  if (!is.list(estimates)) {
    estimates <- list(estimates)
  }

  omopgenerics::assertList(x = variables, class = "character")
  omopgenerics::assertList(x = estimates, class = "character")
  types <- variableTypes(table)
  if (length(variables) == 1 & is.null(names(variables)) & !is.null(names(estimates)) & length(estimates) != 1) {
    variables <- types |>
      dplyr::filter(.data$variable_name %in% unlist(variables)) |>
      dplyr::group_by(.data$variable_type) |>
      dplyr::group_split() |>
      unclass()
    names(variables) <- purrr::map(variables, \(x) unique(x$variable_type))
    variables <- purrr::map(variables, \(x) unique(x$variable_name))
    estimates <- estimates[names(variables)]
  }
  if (length(variables) != length(estimates)) {
    cli::cli_abort("Variables and estimates must have the same length")
  }
  if (!is.null(names(variables)) & !is.null(names(estimates))) {
    if (!identical(sort(names(variables)), sort(names(estimates)))) {
      cli::cli_abort("Names from variables and estimates must be the same")
    }
    variables <- variables[order(names(variables))]
    estimates <- estimates[order(names(estimates))]
  }

  if (length(variables) == 0) {
    return(dplyr::tibble(
      "variable_name" = character(),
      "estimate_name" = character(),
      "variable_type" = character(),
      "estimate_type" = character()
    ))
  }

  estimateFormats <- availableEstimates(fullQuantiles = TRUE) |>
    dplyr::select(-"estimate_description") |>
    dplyr::bind_rows(tidyr::expand_grid(
      variable_type = unique(types$variable_type),
      estimate_name = names(customEstimates),
      estimate_type = "numeric"
    ))

  functions <- lapply(seq_along(variables), function(k) {
    tidyr::expand_grid(
      variable_name = variables[[k]],
      estimate_name = estimates[[k]]
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::inner_join(types, by = "variable_name") |>
    dplyr::inner_join(
      estimateFormats,
      by = c("variable_type", "estimate_name")
    )

  if (length(weights) > 0) {
    functions <- functions |>
      dplyr::mutate(estimate_type = dplyr::if_else(
        .data$estimate_type == "integer" & grepl("count|sum", .data$estimate_name),
        "numeric",
        .data$estimate_type
      ))
  }

  vars <- functions |>
    dplyr::filter(
      .data$variable_type %in% c("integer", "numeric") &
        .data$estimate_name %in% c("count", "percentage")
    ) |>
    dplyr::pull("variable_name") |>
    unique()
  if (length(vars) > 0) {
    vars <- vars |>
      purrr::keep(\(x) {
        labs <- table |>
          dplyr::select(dplyr::all_of(x)) |>
          dplyr::distinct() |>
          utils::head(4L) |>
          dplyr::pull() |>
          as.character()
        all(labs %in% c("0", "1", NA_character_))
      })
    functions <- functions |>
      dplyr::filter(
        .data$variable_name %in% .env$vars |
          !.data$estimate_name %in% c("count", "percentage") |
          !.data$variable_type %in% c("integer", "numeric")
      )
  }

  return(functions)
}

checkCustomEstimates <- function(customEstimates) {
  if (is.null(customEstimates)) {
    return(list())
  }
  if (!is.list(customEstimates)) {
    cli::cli_abort("{.arg customEstimates} must be a named list of functions.")
  }
  if (length(customEstimates) == 0) {
    return(list())
  }
  if (is.null(names(customEstimates)) ||
      any(names(customEstimates) == "") ||
      anyDuplicated(names(customEstimates))) {
    cli::cli_abort(
      "{.arg customEstimates} must have unique, non-empty names."
    )
  }
  if (!all(vapply(customEstimates, is.function, logical(1)))) {
    cli::cli_abort("Every {.arg customEstimates} element must be a function.")
  }
  if (any(vapply(customEstimates, \(fun) length(estimateFormals(fun)) == 0,
                 logical(1)))) {
    cli::cli_abort(
      "Every custom estimate function must have at least one argument."
    )
  }
  builtIn <- union(
    availableEstimates(fullQuantiles = TRUE)$estimate_name,
    names(estimatesFunc)
  )
  conflicts <- intersect(names(customEstimates), builtIn)
  if (length(conflicts) > 0) {
    cli::cli_abort(c(
      "Custom estimate names cannot overwrite built-in estimates.",
      "x" = "Conflicting name{?s}: {conflicts}."
    ))
  }
  customEstimates
}

estimateFormals <- function(fun) {
  fmls <- formals(fun)
  if (is.null(fmls)) {
    fmls <- formals(args(fun))
  }
  fmls
}

checkCensorDate <- function(x, censorDate, call = parent.frame()) {
  check <- x |>
    dplyr::select(dplyr::all_of(censorDate)) |>
    utils::head(1) |>
    dplyr::pull() |>
    inherits("Date")
  if (!check) {
    cli::cli_abort("{censorDate} is not a date variable", call = call)
  }

  hasMissing <- x |>
    dplyr::filter(is.na(.data[[censorDate]])) |>
    utils::head(1) |>
    dplyr::collect() |>
    nrow() > 0

  if (hasMissing) {
    cli::cli_abort(
      "{censorDate} cannot contain missing values when used as censorDate.",
      call = call
    )
  }
}

correctStrata <- function(strata, overall) {
  if (length(strata) == 0 | overall) {
    strata <- c(list(character()), strata)
  }
  strata <- unique(strata)
  return(strata)
}

assertNameStyle <- function(nameStyle,
                            values = list(),
                            call = parent.frame()) {
  omopgenerics::assertCharacter(nameStyle, length = 1,
                                na = FALSE, minNumCharacter = 1, call = call)
  omopgenerics::assertList(values, named = TRUE)
  omopgenerics::assertClass(call, class = "environment")
  err <- character()
  for (k in seq_along(values)) {
    valk <- values[[k]]
    nm <- paste0("\\{", names(values)[k], "\\}")
    if (length(valk) > 1 & !grepl(pattern = nm, x = nameStyle)) {
      err <- c(err, paste0("{{", names(values)[k], "}}"))
    }
  }
  if (length(err) > 0) {
    names(err) <- rep("*", length(err))
    cli::cli_abort(
      message = c("The following elements are not present in nameStyle:", err),
      call = call
    )
  }

  return(invisible(nameStyle))
}

warnOverwriteColumns <- function(x, nameStyle, values = list()) {
  if (length(values) > 0) {
    nameStyle <- tidyr::expand_grid(!!!values) |>
      dplyr::mutate("tmp_12345" = glue::glue(.env$nameStyle)) |>
      dplyr::pull("tmp_12345") |>
      as.character() |>
      unique()
  }

  extraColumns <- colnames(x)[colnames(x) %in% nameStyle]
  if (length(extraColumns) > 0) {
    ms <- extraColumns
    names(ms) <- rep("*", length(ms))
    cli::cli_inform(message = c(
      "!" = "The following columns will be overwritten:", ms
    ))
    x <- x |> dplyr::select(!dplyr::all_of(extraColumns))
  }

  return(x)
}

validateIndexDate <- function(indexDate, null, x, call) {
  if (null) {
    return(NULL)
  }
  omopgenerics::assertCharacter(indexDate, length = 1, call = call)
  if (!indexDate %in% colnames(x)) {
    cli::cli_abort("indexDate must be a column in x.", call = call)
  }
  xx <- x |>
    dplyr::select(dplyr::all_of(indexDate)) |>
    utils::head(1) |>
    dplyr::pull()
  if (!inherits(xx, "Date") && !inherits(xx, "POSIXt")) {
    cli::cli_abort("x[[{indexDate}]] is not a date column.", call = call)
  }
  return(indexDate)
}

materialiseIndexDate <- function(indexDate, x, null = FALSE,
                                 call = parent.frame()) {
  if (null) {
    return(list(x = x, indexDate = NULL, temporaryColumn = NULL))
  }

  if (inherits(indexDate, "Date")) {
    if (length(indexDate) != 1 || is.na(indexDate)) {
      cli::cli_abort(
        "indexDate must be a single non-missing date.",
        call = call
      )
    }
    temporaryColumn <- omopgenerics::uniqueId(exclude = colnames(x))
    x <- x |>
      dplyr::mutate(!!temporaryColumn := .env$indexDate)
    return(list(
      x = x,
      indexDate = temporaryColumn,
      temporaryColumn = temporaryColumn
    ))
  }

  list(
    x = x,
    indexDate = validateIndexDate(
      indexDate = indexDate, null = FALSE, x = x, call = call
    ),
    temporaryColumn = NULL
  )
}

removeMaterialisedIndexDate <- function(x, indexDateInput) {
  if (!is.null(indexDateInput$temporaryColumn)) {
    x <- x |>
      dplyr::select(!dplyr::any_of(indexDateInput$temporaryColumn))
  }
  x
}
validateColumn <- function(col, null = FALSE, call = parent.frame()) {
  if (null) {
    return(NULL)
  }

  nm <- paste0(substitute(col))

  err <- "{nm} must be a snake_case character string"
  if (!is.character(col)) cli::cli_abort(message = err, call = call)
  if (length(col) != 1) cli::cli_abort(message = err, call = call)
  if (is.na(col)) cli::cli_abort(message = err, call = call)

  scCol <- omopgenerics::toSnakeCase(col)

  if (scCol != col) {
    cli::cli_warn(
      c("!" = "{nm} has been modified to be snake_case, {col} -> {scCol}"),
      call = call
    )
  }

  return(scCol)
}
validateAgeMissingMonth <- function(ageMissingMonth, null, call) {
  if (null) {
    return(ageMissingMonth)
  }

  if (is.character(ageMissingMonth)) {
    ageMissingMonth <- as.numeric(ageMissingMonth)
  }
  omopgenerics::assertNumeric(ageMissingMonth, integerish = TRUE, min = 1, max = 12, call = call)
  ageMissingMonth <- as.integer(ageMissingMonth)

  return(ageMissingMonth)
}
validateAgeMissingDay <- function(ageMissingDay, null, call) {
  if (null) {
    return(ageMissingDay)
  }

  if (is.character(ageMissingDay)) {
    ageMissingDay <- as.numeric(ageMissingDay)
  }
  omopgenerics::assertNumeric(ageMissingDay, integerish = TRUE, min = 1, max = 31, call = call)
  ageMissingDay <- as.integer(ageMissingDay)

  return(ageMissingDay)
}
validateMissingValue <- function(x, null, call) {
  if (null) {
    return(NULL)
  }
  nm <- paste0(substitute(x))
  err <- "{nm} must be a character of length 1." |> rlang::set_names("!")
  if (!is.character(x)) cli::cli_abort(message = err, call = call)
  if (length(x) != 1) cli::cli_abort(message = err, call = call)
  return(x)
}
validateType <- function(x, null, call) {
  if (null) { 
    return(NULL)
  }
  nm <- paste0(substitute(x))
  err <- "{nm} must be a choice between 'date' or 'days'." |>
    rlang::set_names("!")
  if (!is.character(x)) cli::cli_abort(message = err, call = call)
  if (length(x) != 1) cli::cli_abort(message = err, call = call)
  if (!x %in% c("date", "days")) cli::cli_abort(message = err, call = call)
  return(x)
}
validateName <- function(name, call = parent.frame()) {
  omopgenerics::assertCharacter(name, length = 1, null = TRUE, call = call)
}
