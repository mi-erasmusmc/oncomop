addWindow <- function(
  summarisedResult,
  window
) {
  win_1 <- window[[1]][[1]] 
  win_2 <- window[[1]][[2]]

  format_window_bound <- function(value) {
    if (is.infinite(value)) {
      return(as.character(value))
    }
    format(
      value,
      scientific = FALSE,
      trim = TRUE
    )
  }

  windowInCharacter <- sprintf(
    "%s to %s days",
    format_window_bound(win_1),
    format_window_bound(win_2)
  )
  summarisedResult |> 
    omopgenerics::splitAdditional() |> 
    dplyr::mutate(
      window = windowInCharacter
    )  |> 
    omopgenerics::uniteAdditional(
      cols = "window" 
    )
}
assertCharacteristic <- function(x, characteristics) {
  checkmate::assertCharacter(x)
  checkmate::assertCharacter(characteristics)
  invalidCharacteristics <- setdiff(
    x,
    characteristics
  )
  if (length(invalidCharacteristics) > 0) {
    cli::cli_abort(c(
      "!" = "Invalid characteristics: {invalidCharacteristics}",
      "i" = "Provide any of this specific characteristics: are:\n{paste0('- ', characteristics, collapse = '\n')}",
      "x" = "Your selected characteristic(s) is/are either misspelled or unavailable."
    ),
    class = "Invalid characteristics")
  } else {
    return(invisible())
  }
}

filterCodelist <- function(
  codelist,
  pattern
) {
  codelist |> 
    omopgenerics::assertList(
      named = TRUE
    )
  pattern |> 
    checkmate::assertCharacter()
  index <- stringr::str_detect(
    names(codelist),
    pattern = pattern
  )
  codelist[index]
}

conceptSetsPath <- function() {
    system.file(
      "concept_sets",
    package = "P4C5006"
  )
}
#' Get a 90-day window around index
#'
#' @returns A list with one window `c(-90, 90)`.
#' @keywords internal
window90Days <- function() {
    window <- list(
      c(-90, 90)
    )
  return(window)
}
#' Get rolling 30-day windows
#'
#' Builds windows from day `-90` to `1825` relative to index date.
#'
#' @returns A list of integer windows.
#' @keywords internal
windows30Days <- function() {
  starts <- seq(-90, 1825, by = 30)
  ends <- pmin(starts + 29, 1825)
  lapply(seq_along(starts), function(i) c(starts[i], ends[i])) 
}
#' Zip study result files
#'
#' Creates a zip archive for a study result folder.
#'
#' @param outputDir Character path to the base output directory.
#' @param resultsDirName Character name or path to the results subfolder.
#' @param dbname Character database identifier.
#'
#' @returns Character file name of the generated zip archive.
#' @keywords internal
zipStudyFiles <- function(
    outputDir,
    resultsDirName,
    dbname
) {
  outputDir <- normalizePath(outputDir)
  checkmate::assertDirectoryExists(outputDir)
  resultsDirName <- basename(resultsDirName)
  studyGenerics::assertCdmNames(dbname)
  cli::cli_alert_info(
    "Exporting results to zip format"
    )
  zipFileName <- glue::glue(
      "results_{dbname}_{format(Sys.Date(), format='%Y%m%d')}.zip"
    )
  zipFileNamePath <- file.path(
      resultsDirName,
      zipFileName
    )
  zip::zip(
    zipfile = zipFileNamePath,
    files = resultsDirName,
    root = outputDir
    )
  checkmate::assertFileExists(
    file.path(
      outputDir,
      zipFileNamePath
    )
  )
  cli::cli_alert_success(
    glue::glue(
      "Results exported to {file.path(outputDir, zipFileNamePath)}"
    )
  )
  return(zipFileName)
}

#' Unzip study result files
#'
#' Finds zip files under `path` and extracts them to `outputDir`.
#'
#' @param path Character directory where zip files are searched.
#' @param pattern Optional regular expression used to filter zip file paths.
#' @param negate Logical passed to `stringr::str_detect()`.
#' @param recursive Logical indicating whether to search recursively.
#' @param outputDir Character directory where files will be unzipped.
#'
#' @returns `NULL`, called for its side effects.
#' @keywords internal
unZipStudyFiles <- function(
    path,
    pattern = NULL,
    negate = TRUE,
    recursive = FALSE,
    outputDir
    ) {

  checkmate::assertDirectoryExists(path)
  checkmate::assertLogical(negate)
  checkmate::assertLogical(recursive)

  zip_files <- list.files(
    path = path,
    pattern = ".zip",
    full.names = TRUE,
    recursive = recursive
    )

  index_files <- stringr::str_detect(
    zip_files,
    pattern = pattern,
    negate = negate
    )

  zip_files_filtered <- zip_files[index_files]

  if (!dir.exists(outputDir)) {
    dir.create(outputDir)
  }

  for (i in 1:length(zip_files)) {
    zip::unzip(
      zip_files[i],
      exdir = outputDir
      )
  }

  ParallelLogger::logInfo(
    glue::glue(
      "Files unzipped to: {outputDir}"
      )
    )
}


#' Get ingredient concept IDs
#'
#' Returns the predefined ingredient IDs used by
#' `DrugExposureDiagnostics::executeChecks()`.
#'
#' @param test Logical. If `TRUE`, returns a short test subset.
#'
#' @returns Integer vector of concept IDs.
#' @keywords internal
getIngredients <- function(test = FALSE) {

  if (isFALSE(test)) {
    return(
      c(19010868, 792649, 40239056, 1301337, 43533090, 40244266, 1309770,
        35604657, 1396423, 1503057, 1536935, 36854863, 35198076, 1348265,
        963987, 42629079, 36861628, 1593273, 19023835, 19013730, 19086176,
        36850058, 1397141, 1344381, 1559930, 1594187, 19033280, 40222431,
        43012292, 36853469, 1337620, 747200, 1145637, 1344905, 1350066,
        35200783, 44818466, 1315411, 35884379, 1390051, 1397599, 35605804,
        40242675, 1310317, 19010792, 43532299, 1311409, 35200803, 1361291,
        1358436, 1254111, 1465264, 19058410, 1518254, 1525866, 1315942,
        1536789, 1338512, 1594034, 1301646, 1559914, 37498261, 1466160,
        37496447, 42900250, 1344354, 1511250, 40230712, 1325363, 1548195,
        1349025, 1549786, 1350504, 19011440, 1398399, 955632, 1356461,
        19020079, 35834910, 747237, 1304044, 36855491, 1319193, 1314924,
        36878778, 1366310, 1366773, 19073699, 975125, 1377141, 35884401,
        19078187, 1304107, 36855119, 1735491, 1380068, 40238188, 1367268,
        19025348, 985708, 1359548, 1355705, 1735080, 46221433, 1315946,
        1388796, 1351541, 1389464, 40168303, 42609269, 1391846, 35201733,
        739518, 1300978, 1301267, 1305058, 1389036, 1309188, 701915,
        746328, 35606215, 35198201, 793846, 1394023, 1315286, 45775396,
        1593861, 45892628, 36853973, 1734398, 45892579, 35605522, 36878777,
        1318011, 1378382, 45892075, 19100985, 1714165, 45775965, 1304919,
        1466158, 42801287, 37002765, 1551099, 1351779, 36856361, 902727,
        19038536, 44818489, 42903460, 779239, 739471, 747362, 1302024,
        1592911, 1145752, 1718850, 1145484, 1308432, 1145791, 36853363,
        36861273, 36854475, 40224095, 1536963, 36854773, 1253903, 35201068,
        36862664, 1436678, 1734429, 19056756, 1466283, 1341149, 19136750,
        739739, 19137385, 35602757, 42609339, 1378509, 1342346, 747052,
        43532497, 1387104, 741851, 905078, 1343039, 1145571, 19012543,
        40238052, 40241937, 19008264, 1308290, 19008336, 36878852, 1343346,
        1464913, 1735539, 1253620)
      )
    } else {
      return(
        c(19010868, 792649, 40239056, 43533090, 40244266, 1309770)
      )
    }
}

#' Create directory for analysis results
#'
#' @description
#' Creates a subdirectory for results based on the database name within a
#' specified output directory. If the output directory is not provided, the
#' current working directory is used.
#'
#' @param outputDir Character string. The path to the main output folder.
#'                  If NULL, defaults to the current working directory.
#' @param dbname Character string. The name of the database, used to suffix
#'               the results folder (e.g., "results_dbname").
#'
#' @returns A list containing three elements:
#' \itemize{
#'   \item \code{outputDir}: The path to the main output directory.
#'   \item \code{resultsDir}: The path to the specific results subdirectory created.
#'   \item \code{resultsDirName}: The name of the results subdirectory.
#' }
#'
#' @importFrom ParallelLogger logInfo
#' @importFrom checkmate assertDirectoryExists
#' @importFrom glue glue
#'
#' @export
createResultsDir <- function(outputDir, dbname) {
  # Set folder location for results ----
  ParallelLogger::logInfo("Setting location for results")
  if (is.null(outputDir)) {
    outputDir <- getwd()
    checkmate::assertDirectoryExists(outputDir)
  } else {
    if (!dir.exists(outputDir)) {
      dir.create(outputDir)
      checkmate::assertDirectoryExists(outputDir)
    } else {
      checkmate::assertDirectoryExists(outputDir)
    }
  }

  resultsDirName <- glue::glue("results_{dbname}")

  resultsDir <- file.path(
    outputDir,
    resultsDirName
    )

  if (!dir.exists(resultsDir)) {
    dir.create(resultsDir)
  }

  checkmate::assertDirectoryExists(resultsDir)
  return(
    list(
      outputDir = outputDir,
      resultsDir = resultsDir,
      resultsDirName = resultsDirName
      )
    )
}
