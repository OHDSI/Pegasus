# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Pegasus
#
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


#' Create cohort definitions using templates
#'
#' @description
#' Uses templates to generate cohort definitions for phenotypes.
#'
#' This requires that the phenotype meta data and concept sets have already been generated and uploaded to the Pegasus
#' database.
#'
#' @template Phenotypes
#' @template PegasusConnectionDetails
#' @template DatabaseReference
#' @template MaxCores
#' @param workFolder A folder where data objects are stored.
#'
#' @returns
#' A tibble with multiple rows per phenotype.
#'
#' @export
generateCohortDefinitionssUsingTemplates <- function(phenotypes,
                                                     pegasusConnection = NULL,
                                                     pegasusConnectionDetails = NULL,
                                                     pegasusDatabaseSchema,
                                                     databaseReference,
                                                     maxCores = 4,
                                                     workFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertClass(pegasusConnection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertClass(databaseReference, "DatabaseReference", add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(pegasusConnection) && is.null(pegasusConnectionDetails)) {
    stop("Must provide either pegasusConnection or pegasusConnectionDetails")
  }

  startTime <- Sys.time()

  ParallelLogger::addDefaultFileLogger(file.path(workFolder, "log.txt"), name = "TEMPLATES_LOGGER")
  on.exit(ParallelLogger::unregisterLogger("TEMPLATES_LOGGER"))

  if (is.null(pegasusConnection)) {
    pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(pegasusConnection), add = TRUE)
  }

  if (!dir.exists(workFolder)) {
    dir.create(workFolder, recursive = TRUE)
  }

  unionsFolder <- file.path(workFolder, "conceptSetUnions")
  dir.create(unionsFolder, showWarnings  = FALSE)

  phenotypes <- addPhenotypeIdIfMissing(
    data = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

  phenotypeConfigs <- buildPhenotypeConfigs(
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

  downloadAndUnionConceptSets(
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    databaseReference = databaseReference,
    maxCores = maxCores,
    unionsFolder = unionsFolder
  )

  cohortDefinitions <- list()
  for (i in seq_len(nrow(phenotypes))) {
    phenotype <- phenotypes[i, ]

    definitionsForPhenotype <- withCache({
      phenotypeConfig <- phenotypeConfigs[[as.character(phenotype$phenotypeId)]]

      bucketFiles <- list.files(unionsFolder, pattern = gsub("[^[:alnum:]]+", "", phenotype$phenotypeName))
      bucketNames <- gsub("^.*_", "", gsub(".json$", "", bucketFiles))
      bucketCs <- lapply(seq_along(bucketFiles), function(j) Capr::readConceptSet(
        path = file.path(unionsFolder, bucketFiles[j]),
        name = bucketNames[j])
      )
      names(bucketCs) <- bucketNames

      templateCds <- PhenotypeTemplates::buildPhenotypeTemplates(
        cs_I            = bucketCs[["I"]],
        cs_H            = bucketCs[["H"]],
        cs_S            = bucketCs[["S"]],
        cs_D            = bucketCs[["D"]],
        cs_T            = bucketCs[["T"]],
        cs_C            = bucketCs[["C"]],
        cs_A            = bucketCs[["A"]],
        cs_E            = bucketCs[["E"]],
        phenotypeLabel  = phenotype$phenotypeName,
        config          = phenotypeConfig,
        startCohortId   = 1
      )

      tibble(
        phenotypeName = phenotype$phenotypeName,
        cohortDefinitionName = templateCds$cohortName,
        circeCohortExpression = templateCds$json,
        caprCohortExpression = sapply(templateCds$json, circeToCaprCohortDefinition),
        cdSourceType = "Templates"
      )
    },
    cacheFolder = workFolder,
    fileName = sprintf("%s.csv", gsub("[^[:alnum:]]+", "", phenotype$phenotypeName))
    )

    cohortDefinitions[[i]] <- definitionsForPhenotype
  }
  cohortDefinitions <- bind_rows(cohortDefinitions)

  delta <- Sys.time() - startTime
  message(sprintf("Creating templated cohort definitions counts took %0.2f %s.",
                  delta,
                  attr(delta, "units")))
  return(cohortDefinitions)
}

buildPhenotypeConfigs <- function(phenotypes,
                                  pegasusConnection,
                                  pegasusDatabaseSchema) {
  message("Configuring phenotype meta data")

  phenotypeMetaData <- downloadPhenotypeMetaData(
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

  # Check if we have metadata for all phenotypes
  missingPhenotypes <- phenotypes |>
    filter(!.data$phenotypeId %in% phenotypeMetaData$phenotypeId)
  if (nrow(missingPhenotypes) > 0) {
    stop(sprintf("Missing phenotype metadata for '%s'. Run generatePhenotypeMetaData() and uploadPhenotypeMetaData() first.",
                 paste(missingPhenotypes$phenotypeName, collapse = "', '")))
  }

  clinicalCourseLookup <- c(
    "PERSISTENT_STABLE"    = "persistent_stable",
    "PERSISTENT_RELAPSING" = "persistent_recurrent",
    "TRANSIENT_RECURRENT"  = "transient_recurrent",
    "TRANSIENT_SINGLE"     = "transient_single"
  )
  expectedCareSettingLookup <- c(
    "ACUTE_CARE_COMMON"   = "acute_care_common",
    "ACUTE_CARE_EXPECTED" = "acute_care_expected",
    "OUTPATIENT_EXPECTED" = "outpatient_expected"
  )
  recommendedCohortExitLookup <- c(
    "END_OF_CONTINUOUS_OBSERVATION"   = "continuous_observation",
    "END_OF_ERA_WITH_PERSISTENCE_GAP" = "era_persistence",
    "FIXED_WINDOW_FROM_INDEX"         = "fixed_window"
  )
  str_to_logical <- function(x) {
    if (is.na(x) || x == "" || tolower(x) == "null") return(FALSE)
    tolower(x) %in% c("true", "yes", "t", "y", "1")
  }
  str_to_numeric <- function(x) {
    if (is.na(x) || x == "" || tolower(x) == "null") return(NULL)
    tryCatch(as.numeric(x), warning = function(w) NULL, error = function(e) NULL)
  }
  parse_sex_constraint <- function(sex_str) {
    sex_str <- toupper(trimws(sex_str))
    list(male = sex_str == "MALE_ONLY", female = sex_str == "FEMALE_ONLY")
  }

  phenotypeConfigs <- list()
  for (i in seq_len(nrow(phenotypeMetaData))) {
    row <- phenotypeMetaData[i, ] |>
      SqlRender::camelCaseToSnakeCaseNames()
    sexConstraint <- parse_sex_constraint(row$sex_constraint)
    clinicalCourse <- unname(clinicalCourseLookup[toupper(trimws(row$clinical_course))])
    occurrenceSettings <- PhenotypeTemplates:::inferPhenotypeOccurrenceSettings(clinicalCourse)

    phenotypeConfigs[[i]] <- PhenotypeTemplates::phenotypeConfig(
      clinicalCourse = clinicalCourse,
      expectedCareSetting = unname(expectedCareSettingLookup[toupper(trimws(row$expected_care_setting))]),
      minAge = str_to_numeric(row$min_age_years),
      maxAge = str_to_numeric(row$max_age_years),
      male = sexConstraint$male,
      female = sexConstraint$female,
      minimumInterepisodeDayGap = str_to_numeric(row$minimum_interepisode_gap_days),
      recommendedCohortExit = if (!is.na(row$recommended_cohort_exit) && row$recommended_cohort_exit != "")
        unname(recommendedCohortExitLookup[toupper(trimws(row$recommended_cohort_exit))]) else NULL,
      fixedExitDays = str_to_numeric(row$recommended_exit_days),
      hasDescreteRecordedSymptoms = str_to_logical(row$has_discrete_recorded_symptoms),
      requiresDiagnosticTestOrProcedure = str_to_logical(row$requires_diagnostic_test_or_procedure),
      requiresActiveTreatmentWithin30d = str_to_logical(row$requires_active_treatment_within_30d),
      expectsConditionSpecificFollowupOrSequelae1yr = str_to_logical(row$expects_condition_specific_followup_or_sequelae_1yr),
      firstOccurrenceOnly = occurrenceSettings$firstOccurrenceOnly,
      primaryCriteriaLimit = occurrenceSettings$primaryCriteriaLimit,
      expressionLimit = occurrenceSettings$expressionLimit,
      hospitalVisitOverlapWindow = occurrenceSettings$hospitalVisitOverlapWindow
    )
  }
  names(phenotypeConfigs) <- phenotypeMetaData$phenotypeId
  return(phenotypeConfigs)
}

downloadAndUnionConceptSets <- function(phenotypes,
                                        pegasusConnection,
                                        pegasusDatabaseSchema,
                                        databaseReference,
                                        maxCores,
                                        unionsFolder) {
  message("Downloading and unioning concept sets")
  phenotypeToConceptSets <- downloadPhenotypeToConceptSets(
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

  # Check if we have concept sets for all phenotypes
  missingPhenotypes <- phenotypes |>
    filter(!.data$phenotypeId %in% unique(phenotypeToConceptSets$phenotypeId))
  if (nrow(missingPhenotypes) > 0) {
    stop(sprintf("Missing concept sets for phenotypes '%s'. Run generateConceptSets() and uploadConceptSets() first.",
                 paste(missingPhenotypes$phenotypeName, collapse = "', '")))
  }

  categoryToBucket <- c(
    "Disease of interest" = "I",
    "Hypernym" = "H",
    "Clinical presentation" = "S",
    "Diagnostic procedure" = "D",
    "Measurement" = "D",
    "Drug" = "T",
    "Treatment procedure" = "T",
    "Etiology" = "E",
    "Alternative diagnosis" = "A",
    "Progression complication" = "C"
  )

  phenotypeToConceptSets <- phenotypeToConceptSets |>
    filter(.data$category != "Risk factor") |>
    mutate(bucket = unname(categoryToBucket[.data$category]))

  groups <- phenotypeToConceptSets |>
    filter(.data$pertains == "Disease of interest") |>
    group_by(.data$phenotypeId, .data$bucket) |>
    group_split()

  batchSize <- 25
  batches <- split(groups, (seq_along(groups) - 1) %/% batchSize)

  cluster <- ParallelLogger::makeCluster(min(maxCores, length(batches)))
  ParallelLogger::clusterApply(
    cluster = cluster,
    x = batches,
    fun = unionConceptSetsBatched,
    databaseReference = databaseReference,
    unionsFolder = unionsFolder
  )
  ParallelLogger::stopCluster(cluster)
}

unionConceptSetsBatched <- function(batch, databaseReference, unionsFolder) {
  connection <- DatabaseConnector::connect(databaseReference$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  for (i in seq_along(batch)) {
    group <- batch[[i]]
    conceptsetName <-  paste(gsub("[^[:alnum:]]+", "", group$phenotypeName[1]),
                             group$bucket[1],
                             sep = "_")
    if (!file.exists(file.path(unionsFolder, sprintf("%s.json", conceptsetName)))) {
      PhenotypeTemplates::unionConceptsets(
        conceptsetName = conceptsetName,
        conceptSetExpressions = group$circeConceptSetExpression,
        vocabularyDatabaseSchema = databaseReference$cdmDatabaseSchema,
        outputFolder = unionsFolder,
        connectionDetails = NULL,
        connection = connection
      )
    }
  }
  invisible(NULL)
}
