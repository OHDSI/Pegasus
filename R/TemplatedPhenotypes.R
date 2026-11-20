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
    "phenotypeId",
    "phenotypeName"
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

  dir.create(workFolder, recursive = TRUE, showWarnings = TRUE)

  ParallelLogger::addDefaultFileLogger(file.path(workFolder, "log.txt"), name = "TEMPLATES_LOGGER")
  on.exit(ParallelLogger::unregisterLogger("TEMPLATES_LOGGER"))

  if (is.null(pegasusConnection)) {
    pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(pegasusConnection), add = TRUE)
  }

  sql <- "
    SELECT DISTINCT phenotype_id
    FROM @database_schema.cohort_definition
    WHERE cd_source_type = 'Templates'
      AND phenotype_id IN (@phenotype_ids);
  "
  existingPhenotypeIds <- DatabaseConnector::renderTranslateQuerySql(
    connection = pegasusConnection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    phenotype_ids = phenotypes$phenotypeId
  )
  if (length(existingPhenotypeIds) > 0) {
    message(sprintf("- Skipping %d of %d phenotypes that already have teamplated cohort definitions.",
                    length(existingPhenotypeIds),
                    nrow(phenotypes)))
    phenotypes <- phenotypes |>
      filter(!.data$phenotypeId %in% existingPhenotypeIds)
    if (nrow(phenotypes) == 0) {
      return(tibble())
    }
  }

  dir.create(workFolder, recursive = TRUE, showWarnings = FALSE)
  unionsFolder <- file.path(workFolder, "conceptSetUnions")
  dir.create(unionsFolder, showWarnings  = FALSE)

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

      bucketFiles <- list.files(unionsFolder, pattern = sprintf("^%s_[A-Z]\\.json$", gsub("[^[:alnum:]]+", "", phenotype$phenotypeName)))
      bucketNames <- gsub("^.*_", "", gsub(".json$", "", bucketFiles))
      bucketCs <- lapply(seq_along(bucketFiles), function(j) Capr::readConceptSet(
        path = file.path(unionsFolder, bucketFiles[j]),
        name = bucketNames[j])
      )
      names(bucketCs) <- bucketNames

      templateCds <- PhenotypeTemplates::buildPhenotypeTemplates(
        conceptSets    = bucketCs,
        phenotypeLabel = phenotype$phenotypeName,
        config         = phenotypeConfig,
        startCohortId  = 1
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
    phenotypeIds = phenotypes$phenotypeId,
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

  phenotypeNames <- phenotypes$phenotypeName[match(phenotypeMetaData$phenotypeId, phenotypes$phenotypeId)]
  problems <- PhenotypeTemplates::validateClinicalContext(phenotypeMetaData, throw = FALSE)
  if (nrow(problems) > 0) {
    stop(sprintf("Invalid phenotype meta data:\n%s",
                 paste(sprintf("  - %s: %s = '%s' %s",
                               phenotypeNames[problems$row],
                               problems$field,
                               ifelse(is.na(problems$value), "", problems$value),
                               problems$problem),
                       collapse = "\n")))
  }

  phenotypeConfigs <- lapply(seq_len(nrow(phenotypeMetaData)), function(i) {
    PhenotypeTemplates::phenotypeConfigFromContext(phenotypeMetaData[i, ])
  })
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
    phenotypeIds = phenotypes$phenotypeId,
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

  phenotypeToConceptSets <- phenotypeToConceptSets |>
    mutate(bucket = PhenotypeTemplates::conceptCategoryToBucket(.data$category)) |>
    filter(!is.na(.data$bucket))

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
