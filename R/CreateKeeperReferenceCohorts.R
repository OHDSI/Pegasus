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

#' Create Keeper reference cohorts
#'
#' @template Phenotypes
#' @template LlmClient
#' @template CostTracker
#' @template DatabaseReference
#' @param maxCores Maximum number of parallel cores to use.
#' @param cacheFolder A folder where data objects can be cached so a restart can continue where it left off.
#'
#' @returns
#' A tibble with a row per phenotype, indicating the reference cohort Id, size, and possibly error messages.
#'
#' @export
createKeeperReferenceCohorts <- function(phenotypes,
                                         llmClient,
                                         costTracker = createCostTracker(),
                                         databaseReference,
                                         maxCores = 4,
                                         cacheFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::assertClass(databaseReference, "DatabaseReference", add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::assertCharacter(cacheFolder, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  connection <- DatabaseConnector::connect(databaseReference$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  referenceCohortTableNames <- Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable)
  referenceCohortTableExists <- DatabaseConnector::existsTable(
    connection = connection,
    databaseSchema = databaseReference$referenceCohortDatabaseSchema,
    tableName = referenceCohortTableNames$referenceCohortMetadataTable
  )
  if (referenceCohortTableExists) {
    existingCohorts <- Keeper::listReferenceCohorts(
      connection = connection,
      referenceCohortDatabaseSchema = databaseReference$referenceCohortDatabaseSchema,
      referenceCohortMetadataTable = referenceCohortTableNames$referenceCohortMetadataTable
    )
    if (nrow(existingCohorts) == 0) {
      lastCohortId <- 0
    } else {
      lastCohortId <- max(existingCohorts$cohortDefinitionId)
    }
  } else {
    Keeper::createReferenceCohortTables(
      connection = connection,
      referenceCohortDatabaseSchema = databaseReference$referenceCohortDatabaseSchema,
      referenceCohortTableNames = referenceCohortTableNames
    )
    existingCohorts <- tibble()
    lastCohortId <- 0
  }

  sensitiveCohortTableExists <- DatabaseConnector::existsTable(
    connection = connection,
    databaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
    tableName = databaseReference$sensitiveCohortTable
  )
  if (!sensitiveCohortTableExists) {
    Keeper::createSensitiveCohortTable(
      connection = connection,
      sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
      sensitiveCohortTable = databaseReference$sensitiveCohortTable
    )
  }

  phenotypes <- phenotypes |>
    mutate(cohortId = row_number() + lastCohortId)

  message("Generating Keeper reference profiles")
  ParallelLogger::addDefaultFileLogger(file.path(cacheFolder, "log.txt"), name = "KEEPER_LOGGER")
  cluster <- ParallelLogger::makeCluster(min(maxCores, nrow(phenotypes)))
  ParallelLogger::clusterRequire(cluster, "dplyr")
  rows <- split(phenotypes, phenotypes$cohortId)
  cost <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = rows,
    fun = createKeeperReferenceCohort,
    llmClient = llmClient,
    databaseReference = databaseReference,
    cacheFolder = cacheFolder
  )
  ParallelLogger::stopCluster(cluster)
  ParallelLogger::unregisterLogger("KEEPER_LOGGER")

  costTracker$amount <- costTracker$amount + sum(unlist(cost))

  message("Uploading Keeper reference profiles")
  status <- phenotypes |>
    select("cohortId", "phenotypeName") |>
    mutate(referenceCohortSize = 0,
           error = "")
  for (i in seq_len(nrow(phenotypes))) {
    row <- phenotypes[i, ]
    message("- Uploading reference cohort for '", row$phenotypeName,"'.")
    referenceCohortId <-lastCohortId + 1
    databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
    phenotypeFolder <- file.path(cacheFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))

    fileName <- file.path(phenotypeFolder, sprintf("llmReviews_%s.rds", databaseForFileName))
    if (!file.exists(fileName)) {
      message("- No reference cohort found for '", row$phenotypeName, "'. Skipping")
      error <- "No reference cohort"
      # There may be a more informative error message in the specific concepts file:
      fileName <- file.path(phenotypeFolder, sprintf("SpecificConcepts_%s.csv", databaseForFileName))
      if (!file.exists(fileName)) {
        specificConcepts <- readr::read_csv(fileName, show_col_types = FALSE)
        if ("error" %in% colnames(specificConcepts)) {
          error <- specificConcepts$error[1]
        }
      }
      status$error[i] <- error
    } else {
      llmReviews <- readRDS(fileName)
      if (is.character(llmReviews)) {
        message("- Phenotype '", row$phenotypeName, "' has error '", llmReviews, "'. Skipping")
        status$error[i] <- llmReviews
      } else {
        Keeper::uploadReferenceCohortUsingStratifiedSample(
          connection = connection,
          sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
          sensitiveCohortTable = databaseReference$sensitiveCohortTable,
          sensitiveCohortDefinitionId = row$cohortId,
          referenceCohortDatabaseSchema = databaseReference$referenceCohortDatabaseSchema,
          referenceCohortTableNames = referenceCohortTableNames,
          referenceCohortDefinitionId = row$cohortId,
          createReferenceCohortTables = FALSE,
          stratifiedReviews = llmReviews
        )
        status$referenceCohortSize[i] <- nrow(llmReviews$llmReviews)
      }
    }
  }
  return(status)
}

# row = rows[[1]]
createKeeperReferenceCohort <- function(row,
                                        llmClient,
                                        databaseReference,
                                        cacheFolder,
                                        stratifiedSampling = TRUE) {

  message(sprintf("*** Creating Keeper reference set for '%s' in %s ***",
                  row$phenotypeName,
                  databaseReference$databaseName))

  databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
  phenotypeFolder <- file.path(cacheFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))
  dir.create(phenotypeFolder)

  # Generate concept sets ----------------------------------------------------------------------------------------------
  conceptSets <- withCache({
    Keeper::generateKeeperConceptSets(
      phenotype = row$phenotypeName,
      clinicalDefinition = row$clinicalDefinition,
      client = llmClient,
      vocabConnectionDetails = databaseReference$connectionDetails,
      vocabDatabaseSchema = databaseReference$cdmDatabaseSchema
    )
  },
  cacheFolder = phenotypeFolder,
  fileName = "KeeperConceptSets.csv"
  )

  # Create sensitive cohort --------------------------------------------------------------------------------------------
  specificConcepts <- withCache({
    tryCatch({
      Keeper::createSensitiveCohort(
        connectionDetails = databaseReference$connectionDetails,
        cdmDatabaseSchema = databaseReference$cdmDatabaseSchema,
        tempEmulationSchema = databaseReference$tempEmulationSchema,
        sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
        sensitiveCohortTable = databaseReference$sensitiveCohortTable,
        sensitiveCohortDefinitionId = row$cohortId,
        createCohortTable = FALSE,
        keeperConceptSets = conceptSets
      )
    },
    NoPersonsWithDoiError = function(e) {
      warning(e$message)
      return(tibble(error = "No persons with disease of interest or hypernym concepts."))
    })
  },
  cacheFolder = phenotypeFolder,
  fileName = sprintf("SpecificConcepts_%s.csv", databaseForFileName)
  )
  if ("error" %in% colnames(specificConcepts)) {
    return(NULL)
  }

  # Review patient profiles using LLM ----------------------------------------------------------------------------------
  llmReviews <- withCache({
    tryCatch({
      Keeper::reviewCasesUsingStratifiedSampling(
        connectionDetails = databaseReference$connectionDetails,
        cdmDatabaseSchema = databaseReference$cdmDatabaseSchema,
        tempEmulationSchema = databaseReference$tempEmulationSchema,
        sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
        sensitiveCohortTable = databaseReference$sensitiveCohortTable,
        sensitiveCohortDefinitionId = row$cohortId,
        keeperConceptSets = conceptSets,
        client = llmClient,
        phenotypeName = row$phenotypeName,
        clinicalDefinition = row$clinicalDefinition,
        cacheFolder =  file.path(phenotypeFolder, sprintf("cache_%s", databaseForFileName))
      )
    },
    NoCasesInSampleError = function(e) {
      warning(e$message)
      return("No cases in sample")
    })
  },
  cacheFolder = phenotypeFolder,
  fileName = sprintf("llmReviews_%s.rds", databaseForFileName)
  )
  if (!is.character(llmReviews)) {
    return(llmReviews$cost)
  } else {
    return(0)
  }
}
