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

#' Title
#'
#' @template Phenotypes
#' @template LlmClient
#' @template CdmConnectionDetails
#' @param sensitiveCohortDatabaseSchema A database schema where the sensitive cohort can be instantiated.
#' @param sensitiveCohortTable A table in the sensitive cohort database schema where hte sensitive cohort can be
#'                             instantiated.
#' @param referenceCohortDatabaseSchema The name of the database schema where the reference cohort will be stored.
#' @param referenceCohortTable The name of the table where the reference cohort will be stored.
#' @param maxCores Maximum number of parallel cores to use.
#' @param cacheFolder A folder where data objects can be cached so a restart can continue where it left off.
#'
#' @returns
#' A tibble with a row per phenotype, indicating the reference cohort Id, size, and possibly error messages.
#'
#' @export
createKeeperReferenceCohorts <- function(phenotypes,
                                         llmClient,
                                         cdmConnectionDetails,
                                         tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                         cdmDatabaseSchema,
                                         sensitiveCohortDatabaseSchema,
                                         sensitiveCohortTable,
                                         referenceCohortDatabaseSchema,
                                         referenceCohortTable,
                                         maxCores = 4,
                                         cacheFolder) {

  connection <- DatabaseConnector::connect(cdmConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  referenceCohortTableNames <- Keeper::createReferenceCohortTableNamesUsingStratifiedSample(referenceCohortTable)
  tableExists <- DatabaseConnector::existsTable(
    connection = connection,
    databaseSchema = referenceCohortDatabaseSchema,
    tableName = referenceCohortTableNames$referenceCohortMetadataTable
  )
  if (tableExists) {
    existingCohorts <- Keeper::listReferenceCohorts(
      connection = connection,
      referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
      referenceCohortMetadataTable = referenceCohortTableNames$referenceCohortMetadataTable
    )
    lastCohortId <- max(existingCohorts$cohortDefinitionId)
  } else {
    Keeper::createReferenceCohortTablesUsingStratifiedSample(
      connection = connection,
      referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
      referenceCohortTableNames = referenceCohortTableNames
    )
    existingCohorts <- tibble()
    lastCohortId <- 0
  }
  phenotypes <- phenotypes |>
    mutate(cohortId = row_number() + lastCohortId)

  message("Generating Keeper reference profiles")
  ParallelLogger::addDefaultFileLogger(file.path(cacheFolder, "log.txt"), name = "KEEPER_LOGGER")
  cluster <- ParallelLogger::makeCluster(maxCores)
  ParallelLogger::clusterRequire(cluster, "dplyr")
  rows <- split(phenotypes, phenotypes$cohortId)
  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = rows,
    fun = createKeeperReferenceCohort,
    llmClient = llmClient,
    cdmConnectionDetails = cdmConnectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    sensitiveCohortDatabaseSchema = sensitiveCohortDatabaseSchema,
    sensitiveCohortTable = sensitiveCohortTable,
    tempEmulationSchema = tempEmulationSchema,
    cacheFolder = cacheFolder
  )
  ParallelLogger::stopCluster(cluster)
  ParallelLogger::unregisterLogger("KEEPER_LOGGER")

  message("Uploading Keeper reference profiles")
  status <- phenotypes |>
    select("cohortId", "phenotypeName") |>
    mutate(referenceCohortSize = 0,
           error = "")
  for (i in seq_len(nrow(phenotypes))) {
    row <- phenotypes[i, ]
    message("- Uploading reference cohort for '", row$phenotypeName,"'.")
    referenceCohortId <-lastCohortId + 1
    databaseForFileName <- gsub("[^[:alnum:]]+", "", SqlRender::snakeCaseToCamelCase(cdmDatabaseSchema))
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
          sensitiveCohortDatabaseSchema = sensitiveCohortDatabaseSchema,
          sensitiveCohortTable = sensitiveCohortTable,
          sensitiveCohortDefinitionId = row$cohortId,
          referenceCohortDatabaseSchema = referenceCohortDatabaseSchema,
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
                                        cdmConnectionDetails,
                                        cdmDatabaseSchema,
                                        sensitiveCohortDatabaseSchema,
                                        sensitiveCohortTable,
                                        tempEmulationSchema,
                                        cacheFolder,
                                        stratifiedSampling = TRUE) {

  message(sprintf("*** Creating Keeper reference set for '%s' in %s ***",
                  row$phenotypeName,
                  cdmDatabaseSchema))

  databaseForFileName <- gsub("[^[:alnum:]]+", "", SqlRender::snakeCaseToCamelCase(cdmDatabaseSchema))
  phenotypeFolder <- file.path(cacheFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))
  dir.create(phenotypeFolder)

  # Generate concept sets ----------------------------------------------------------------------------------------------
  conceptSets <- withCache({
    Keeper::generateKeeperConceptSets(
      phenotype = row$phenotypeName,
      clinicalDefinition = row$clinicalDefinition,
      client = llmClient,
      vocabConnectionDetails = cdmConnectionDetails,
      vocabDatabaseSchema = cdmDatabaseSchema
    )
  },
  cacheFolder = phenotypeFolder,
  fileName = "KeeperConceptSets.csv"
  )

  # Create sensitive cohort --------------------------------------------------------------------------------------------
  specificConcepts <- withCache({
    tryCatch({
      Keeper::createSensitiveCohort(
        connectionDetails = cdmConnectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        cohortDatabaseSchema = sensitiveCohortDatabaseSchema,
        cohortTable = sensitiveCohortTable,
        cohortDefinitionId = row$cohortId,
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
        connectionDetails = cdmConnectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        sensitiveCohortDatabaseSchema = sensitiveCohortDatabaseSchema,
        sensitiveCohortTable = sensitiveCohortTable,
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
  return(NULL)
}
