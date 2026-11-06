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
#' @template MaxCores
#' @param uploadKeeperProfiles If TRUE then the patient profiles will also be uploaded to the database, which can be
#' used by the MonoFlow agent.
#' @param workFolder A folder where data objects are stored.
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
                                         uploadKeeperProfiles = TRUE,
                                         workFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::assertClass(databaseReference, "DatabaseReference", add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::assertLogical(uploadKeeperProfiles, len = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  startCost <- costTracker$amount

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
      overlap <- intersect(phenotypes$phenotypeName, existingCohorts$phenotype)
      if (length(overlap) > 0) {
        stop(sprintf("Phenotypes '%s' already have reference cohorts in the database",
                     paste(overlap, collapse =  "', '")))
      }
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

  message("Generating Keeper reference cohorts")
  ParallelLogger::addDefaultFileLogger(file.path(workFolder, "log.txt"), name = "KEEPER_LOGGER")
  cluster <- ParallelLogger::makeCluster(min(maxCores, nrow(phenotypes)))
  ParallelLogger::clusterRequire(cluster, "dplyr")
  rows <- split(phenotypes, phenotypes$cohortId)
  cost <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = rows,
    fun = createKeeperReferenceCohort,
    llmClient = llmClient,
    databaseReference = databaseReference,
    workFolder = workFolder
  )
  ParallelLogger::stopCluster(cluster)
  ParallelLogger::unregisterLogger("KEEPER_LOGGER")

  costTracker$amount <- costTracker$amount + sum(unlist(cost))

  status <- uploadReferenceCohortsAndProfiles(
    phenotypes = phenotypes,
    connection = connection,
    databaseReference = databaseReference,
    uploadKeeperProfiles = uploadKeeperProfiles,
    workFolder = workFolder
  )

  delta <- Sys.time() - startTime
  message(sprintf("Generating Keeper reference cohorts took %0.2f %s and cost $%0.2f.",
                  delta,
                  attr(delta, "units"),
                  costTracker$amount - startCost))
  return(status)
}

uploadReferenceCohortsAndProfiles <- function(phenotypes,
                                              connection,
                                              databaseReference,
                                              uploadKeeperProfiles,
                                              workFolder) {
  message("Uploading Keeper reference cohorts and profiles")

  if (uploadKeeperProfiles) {
    profilesTableExists <- DatabaseConnector::existsTable(
      connection = connection,
      databaseSchema = databaseReference$referenceCohortDatabaseSchema,
      tableName = paste0(databaseReference$referenceCohortTable, "_profile")
    )
    if (!profilesTableExists) {
      message("- Creating profile table")
      sql <- "
        CREATE TABLE @database_schema.@table (
          cohort_definition_id INT,
          person_id BIGINT,
          start_day INT,
          end_day INT,
          concept_id INT,
          concept_name VARCHAR,
          category VARCHAR,
          target VARCHAR,
          extra_data VARCHAR
        );
      "
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql,
        database_schema = databaseReference$referenceCohortDatabaseSchema,
        table = paste0(databaseReference$referenceCohortTable, "_profile")
      )
    }
  }
  status <- phenotypes |>
    select("cohortId", "phenotypeName") |>
    mutate(referenceCohortSize = 0,
           error = "")
  for (i in seq_len(nrow(phenotypes))) {
    row <- phenotypes[i, ]
    message("- Uploading reference cohort and profiles for '", row$phenotypeName,"'.")
    databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
    phenotypeFolder <- file.path(workFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))

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
          referenceCohortTableNames = Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable),
          referenceCohortDefinitionId = row$cohortId,
          createReferenceCohortTables = FALSE,
          stratifiedReviews = llmReviews
        )
        status$referenceCohortSize[i] <- nrow(llmReviews$llmReviews)

        if (uploadKeeperProfiles) {
          subFolder =  file.path(phenotypeFolder, sprintf("cache_%s", databaseForFileName))
          keeperSamplePhase1 <- readRDS(file.path(subFolder, "keeperSamplePhase1.rds"))
          keeperSamplePhase2 <- readRDS(file.path(subFolder, "keeperSamplePhase2.rds"))
          keeper <- bind_rows(personIdToColumn(keeperSamplePhase1),
                              personIdToColumn(keeperSamplePhase2)) |>
            mutate(cohortDefinitionId = row$cohortId)
          sql <- "
          DELETE FROM @database_schema.@table
          WHERE cohort_definition_id = @cohort_id;
        "
          DatabaseConnector::renderTranslateExecuteSql(
            connection = connection,
            sql = sql,
            database_schema = databaseReference$referenceCohortDatabaseSchema,
            table = paste0(databaseReference$referenceCohortTable, "_profile"),
            cohort_id = row$cohortId,
            reportOverallTime = FALSE,
            progressBar = FALSE
          )
          DatabaseConnector::insertTable(
            connection = connection,
            databaseSchema = databaseReference$referenceCohortDatabaseSchema,
            tableName = paste0(databaseReference$referenceCohortTable, "_profile"),
            data = keeper,
            createTable = FALSE,
            tempTable = FALSE,
            camelCaseToSnakeCase = TRUE
          )
        }
      }
    }
  }
  return(status)
}

personIdToColumn <- function(keeper) {
  # Note: generated ID is re-used across samples, so best to drop:
  keeper <- keeper |>
    inner_join(
      keeper |>
        filter(.data$category == "personId") |>
        transmute(.data$generatedId,
                  personId = bit64::as.integer64(.data$conceptName)),
      by = join_by("generatedId")
    ) |>
    select(-"generatedId")
  return(keeper)

}


# row = rows[[1]]
createKeeperReferenceCohort <- function(row,
                                        llmClient,
                                        databaseReference,
                                        workFolder,
                                        stratifiedSampling = TRUE) {

  message(sprintf("*** Creating Keeper reference set for '%s' in %s ***",
                  row$phenotypeName,
                  databaseReference$databaseName))

  databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
  phenotypeFolder <- file.path(workFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))
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
        cacheFolder = file.path(phenotypeFolder, sprintf("cache_%s", databaseForFileName))
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
