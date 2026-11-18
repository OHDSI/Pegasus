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
#' @template DatabaseReferences
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
                                         databaseReferences,
                                         maxCores = 4,
                                         uploadKeeperProfiles = TRUE,
                                         workFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeId",
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::assertList(databaseReferences, types = "DatabaseReference", min.len = 1, add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::assertLogical(uploadKeeperProfiles, len = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  startCost <- costTracker$amount

  dir.create(workFolder, recursive = TRUE, showWarnings = FALSE)
  ParallelLogger::addDefaultFileLogger(file.path(workFolder, "log.txt"), name = "KEEPER_LOGGER")

  phenotypeDatabases <- lapply(databaseReferences, removeFinishedKeeperReferenceCohortJobs, phenotypes = phenotypes)
  phenotypeDatabases <- bind_rows(phenotypeDatabases)

  remainingPhenotypes <- phenotypeDatabases |>
    distinct(.data$phenotypeId, .data$phenotypeName, .data$clinicalDefinition) |>
    mutate(phenotypeFolder =  file.path(workFolder, sprintf("Phenotype_%d", .data$phenotypeId)))
  for (i in seq_len(nrow(remainingPhenotypes))) {
    createKeeperConceptSets(
      phenotype = remainingPhenotypes[i, ],
      llmClient = llmClient,
      costTracker = costTracker,
      databaseReference = databaseReferences[[1]]
    )
  }

  status <- list()
  for (i in seq_along(databaseReferences)) {
    databaseReference <- databaseReferences[[i]]
    phenotypesInDatabase <- phenotypeDatabases |>
      filter(.data$databaseId == databaseReference$databaseId)
    if (nrow(phenotypesInDatabase) == 0) {
      next
    }
    status[[i]] <- createKeeperReferenceCohortsForDatabase(
      phenotypes = phenotypesInDatabase,
      llmClient = llmClient,
      costTracker = costTracker,
      databaseReference = databaseReference,
      maxCores = maxCores,
      uploadKeeperProfiles = uploadKeeperProfiles,
      workFolder = workFolder
    )
  }

  status <- bind_rows(status)
  delta <- Sys.time() - startTime
  message(sprintf("Generating Keeper reference cohorts took %0.2f %s and cost $%0.2f.",
                  delta,
                  attr(delta, "units"),
                  costTracker$amount - startCost))
  ParallelLogger::unregisterLogger("KEEPER_LOGGER")

  return(status)
}

removeFinishedKeeperReferenceCohortJobs <- function(databaseReference,
                                                    phenotypes) {
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
    overlap <- intersect(phenotypes$phenotypeId, existingCohorts$cohortDefinitionId)
    if (length(overlap) > 0) {
      message(sprintf("- Phenotypes %s already have reference cohorts in database %s. Skipping",
                      paste(overlap, collapse =  ", "),
                      databaseReference$databaseName))
      phenotypes <- phenotypes |>
        filter(!as.character(.data$phenotypeId) %in% existingCohorts$phenotype)
    }
  } else {
    Keeper::createReferenceCohortTables(
      connection = connection,
      referenceCohortDatabaseSchema = databaseReference$referenceCohortDatabaseSchema,
      referenceCohortTableNames = referenceCohortTableNames
    )
  }
  phenotypesWithDatabase <- phenotypes |>
    mutate(databaseId = databaseReference$databaseId)
  return(phenotypesWithDatabase)
}

createKeeperReferenceCohortsForDatabase <- function(phenotypes,
                                                    llmClient,
                                                    costTracker,
                                                    databaseReference,
                                                    maxCores,
                                                    uploadKeeperProfiles,
                                                    workFolder) {
  message(sprintf("Creating Keeper reference cohorts for database %s",
                  databaseReference$databaseName))

  connection <- DatabaseConnector::connect(databaseReference$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

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

  cluster <- ParallelLogger::makeCluster(min(maxCores, nrow(phenotypes)))
  ParallelLogger::clusterRequire(cluster, "dplyr")
  rows <- split(phenotypes, phenotypes$phenotypeId)
  cost <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = rows,
    fun = createKeeperReferenceCohort,
    llmClient = llmClient,
    databaseReference = databaseReference
  )
  ParallelLogger::stopCluster(cluster)

  costTracker$amount <- costTracker$amount + sum(unlist(cost))

  status <- uploadReferenceCohortsAndProfiles(
    phenotypes = phenotypes,
    connection = connection,
    databaseReference = databaseReference,
    uploadKeeperProfiles = uploadKeeperProfiles
  )
  return(status)
}

uploadReferenceCohortsAndProfiles <- function(phenotypes,
                                              connection,
                                              databaseReference,
                                              uploadKeeperProfiles) {
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
    select("phenotypeId") |>
    mutate(databaseId = databaseReference$databaseId,
           referenceCohortSize = 0,
           error = "")
  for (i in seq_len(nrow(phenotypes))) {
    row <- phenotypes[i, ]
    message(sprintf("- Uploading reference cohort and profiles for '%s' (%d).",
                    row$phenotypeName,
                    row$phenotypeId))
    databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)

    fileName <- file.path(row$phenotypeFolder, sprintf("llmReviews_%s.rds", databaseForFileName))
    if (!file.exists(fileName)) {
      message("- No reference cohort found for '", row$phenotypeName, "'. Skipping")
      error <- "No reference cohort"
      # There may be a more informative error message in the specific concepts file:
      fileName <- file.path(row$phenotypeFolder, sprintf("SpecificConcepts_%s.csv", databaseForFileName))
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
        message(sprintf("- Phenotype '%s' (%d) has error '%s'. Skipping",
                        row$phenotypeName,
                        row$phenotypeId,
                        llmReviews))
        status$error[i] <- llmReviews
      } else {
        Keeper::uploadReferenceCohortUsingStratifiedSample(
          connection = connection,
          sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
          sensitiveCohortTable = databaseReference$sensitiveCohortTable,
          sensitiveCohortDefinitionId = row$phenotypeId,
          referenceCohortDatabaseSchema = databaseReference$referenceCohortDatabaseSchema,
          referenceCohortTableNames = Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable),
          referenceCohortDefinitionId = row$phenotypeId,
          createReferenceCohortTables = FALSE,
          stratifiedReviews = llmReviews
        )
        status$referenceCohortSize[i] <- nrow(llmReviews$llmReviews)

        if (uploadKeeperProfiles) {
          subFolder =  file.path(row$phenotypeFolder, sprintf("cache_%s", databaseForFileName))
          keeperSamplePhase1File <- file.path(subFolder, "keeperSamplePhase1.rds")
          keeper <- readRDS(keeperSamplePhase1File) |>
            personIdToColumn()

          keeperSamplePhase2File <- file.path(subFolder, "keeperSamplePhase2.rds")
          if (file.exists(keeperSamplePhase2File)) {
            # Phase 2 sample may not exist if phase 1 sample was already bigger than required.
            keeperSamplePhase2 <- readRDS(keeperSamplePhase2File) |>
              personIdToColumn()
            keeper <- bind_rows(keeper,
                                keeperSamplePhase2)
          }
          keeper <- keeper |>
            mutate(cohortDefinitionId = row$phenotypeId)
          sql <- "
            DELETE FROM @database_schema.@table
            WHERE cohort_definition_id = @cohort_definition_id;
          "
          DatabaseConnector::renderTranslateExecuteSql(
            connection = connection,
            sql = sql,
            database_schema = databaseReference$referenceCohortDatabaseSchema,
            table = paste0(databaseReference$referenceCohortTable, "_profile"),
            cohort_definition_id = row$phenotypeId,
            reportOverallTime = FALSE,
            progressBar = FALSE
          )
          DatabaseConnector::insertTable(
            connection = connection,
            databaseSchema = databaseReference$referenceCohortDatabaseSchema,
            tableName = paste0(databaseReference$referenceCohortTable, "_profile"),
            data = keeper,
            createTable = FALSE,
            dropTableIfExists = FALSE,
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

createKeeperConceptSets <- function(phenotype,
                                    llmClient,
                                    costTracker,
                                    databaseReference) {
  message(sprintf("Creating Keeper concept sets for '%s' (%d)",
                  phenotype$phenotypeName,
                  phenotype$phenotypeId))

  dir.create(phenotype$phenotypeFolder)

  fileName <- file.path(phenotype$phenotypeFolder, "KeeperConceptSets.csv")

  if (!file.exists(fileName)) {
    keeperCostTracker <- Keeper::createCostTracker()
    conceptSets <- Keeper::generateKeeperConceptSets(
      phenotype = phenotype$phenotypeName,
      clinicalDefinition = phenotype$clinicalDefinition,
      client = llmClient,
      costTracker = keeperCostTracker,
      vocabConnectionDetails = databaseReference$connectionDetails,
      vocabDatabaseSchema = databaseReference$cdmDatabaseSchema
    )
    costTracker$amount <- costTracker$amount + keeperCostTracker$amount
    readr::write_csv(conceptSets, fileName)
  }
}


# phenotype = phenotypesWithFolders[1,]
createKeeperReferenceCohort <- function(phenotype,
                                        llmClient,
                                        databaseReference,
                                        stratifiedSampling = TRUE) {
  message(sprintf("*** Creating Keeper reference set for '%s' (%d) in %s ***",
                  phenotype$phenotypeName,
                  phenotype$phenotypeId,
                  databaseReference$databaseName))

  databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
  databaseCacheFolder <- file.path(phenotype$phenotypeFolder, sprintf("cache_%s", databaseForFileName))
  dir.create(databaseCacheFolder, showWarnings = FALSE)

  # Load concept sets ----------------------------------------------------------------------------------------------
  fileName <- file.path(phenotype$phenotypeFolder, "KeeperConceptSets.csv")
  conceptSets <- readr::read_csv(fileName, show_col_types = FALSE)

  # Create sensitive cohort --------------------------------------------------------------------------------------------
  specificConcepts <- withCache({
    tryCatch({
      Keeper::createSensitiveCohort(
        connectionDetails = databaseReference$connectionDetails,
        cdmDatabaseSchema = databaseReference$cdmDatabaseSchema,
        tempEmulationSchema = databaseReference$tempEmulationSchema,
        sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
        sensitiveCohortTable = databaseReference$sensitiveCohortTable,
        sensitiveCohortDefinitionId = phenotype$phenotypeId,
        createCohortTable = FALSE,
        keeperConceptSets = conceptSets
      )
    },
    NoPersonsWithDoiError = function(e) {
      warning(e$message)
      return(tibble(error = "No persons with disease of interest or hypernym concepts."))
    })
  },
  cacheFolder = phenotype$phenotypeFolder,
  fileName = sprintf("SpecificConcepts_%s.csv", databaseForFileName)
  )
  if ("error" %in% colnames(specificConcepts)) {
    return(NULL)
  }

  # Review patient profiles using LLM ----------------------------------------------------------------------------------
  keeperCostTracker <- Keeper::createCostTracker()
  llmReviews <- withCache({
    tryCatch({
      Keeper::reviewCasesUsingStratifiedSampling(
        connectionDetails = databaseReference$connectionDetails,
        cdmDatabaseSchema = databaseReference$cdmDatabaseSchema,
        tempEmulationSchema = databaseReference$tempEmulationSchema,
        sensitiveCohortDatabaseSchema = databaseReference$sensitiveCohortDatabaseSchema,
        sensitiveCohortTable = databaseReference$sensitiveCohortTable,
        sensitiveCohortDefinitionId = phenotype$cohortId,
        keeperConceptSets = conceptSets,
        client = llmClient,
        costTracker = keeperCostTracker,
        phenotypeName = phenotype$phenotypeName,
        clinicalDefinition = phenotype$clinicalDefinition,
        cacheFolder = databaseCacheFolder
      )
    },
    NoCasesInSampleError = function(e) {
      warning(e$message)
      return("No cases in sample")
    })
  },
  cacheFolder = phenotype$phenotypeFolder,
  fileName = sprintf("llmReviews_%s.rds", databaseForFileName)
  )
  if (!is.character(llmReviews)) {
    return(llmReviews$cost)
  } else {
    return(0)
  }
}

#' Evaluate cohort definitions using Keeper reference cohorts
#'
#' @template Phenotypes
#' @template PegasusConnectionDetails
#' @template DatabaseReferences
#'
#' @returns
#' A tibble with one row per cohort definition - database combination.
#'
#' @export

evaluateUsingKeeperReferenceCohorts <- function(phenotypes,
                                                pegasusConnection = NULL,
                                                pegasusConnectionDetails = NULL,
                                                pegasusDatabaseSchema,
                                                databaseReferences) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeId",
    "phenotypeName"
  ), add = errorMessages)
  checkmate::assertClass(pegasusConnection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(databaseReferences, min.len = 1, add = errorMessages)
  for (databaseReference in databaseReferences) {
    checkmate::assertClass(databaseReference, "DatabaseReference", add = errorMessages)
  }
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(pegasusConnection) && is.null(pegasusConnectionDetails)) {
    stop("Must provide either pegasusConnection or pegasusConnectionDetails")
  }

  startTime <- Sys.time()

  if (is.null(pegasusConnection)) {
    pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(pegasusConnection))
  }

  cohortDefinitions <- downloadCohortDefinition(
    phenotypeIds = phenotypes$phenotypeId,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  ) |>
    inner_join(phenotypes |>
                 select("phenotypeId", "phenotypeName"),
               by = join_by("phenotypeId"))

  existingEvaluations <- getExistingEvaluations(
    cohortDefinitionIds = cohortDefinitions$cohortDefinitionId,
    databaseIds = sapply(databaseReferences, function(x) x$databaseId),
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

  jsonToSql <- function(json) {
    expression <- CirceR::cohortExpressionFromJson(json)
    sql <- CirceR::buildCohortQuery(expression, CirceR::createGenerateOptions(generateStats = FALSE))
    return(sql)
  }

  cohortDefinitions <- cohortDefinitions |>
    mutate(sql = sapply(.data$circeCohortExpression, jsonToSql))

  results <- list()
  for (i in seq_along(databaseReferences)) {
    databaseReference <- databaseReferences[[i]]
    toSkip <- existingEvaluations |>
      filter(.data$databaseId == databaseReference$databaseId)
    results[[i]] <- evaluateInDatabase(
      cohortDefinitions = cohortDefinitions,
      databaseReference = databaseReference,
      toSkip = toSkip
    )
  }
  results <- bind_rows(results)

  delta <- Sys.time() - startTime
  message(sprintf("Evaluating cohort definitions took %0.2f %s.",
                  delta,
                  attr(delta, "units")))
  return(results)
}

evaluateInDatabase <- function(cohortDefinitions, databaseReference, toSkip) {
  message("Evaluating cohort definitions in database ", databaseReference$databaseName)
  connection <- DatabaseConnector::connect(databaseReference$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Instantiate cohorts ------------------------------------------------------------------------------------------------
  cohortTableNames <- CohortGenerator::getCohortTableNames(databaseReference$candidateCohortTable)
  if (!DatabaseConnector::existsTable(connection = connection,
                                      databaseSchema = databaseReference$candidateCohortDatabaseSchema,
                                      tableName = databaseReference$candidateCohortTable)) {
    CohortGenerator::createCohortTables(
      connection = connection,
      cohortDatabaseSchema = databaseReference$candidateCohortDatabaseSchema,
      cohortTableNames = cohortTableNames
    )
  }
  toEvaluate <- cohortDefinitions  |>
    filter(!.data$cohortDefinitionId %in% toSkip$cohortDefinitionId)
  if (nrow(toEvaluate) == 0) {
    return(NULL)
  }
  cohortDefinitionSet <- toEvaluate |>
    select(
      cohortId = "cohortDefinitionId",
      cohortName = "cohortDefinitionName",
      "sql",
      json = "circeCohortExpression"
    )
  CohortGenerator::generateCohortSet(
    connection = connection,
    cdmDatabaseSchema = databaseReference$cdmDatabaseSchema,
    cohortDatabaseSchema = databaseReference$candidateCohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortDefinitionSet,
    incremental = TRUE,
    tempEmulationSchema = databaseReference$tempEmulationSchema
  )
  counts <- CohortGenerator::getCohortCounts(
    connection = connection,
    cohortDatabaseSchema = databaseReference$candidateCohortDatabaseSchema,
    cohortTable = databaseReference$candidateCohortTable,
    cohortIds = toEvaluate$cohortDefinitionId
  )
  times <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM @database_schema.@table WHERE cohort_definition_id IN (@cohort_ids);",
    database_schema = databaseReference$candidateCohortDatabaseSchema,
    table = cohortTableNames$cohortChecksumTable,
    cohort_ids = toEvaluate$cohortDefinitionId,
    snakeCaseToCamelCase = TRUE
  )
  times <- times |>
    mutate(generationSeconds = (.data$endTime - .data$startTime) / 1000)

  # Evaluate cohorts ---------------------------------------------------------------------------------------------------
  results <- list()
  for (i in seq_len(nrow(toEvaluate))) {
    cohortDefinition <- toEvaluate[i, ]
    referenceCohortDefinitionId <- getKeeperReferenceCohortId(connection = connection,
                                                              databaseReference = databaseReference,
                                                              phenotypeId = cohortDefinition$phenotypeId,
                                                              phenotypeName = cohortDefinition$phenotypeName)
    metrics <- Keeper::evaluateCohortUsingReferenceCohort(
      connection = connection,
      cohortDatabaseSchema = databaseReference$candidateCohortDatabaseSchema,
      cohortTable = databaseReference$candidateCohortTable,
      cohortDefinitionId = cohortDefinition$cohortDefinitionId,
      referenceCohortDatabaseSchema = databaseReference$referenceCohortDatabaseSchema,
      referenceCohortTableNames = Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable),
      referenceCohortDefinitionId = referenceCohortDefinitionId
    )
    results[[i]] <- metrics |>
      select(
        truePositives = "tp",
        falsePositives = "fp",
        trueNegatives = "tn",
        falseNegatives = "fn",
        "ppv",
        "ppvLb",
        "ppvUb",
        "sensitivity",
        "sensitivityLb",
        "sensitivityUb",
        specificity = "specificityOverall",
        specificityLb = "specificityOverallLb",
        specificityUb = "specificityOverallUb",
        npv = "npvOverall",
        npvLb = "npvOverallLb",
        npvUb = "npvOverallUb"
      ) |>
      mutate(cohortDefinitionId = cohortDefinition$cohortDefinitionId,
             phenotypeId = cohortDefinition$phenotypeId,
             phenotypeName = cohortDefinition$phenotypeName)
  }
  results <- bind_rows(results) |>
    mutate(databaseId = databaseReference$databaseId,
           databaseName = databaseReference$databaseName,
           evaluationType = "KEEPER + llm") |>
    inner_join(counts |>
                 select(cohortDefinitionId = "cohortId",
                        "cohortEntries",
                        "cohortSubjects"),
               by = join_by("cohortDefinitionId"))|>
    inner_join(times |>
                 select("cohortDefinitionId",
                        "generationSeconds"),
               by = join_by("cohortDefinitionId"))
  return(results)
}

getExistingEvaluations <- function(cohortDefinitionIds, databaseIds, pegasusConnection, pegasusDatabaseSchema) {
  sql <- "
    SELECT cohort_definition_id,
      database_id
    FROM @database_schema.cohort_definition_evaluation
    WHERE cohort_definition_id IN (@cohort_ids)
      AND database_id IN (@database_ids)
      AND evaluation_type = 'KEEPER + llm';
  "
  existingEvaluations <- DatabaseConnector::renderTranslateQuerySql(
    connection = pegasusConnection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    cohort_ids = cohortDefinitionIds,
    database_ids = databaseIds,
    snakeCaseToCamelCase = TRUE
  )
  return(existingEvaluations)
}


getKeeperReferenceCohortId <- function(connection, databaseReference, phenotypeId, phenotypeName) {
  sql <- "
    SELECT cohort_definition_id,
      phenotype
    FROM @database_schema.@table
    WHERE cohort_definition_id = @cohort_definition_id;
  "
  keeperReference <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseReference$referenceCohortDatabaseSchema,
    table = Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable)$referenceCohortMetadataTable,
    cohort_definition_id = phenotypeId,
    snakeCaseToCamelCase = TRUE
  )
  if (nrow(keeperReference) == 0) {
    stop("Could not find reference cohort for phenotype ", phenotypeId, " in database '", databaseReference$databaseName, "'")
  }
  if (keeperReference$phenotype != phenotypeName) {
    warning(sprintf("Reference cohort %d was created for phenotype '%s', which is a different name than '%s'",
                    keeperReference$phenotype,
                    phenotypeName))
  }

  return(keeperReference$cohortDefinitionId)
}
