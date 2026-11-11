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

  status <- list()
  for (i in seq_along(databaseReferences)) {
    status[[i]] <- createKeeperReferenceCohortsForDatabase(
      phenotypes = phenotypes,
      llmClient = llmClient,
      costTracker = costTracker,
      databaseReference = databaseReferences[[i]],
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
  return(status)
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

  databaseWorkFolder <- file.path(workFolder, gsub("[^[:alnum:]]+", "", databaseReference$databaseName))
  dir.create(databaseWorkFolder, recursive = TRUE)

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
    databaseWorkFolder = databaseWorkFolder
  )
  ParallelLogger::stopCluster(cluster)
  ParallelLogger::unregisterLogger("KEEPER_LOGGER")

  costTracker$amount <- costTracker$amount + sum(unlist(cost))

  status <- uploadReferenceCohortsAndProfiles(
    phenotypes = phenotypes,
    connection = connection,
    databaseReference = databaseReference,
    uploadKeeperProfiles = uploadKeeperProfiles,
    databaseWorkFolder = databaseWorkFolder
  )
  return(status)
}

uploadReferenceCohortsAndProfiles <- function(phenotypes,
                                              connection,
                                              databaseReference,
                                              uploadKeeperProfiles,
                                              databaseWorkFolder) {
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
    mutate(databaseId = databaseReference$databaseId,
           referenceCohortSize = 0,
           error = "")
  for (i in seq_len(nrow(phenotypes))) {
    row <- phenotypes[i, ]
    message("- Uploading reference cohort and profiles for '", row$phenotypeName,"'.")
    databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
    phenotypeFolder <- file.path(databaseWorkFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))

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
                                        databaseWorkFolder,
                                        stratifiedSampling = TRUE) {

  message(sprintf("*** Creating Keeper reference set for '%s' in %s ***",
                  row$phenotypeName,
                  databaseReference$databaseName))

  databaseForFileName <- gsub("[^[:alnum:]]+", "", databaseReference$databaseName)
  phenotypeFolder <- file.path(databaseWorkFolder, gsub("[^[:alnum:]]+", "_", row$phenotypeName))
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
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

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
      filter(.data$databaseId == databaseReferences$databaseId)
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
                                                              phenotype = cohortDefinition$phenotypeName)
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


getKeeperReferenceCohortId <- function(connection, databaseReference, phenotype) {
  sql <- "
    SELECT cohort_definition_id
    FROM @database_schema.@table
    WHERE LOWER(phenotype) = LOWER(@phenotype);
  "
  keeperReference <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseReference$referenceCohortDatabaseSchema,
    table = Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable)$referenceCohortMetadataTable,
    phenotype = quoteSqlString(phenotype),
    snakeCaseToCamelCase = TRUE
  )
  if (nrow(keeperReference) == 0) {
    stop("Could not find reference cohort for phenotype ", phenotype, " in database '", databaseReference$databaseName, "'")
  }
  return(keeperReference$cohortDefinitionId)
}
