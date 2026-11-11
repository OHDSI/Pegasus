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


#' Create concept set person counts
#'
#' @description
#' For all concept sets associated with the provided phenotypes, counts the number of persons with at least one concept
#' in the concept set, both overall and stratified by domain.
#'
#' IThis function will skip concept set - database combinations that already have counts.
#'
#' @template Phenotypes
#' @template PegasusConnectionDetails
#' @template DatabaseReferences
#' @template MaxCores
#' @template CacheFolder
#'
#' @returns
#' A tibble with one row per concept set, with columns with counts per domain and overall.
#'
#' @export
createConceptSetCounts <- function(phenotypes,
                                   pegasusConnection = NULL,
                                   pegasusConnectionDetails = NULL,
                                   pegasusDatabaseSchema,
                                   databaseReferences,
                                   maxCores = 4,
                                   cacheFolder = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertClass(pegasusConnection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertList(databaseReferences, types = "DatabaseReference", min.len = 1, add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::assertCharacter(cacheFolder, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(pegasusConnection) && is.null(pegasusConnectionDetails)) {
    stop("Must provide either pegasusConnection or pegasusConnectionDetails")
  }
  startTime <- Sys.time()

  if (is.null(pegasusConnection)) {
    pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(pegasusConnection))
  }

  phenotypeToConceptSets <- downloadPhenotypeToConceptSets(
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )

  uniqueConceptSets <- phenotypeToConceptSets |>
    filter(!duplicated(.data$conceptSetId))

  cluster <- ParallelLogger::makeCluster(maxCores)
  ParallelLogger::clusterRequire(cluster, "dplyr")

  counts <- list()
  for (i in seq_along(databaseReferences)) {
    databaseReference <- databaseReferences[[i]]
    message(sprintf("Creating concept set person counts in %s", databaseReference$databaseName))
    if (is.null(cacheFolder)) {
      databaseCacheFolder <- NULL
    } else {
      databaseCacheFolder <- file.path(cacheFolder, gsub("[^[:alnum:]]+", "", databaseReference$databaseName))
      dir.create(databaseCacheFolder, recursive = TRUE)
    }
    conceptSetsToCount <- withCache({
      removeConceptSetsWithExistingCounts(
        uniqueConceptSets = uniqueConceptSets,
        databaseId = databaseReference$databaseId,
        pegasusConnection = pegasusConnection,
        pegasusDatabaseSchema = pegasusDatabaseSchema
      )
    },
    cacheFolder = databaseCacheFolder,
    fileName = "conceptSetsToCount.rds"
    )
    batchSize <- 25
    batches <- split(conceptSetsToCount, (seq_len(nrow(conceptSetsToCount)) - 1) %/% batchSize)
    databaseCounts <- ParallelLogger::clusterApply(
      cluster = cluster,
      x = batches,
      fun = getCountsBatch,
      databaseReference = databaseReference,
      databaseCacheFolder = databaseCacheFolder
    )
    counts[[i]] <- bind_rows(databaseCounts)|>
      mutate(databaseId = databaseReference$databaseId)
  }
  counts <- bind_rows(counts)

  countColumns <- c("overallPersons",
                    "conditionPersons",
                    "drugPersons",
                    "procedurePersons",
                    "measurementPersons",
                    "observationPersons",
                    "procedurePersons",
                    "devicePersons",
                    "visitPersons")
  for (countColumn in countColumns) {
    if (countColumn %in% colnames(counts)) {
      counts[[countColumn]] <- if_else(is.na(counts[[countColumn]]), 0, counts[[countColumn]])
    } else {
      counts[[countColumn]] <- 0
    }
  }

  delta <- Sys.time() - startTime
  message(sprintf("Creating concept sets person counts took %0.2f %s.",
                  delta,
                  attr(delta, "units")))
  return(counts)
}

# batch = batches[[1]]
getCountsBatch <- function(batch, databaseReference, databaseCacheFolder) {
  connection <- DatabaseConnector::connect(databaseReference$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  counts <- list()
  for (i in seq_len(nrow(batch))) {
    circeConceptSetExpression <- batch$circeConceptSetExpression[i]
    counts[[i]] <- withCache({
      getCounts(circeConceptSetExpression = circeConceptSetExpression,
                connection = connection,
                cdmDatabaseSchema = databaseReference$cdmDatabaseSchema,
                tempEmulationSchema = databaseReference$tempEmulationSchema)
    },
    cacheFolder = databaseCacheFolder,
    fileName = sprintf("%s.rds", digest::digest(circeConceptSetExpression))
    )
  }
  counts <- bind_rows(counts)
  counts <- batch |>
    select("conceptSetId") |>
    bind_cols(counts)
  return(counts)
}

getCounts <- function(circeConceptSetExpression, connection, cdmDatabaseSchema, tempEmulationSchema) {
  conceptSetSql <- CirceR::buildConceptSetQuery(circeConceptSetExpression)
  sql <- "
    DROP TABLE IF EXISTS #concept_set;

    SELECT cs.concept_id,
      concept.domain_id
    INTO #concept_set
    FROM (
      @concept_set_sql
    ) cs
    INNER JOIN @cdm_database_schema.concept
      ON cs.concept_id = concept.concept_id;
  "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    concept_set_sql = SqlRender::render(conceptSetSql, vocabulary_database_schema = cdmDatabaseSchema),
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  sql <- "
    SELECT DISTINCT domain_id
    FROM #concept_set ;
  "
  domains <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    tempEmulationSchema = tempEmulationSchema,
    snakeCaseToCamelCase = TRUE
  )[, 1]
  sql <- "
    WITH domain_persons AS (
    {@has_condition} ? {
      SELECT DISTINCT
        person_id,
        'Condition' AS domain_id
      FROM @cdm_database_schema.condition_occurrence
      WHERE condition_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    {@has_procedure} ? {
      {@has_condition} ? {
      UNION ALL
      }
      SELECT DISTINCT
        person_id,
        'Procedure' AS domain_id
      FROM @cdm_database_schema.procedure_occurrence
      WHERE procedure_concept_id IN (
       SELECT concept_id
       FROM #concept_set
      )
    }
    {@has_drug} ?{
      {@has_condition | @has_procedure} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
        'Drug' AS domain_id
      FROM @cdm_database_schema.drug_exposure
      WHERE drug_concept_id IN (
       SELECT concept_id
       FROM #concept_set
      )
    }
    {@has_device} ? {
      {@has_condition | @has_procedure | @has_drug} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
        'Device' AS domain_id
      FROM @cdm_database_schema.device_exposure
      WHERE device_concept_id IN (
       SELECT concept_id
       FROM #concept_set
      )
    }
    {@has_measurement} ? {
      {@has_condition | @has_procedure | @has_drug | @has_device} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
        'Measurement' AS domain_id
      FROM @cdm_database_schema.measurement
      WHERE measurement_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    {@has_observation} ? {
      {@has_condition | @has_procedure | @has_drug | @has_device | @has_measurement} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
       'Observation' AS domain_id
      FROM @cdm_database_schema.observation
      WHERE observation_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    {@has_visit} ? {
      {@has_condition | @has_procedure | @has_drug | @has_device | @has_measurement | @has_observation} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
       'Visit' AS domain_id
      FROM @cdm_database_schema.visit_occurrence
      WHERE visit_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    )
    SELECT
    {@has_condition} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Condition' THEN person_id
      END) AS condition_persons,
    }
    {@has_procedure} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Procedure' THEN person_id
      END) AS procedure_persons,
    }
    {@has_drug} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Drug' THEN person_id
      END) AS drug_persons,
    }
    {@has_device} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Device' THEN person_id
      END) AS device_persons,
    }
    {@has_measurement} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Measurement' THEN person_id
      END) AS measurement_persons,
    }
    {@has_observation} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Observation' THEN person_id
      END) AS observation_persons,
    }
    {@has_visit} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Visit' THEN person_id
      END) AS visit_persons,
    }
      COUNT(DISTINCT person_id) AS overall_persons
      FROM domain_persons;
  "
  counts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cdm_database_schema = cdmDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    has_condition = "Condition" %in% domains,
    has_procedure = "Procedure" %in% domains,
    has_drug = "Drug" %in% domains,
    has_device = "Device" %in% domains,
    has_measurement = "Measurement" %in% domains,
    has_observation = "Observation" %in% domains,
    has_visit = "Visit" %in% domains,
    snakeCaseToCamelCase = TRUE
  )
  return(counts)
}

removeConceptSetsWithExistingCounts <- function(uniqueConceptSets,
                                                databaseId,
                                                pegasusConnection,
                                                pegasusDatabaseSchema) {
  if (is.null(pegasusConnection)) {
    return(uniqueConceptSets)
  } else {
    sql <- "
      SELECT concept_set_id
      FROM @database_schema.concept_set_person_count
      WHERE database_id = @database_id;
    "
    existingConceptSetIds <- DatabaseConnector::renderTranslateQuerySql(
      connection = pegasusConnection,
      sql = sql,
      database_schema = pegasusDatabaseSchema,
      database_id = databaseId
    )[, 1]
    conceptSetsToCount <- uniqueConceptSets |>
      filter(!.data$conceptSetId %in% existingConceptSetIds)
    message(sprintf("- Skipping %d concept set that already have counts",
                    nrow(uniqueConceptSets) - nrow(conceptSetsToCount)))
    return(conceptSetsToCount)
  }
}

