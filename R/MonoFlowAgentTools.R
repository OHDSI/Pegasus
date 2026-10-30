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

requiredDatabaseColumns <- c(
  "databaseName",
  "databaseDescription",
  "cdmDatabaseSchema",
  "referenceCohortDatabaseSchema",
  "referenceCohortTable",
  "referenceCohortProfilesTable",
  "cohortDatabaseSchema",
  "cohortTable",
  "conceptSetTable",
  "conceptSetDefinitionTable"
)

PegasusAgentContext <- R6::R6Class(
  "PegasusAgentContext",
  public = list(
    connectionDetails = NULL,
    databases = NULL,
    conceptSetDatabaseSchema = NULL,
    conceptSetExpressionsPlusTable = NULL,
    phenotypeToConceptSetNameTable = NULL,
    tempEmulationSchema = NULL,
    standardConceptSets = NULL,
    connectionPool = NULL,

    initialize = function(connectionDetails,
                          databases,
                          conceptSetDatabaseSchema,
                          conceptSetExpressionsPlusTable = "concept_set_expression_plus",
                          phenotypeToConceptSetNameTable = "phenotype_to_concept_set",
                          tempEmulationSchema = NULL,
                          standardConceptSets = NULL) {
      checkmate::assertClass(connectionDetails, "ConnectionDetails")
      checkmate::assertDataFrame(databases, min.rows = 1)
      checkmate::assertNames(names(databases), must.include = requiredDatabaseColumns)
      checkmate::assertCharacter(databases$databaseName, any.missing = FALSE, unique = TRUE)
      checkmate::assertString(conceptSetDatabaseSchema)
      checkmate::assertString(conceptSetExpressionsPlusTable)
      checkmate::assertString(phenotypeToConceptSetNameTable)
      checkmate::assertString(tempEmulationSchema, null.ok = TRUE)
      checkmate::assertDataFrame(standardConceptSets, null.ok = TRUE)

      self$connectionDetails <- connectionDetails
      self$databases <- as_tibble(databases)
      self$conceptSetDatabaseSchema <- conceptSetDatabaseSchema
      self$conceptSetExpressionsPlusTable <- conceptSetExpressionsPlusTable
      self$phenotypeToConceptSetNameTable <- phenotypeToConceptSetNameTable
      self$tempEmulationSchema <- tempEmulationSchema
      self$standardConceptSets <- if (is.null(standardConceptSets)) tibble() else as_tibble(standardConceptSets)
      self$connectionPool <- pool::poolCreate(
        factory = function() {
          DatabaseConnector::connect(connectionDetails)
        }
      )
    },

    withConnection = function(fun) {
      connection <- pool::poolCheckout(self$connectionPool)
      on.exit(pool::poolReturn(connection))
      return(fun(connection))
    },

    close = function() {
      if (!is.null(self$connectionPool) && pool::dbIsValid(self$connectionPool)) {
        pool::poolClose(self$connectionPool)
      }
      invisible(NULL)
    },

    tools = function() {
      return(createAgentTools(self))
    }
  ),
  private = list(
    finalize = function() {
      try(self$close(), silent = TRUE)
    }
  )
)

# Support functions ----------------------------------------------------------------------------------------------------
# Query and execute through the context's pool, always passing the temp emulation schema explicitly.
queryContext <- function(context, sql, ...) {
  DatabaseConnector::renderTranslateQuerySql(
    connection = context$connectionPool,
    sql = sql,
    tempEmulationSchema = context$tempEmulationSchema,
    snakeCaseToCamelCase = TRUE,
    ...
  )
}

executeContext <- function(context, sql, ...) {
  DatabaseConnector::renderTranslateExecuteSql(
    connection = context$connectionPool,
    sql = sql,
    tempEmulationSchema = context$tempEmulationSchema,
    ...
  )
}

runQuietly <- function(expr) {
  outputFile <- tempfile(fileext = ".log")
  messageFile <- tempfile(fileext = ".log")
  outputCon <- file(outputFile, open = "wt")
  messageCon <- file(messageFile, open = "wt")
  on.exit({
    sink(type = "message")
    sink()
    close(messageCon)
    close(outputCon)
    unlink(c(outputFile, messageFile))
  }, add = TRUE)
  sink(outputCon)
  sink(messageCon, type = "message")
  force(expr)
}

# Returns the rows of `databases` matching the requested names (NULL means all databases).
selectDatabases <- function(context, database = NULL) {
  databases <- context$databases
  if (is.null(database) || length(database) == 0L) {
    return(databases)
  }
  database <- as.character(unlist(database))
  unknown <- setdiff(database, databases$databaseName)
  if (length(unknown) > 0L) {
    stop("Unknown database(s): ", paste(unknown, collapse = ", "),
         ". Valid databases: ", paste(databases$databaseName, collapse = ", "))
  }
  return(databases[match(database, databases$databaseName), ])
}

# Applies a function to each database row and row-binds the resulting data frames.
collectPerDatabase <- function(databaseRows, fun) {
  results <- lapply(seq_len(nrow(databaseRows)), function(i) fun(databaseRows[i, ]))
  return(bind_rows(results))
}

# Looks up the cohort in one database's cohort table. Returns matching ID (NA if none) and the IDs in use.
findCohortInDatabase <- function(connection, databaseRow, checksum) {
  cohortTableNames <- CohortGenerator::getCohortTableNames(databaseRow$cohortTable)
  if (!DatabaseConnector::existsTable(connection, databaseRow$cohortDatabaseSchema, databaseRow$cohortTable)) {
    return(list(matchingId = NA_integer_, existingIds = integer(0)))
  }
  existingCohorts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT cohort_definition_id, checksum FROM @cohort_database_schema.@table;",
    cohort_database_schema = databaseRow$cohortDatabaseSchema,
    table = cohortTableNames$cohortChecksumTable,
    snakeCaseToCamelCase = TRUE
  )
  matchingIds <- existingCohorts |>
    filter(.data$checksum == !!checksum) |>
    pull(.data$cohortDefinitionId)
  if (length(matchingIds) > 1) {
    stop("Cohort is stored under multiple IDs in database '", databaseRow$databaseName, "': ",
         paste(matchingIds, collapse = ", "))
  }
  return(list(
    matchingId = if (length(matchingIds) == 1) as.integer(matchingIds) else NA_integer_,
    existingIds = as.integer(existingCohorts$cohortDefinitionId)
  ))
}

# Returns the cohort ID, which is identical in all selected databases:
ensureCohortExists <- function(context, json, databaseRows) {
  # Cannot call DatabaseConnector::existsTable() on a connection pool object for some reason
  context$withConnection(function(connection) {
    ensureCohortExistsWithConnection(context, connection, json, databaseRows)
  })
}

ensureCohortExistsWithConnection <- function(context, connection, json, databaseRows) {
  expression <- CirceR::cohortExpressionFromJson(json)
  sql <- CirceR::buildCohortQuery(expression, CirceR::createGenerateOptions(generateStats = TRUE))
  checksum <- CohortGenerator::computeChecksum(sql)

  lookups <- lapply(seq_len(nrow(databaseRows)), function(i) {
    findCohortInDatabase(connection, databaseRows[i, ], checksum)
  })
  matchingIds <- vapply(lookups, function(lookup) lookup$matchingId, integer(1))
  foundIds <- unique(matchingIds[!is.na(matchingIds)])

  if (length(foundIds) > 1) {
    stop("Cohort has inconsistent IDs across databases: ",
         paste(sprintf("%s = %s", databaseRows$databaseName[!is.na(matchingIds)], matchingIds[!is.na(matchingIds)]),
               collapse = ", "))
  }
  if (length(foundIds) == 1) {
    cohortId <- foundIds
    for (i in which(is.na(matchingIds))) {
      if (cohortId %in% lookups[[i]]$existingIds) {
        stop("Cohort ID ", cohortId, " is already used by a different cohort in database '",
             databaseRows$databaseName[i], "'")
      }
    }
  } else {
    allIds <- unlist(lapply(lookups, function(lookup) lookup$existingIds))
    cohortId <- if (length(allIds) == 0) 1L else max(allIds) + 1L
  }

  for (i in which(is.na(matchingIds))) {
    databaseRow <- databaseRows[i, ]
    cohortTableNames <- CohortGenerator::getCohortTableNames(databaseRow$cohortTable)
    if (!DatabaseConnector::existsTable(connection, databaseRow$cohortDatabaseSchema, databaseRow$cohortTable)) {
      CohortGenerator::createCohortTables(
        connection = connection,
        cohortDatabaseSchema = databaseRow$cohortDatabaseSchema,
        cohortTableNames = cohortTableNames
      )
    }
    cohortDefinitionSet <- tibble(
      cohortId = cohortId,
      cohortName = paste("Cohort", cohortId),
      sql = sql,
      json = json
    )
    runQuietly(
      CohortGenerator::generateCohortSet(
        connection = connection,
        cdmDatabaseSchema = databaseRow$cdmDatabaseSchema,
        cohortDatabaseSchema = databaseRow$cohortDatabaseSchema,
        cohortTableNames = cohortTableNames,
        cohortDefinitionSet = cohortDefinitionSet,
        incremental = TRUE,
        tempEmulationSchema = context$tempEmulationSchema
      )
    )
    runQuietly(
      CohortGenerator::insertInclusionRuleNames(
        connection = connection,
        cohortDatabaseSchema = databaseRow$cohortDatabaseSchema,
        cohortDefinitionSet = cohortDefinitionSet,
        cohortInclusionTable = cohortTableNames$cohortInclusionTable
      )
    )
  }
  return(cohortId)
}

# Compile a client-supplied Capr cohort definition (R text) to Circe JSON in an isolated,
# credential-less process (Option A1 + compile-worker split). The worker validates the code
# against a strict allow-list before evaluating it; this main process - which holds the CDM
# credentials - only ever receives the compiled JSON back, and never runs the client code.
#
# Deployment note: on RStudio Connect, additionally run the worker under an OS sandbox
# (no network egress, read-only filesystem, non-root UID, CPU/memory/wall-clock limits). The
# R-level allow-list in compileWorker.R is defense-in-depth, not a complete boundary.
compileCaprViaWorker <- function(caprCode, timeoutSeconds = 60) {
  workerPath <- system.file("CompileWorker.R", package = "Pegasus")
  callr::r(
    func = function(code, worker) {
      suppressPackageStartupMessages(loadNamespace("Capr"))
      workerEnv <- new.env()
      source(worker, local = workerEnv)
      workerEnv$validateAndCompileCapr(code)
    },
    args = list(code = caprCode, worker = workerPath),
    timeout = timeoutSeconds,
    env = callr::rcmd_safe_env(),
    show = FALSE
  )
}

compileCaprConceptSetsViaWorker <- function(caprCode, timeoutSeconds = 60) {
  workerPath <- system.file("CompileWorker.R", package = "Pegasus")
  callr::r(
    func = function(code, worker) {
      suppressPackageStartupMessages(loadNamespace("Capr"))
      workerEnv <- new.env()
      source(worker, local = workerEnv)
      workerEnv$validateAndCompileConceptSets(code)
    },
    args = list(code = caprCode, worker = workerPath),
    timeout = timeoutSeconds,
    env = callr::rcmd_safe_env(),
    show = FALSE
  )
}

ensureConceptSetTablesExist <- function(context, databaseRow) {
  sql <- "
    CREATE TABLE IF NOT EXISTS @cohort_database_schema.@definition_table (
      concept_set_hash STRING,
      concept_set_name STRING,
      concept_set_json STRING
    );

    CREATE TABLE IF NOT EXISTS @cohort_database_schema.@concept_set_table (
      concept_set_hash STRING,
      concept_id BIGINT
    );
  "
  executeContext(
    context,
    sql = sql,
    cohort_database_schema = databaseRow$cohortDatabaseSchema,
    definition_table = databaseRow$conceptSetDefinitionTable,
    concept_set_table = databaseRow$conceptSetTable,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}

quoteSqlString <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

formatPipeTable <- function(data, zeroAsBlank = character()) {
  escapeCell <- function(value, blankZero = FALSE) {
    if (length(value) == 0L || is.na(value) || (blankZero && value == 0)) {
      return("")
    }
    text <- as.character(value)
    text <- gsub("|", "\\|", text, fixed = TRUE)
    gsub("[\r\n]+", "<br>", text)
  }

  columns <- names(data)
  rendered <- lapply(columns, function(column) {
    vapply(data[[column]], escapeCell, character(1), blankZero = column %in% zeroAsBlank)
  })
  names(rendered) <- columns
  rendered <- as.data.frame(rendered, stringsAsFactors = FALSE, check.names = FALSE)

  lines <- c(
    paste0("| ", paste(columns, collapse = " | "), " |"),
    paste0("| ", paste(rep("---", length(columns)), collapse = " | "), " |")
  )
  if (nrow(rendered) > 0L) {
    lines <- c(
      lines,
      apply(rendered, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
    )
  }
  paste(lines, collapse = "\n")
}

ensureConceptSetsExist <- function(context, conceptSetsToCreate, databaseRow) {
  ensureConceptSetTablesExist(context, databaseRow)
  conceptSetsToCreate$conceptSetHash <- vapply(
    conceptSetsToCreate$json,
    CohortGenerator::computeChecksum,
    character(1)
  )

  existingHashes <- queryContext(
    context,
    sql = "SELECT concept_set_hash FROM @cohort_database_schema.@definition_table;",
    cohort_database_schema = databaseRow$cohortDatabaseSchema,
    definition_table = databaseRow$conceptSetDefinitionTable
  ) |>
    pull(.data$conceptSetHash)

  newConceptSetRows <- which(!conceptSetsToCreate$conceptSetHash %in% existingHashes)
  for (rowIndex in newConceptSetRows) {
    conceptSetHash <- conceptSetsToCreate$conceptSetHash[rowIndex]
    conceptSetSql <- CirceR::buildConceptSetQuery(conceptSetsToCreate$json[rowIndex])
    sql <- "
      MERGE INTO @cohort_database_schema.@concept_set_table AS target
      USING (
        SELECT @concept_set_hash AS concept_set_hash, concept_id
        FROM (
          @concept_set_sql
        ) concept_set
      ) AS source
      ON target.concept_set_hash = source.concept_set_hash
        AND target.concept_id = source.concept_id
      WHEN NOT MATCHED THEN
        INSERT (concept_set_hash, concept_id)
        VALUES (source.concept_set_hash, source.concept_id);

      MERGE INTO @cohort_database_schema.@definition_table AS target
      USING (
        SELECT @concept_set_hash AS concept_set_hash,
          @concept_set_name AS concept_set_name,
          @concept_set_json AS concept_set_json
      ) AS source
      ON target.concept_set_hash = source.concept_set_hash
      WHEN NOT MATCHED THEN
        INSERT (concept_set_hash, concept_set_name, concept_set_json)
        VALUES (source.concept_set_hash, source.concept_set_name, source.concept_set_json);
    "
    executeContext(
      context,
      sql = sql,
      cohort_database_schema = databaseRow$cohortDatabaseSchema,
      concept_set_table = databaseRow$conceptSetTable,
      definition_table = databaseRow$conceptSetDefinitionTable,
      concept_set_hash = quoteSqlString(conceptSetHash),
      concept_set_name = quoteSqlString(conceptSetsToCreate$name[rowIndex]),
      concept_set_json = quoteSqlString(conceptSetsToCreate$json[rowIndex]),
      concept_set_sql = SqlRender::render(
        conceptSetSql,
        vocabulary_database_schema = databaseRow$cdmDatabaseSchema
      ),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }
  return(conceptSetsToCreate)
}

getKeeperReferenceCohortId <- function(context, phenotype, databaseRow) {
  sql <- "
    SELECT cohort_definition_id
    FROM @database_schema.@table
    WHERE LOWER(phenotype) = LOWER(@phenotype);
  "
  keeperReference <- queryContext(
    context,
    sql = sql,
    database_schema = databaseRow$referenceCohortDatabaseSchema,
    table = Keeper::createReferenceCohortTableNames(databaseRow$referenceCohortTable)$referenceCohortMetadataTable,
    phenotype = quoteSqlString(phenotype)
  )
  if (nrow(keeperReference) == 0) {
    stop("Could not find reference cohort for phenotype ", phenotype, " in database '", databaseRow$databaseName, "'")
  }
  return(keeperReference$cohortDefinitionId)
}

# Tool functions --------------------------------------------------------------------------------------------------------
listConceptSets <- function(context, phenotype) {
  sql <- "
    SELECT DISTINCT concept_set_expression.*
    FROM @database_schema.@concept_set_expression_plus_table concept_set_expression
    INNER JOIN @database_schema.@phenotype_to_concept_set_table phenotype_to_concept_set
      ON phenotype_to_concept_set.concept_set_name = concept_set_expression.concept_set_name
        AND phenotype_to_concept_set.hypernym = concept_set_expression.hypernym
    WHERE LOWER(phenotype) = LOWER(@phenotype);
  "
  conceptSets <- queryContext(
    context,
    sql = sql,
    database_schema = context$conceptSetDatabaseSchema,
    phenotype_to_concept_set_table = context$phenotypeToConceptSetNameTable,
    concept_set_expression_plus_table = context$conceptSetExpressionsPlusTable,
    phenotype = quoteSqlString(phenotype)
  )

  subset <- conceptSets |>
    bind_rows(context$standardConceptSets) |>
    select("conceptSetName",
           "hypernym",
           "overallPersons") |>
    arrange(.data$conceptSetName)

  table <- c("| conceptsetName | withDescendants | personCount |",
             "| -------------- | ------------------ | ----------- |",
             sprintf("| %s | %s | %d |",
                     subset$conceptSetName,
                     if_else(subset$hypernym == 1, "N", "Y"),
                     subset$overallPersons))
  table <- paste0(table, collapse = "\n")
  return(table)
}

getConceptSetsCapr <- function(context, phenotype, conceptSetNames, detail = "code_and_counts") {
  if (!detail %in% c("code", "code_and_counts", "full_reference")) {
    stop("The detail argument should be 'code', 'code_and_counts', or 'full_reference'")
  }
  sql <- "
    SELECT DISTINCT concept_set_expression.*
    FROM @database_schema.@concept_set_expression_plus_table concept_set_expression
    INNER JOIN @database_schema.@phenotype_to_concept_set_table phenotype_to_concept_set
      ON phenotype_to_concept_set.concept_set_name = concept_set_expression.concept_set_name
        AND phenotype_to_concept_set.hypernym = concept_set_expression.hypernym
    WHERE LOWER(phenotype) = LOWER(@phenotype)
      AND LOWER(concept_set_expression.concept_set_name) IN (@concept_set_names);
  "
  conceptSets <- queryContext(
    context,
    sql = sql,
    database_schema = context$conceptSetDatabaseSchema,
    phenotype_to_concept_set_table = context$phenotypeToConceptSetNameTable,
    concept_set_expression_plus_table = context$conceptSetExpressionsPlusTable,
    phenotype = quoteSqlString(phenotype),
    concept_set_names = paste(tolower(quoteSqlString(conceptSetNames)),     collapse = ", ")
  )
  caprWithReference <- conceptSets |>
    bind_rows(context$standardConceptSets) |>
    filter(tolower(.data$conceptSetName) %in% tolower(conceptSetNames))

  # Maintain order of input:
  caprWithReference <- caprWithReference |>
    mutate(lcConceptSetName = tolower(.data$conceptSetName)) |>
    inner_join(tibble(lcConceptSetName = tolower(conceptSetNames),
                      order = seq_along(conceptSetNames)), by = join_by("lcConceptSetName")) |>
    arrange(order) |>
    select(-"order", -"lcConceptSetName")

  columnsToInclude <- c("capr")
  if (detail %in% c("code_and_counts", "full_reference")) {
    columnsToInclude <- c(columnsToInclude, "conditionPersons", "procedurePersons", "drugPersons", "measurementPersons", "observationPersons", "visitPersons")    }
  if (detail == "full_reference") {
    columnsToInclude <- c(columnsToInclude, "conceptReference")
    caprWithReference <- caprWithReference |>
      mutate(conceptReference = paste("dummy", row_number()))
  }
  result <- caprWithReference[, columnsToInclude]
  json <- jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE)

  json <- gsub(",\n  }", "\n  }", gsub('\n[^:]+Persons": 0,?', "", json))

  if (detail == "full_reference") {
    for (i in seq_len(nrow(caprWithReference))) {
      json <- gsub(sprintf('"dummy %s"', i), caprWithReference$reference[i], json)
    }
  }
  return(json)
}

validateCapr <- function(context, caprCode) {
  result <- tryCatch({
    compileCaprViaWorker(caprCode)
    "Valid"
  },
  error = function(e) {
    return(paste("Error:", e$parent$message))
  }
  )
  return(result)
}

convertCaprToJson <- function(context, caprCode, database = NULL) {
  # Only the vocabulary is needed, so the first selected database suffices
  databaseRow <- selectDatabases(context, database)[1, ]
  json <- compileCaprViaWorker(caprCode)

  conceptIdMatches <- regmatches(json, gregexpr('"CONCEPT_ID"\\s*:\\s*\\d+', json))[[1]]
  conceptIds <- unique(as.integer(sub('.*:\\s*', '', conceptIdMatches)))
  sql <- "
    SELECT *
    FROM @cdm_database_schema.concept
    WHERE concept_id IN (@concept_ids);
  "
  concepts <- queryContext(
    context,
    sql = sql,
    cdm_database_schema = databaseRow$cdmDatabaseSchema,
    concept_ids = conceptIds
  )
  # Circe JSON uses upper snake case field names
  colnames(concepts) <- toupper(SqlRender::camelCaseToSnakeCase(colnames(concepts)))
  concepts <- concepts |>
    mutate(STANDARD_CONCEPT_CAPTION = if_else(.data$STANDARD_CONCEPT == "S",
                                              "Standard",
                                              if_else(.data$STANDARD_CONCEPT == "C",
                                                      "Classification",
                                                      "Non-Standard")))
  for (concept in split(concepts, concepts$CONCEPT_ID)) {
    conceptJson <- jsonlite::toJSON(concept)
    conceptJson <- gsub('\\]$', '', gsub('^\\[', '"concept": ', conceptJson))
    json <- gsub(paste0('"concept":\\s*\\{[^}]*"CONCEPT_ID"\\s*:\\s*',concept$CONCEPT_ID,'[^}]*\\}'),
                 conceptJson,
                 json)
  }

  return(json)
}

generateCohort <- function(context, caprCode, database = NULL) {
  json <- compileCaprViaWorker(caprCode)
  cohortId <- ensureCohortExists(context, json, selectDatabases(context, database))
  return(cohortId)
}

getCohortCount <- function(context, cohortId, database = NULL) {
  counts <- collectPerDatabase(selectDatabases(context, database), function(databaseRow) {
    getCohortCountForDatabase(context, cohortId, databaseRow)
  })
  json <- jsonlite::toJSON(counts)
  return(json)
}

getCohortCountForDatabase <- function(context, cohortId, databaseRow) {
  cohortDatabaseSchema <- databaseRow$cohortDatabaseSchema
  cohortTable <- databaseRow$cohortTable
  sql <- "
      SELECT *
      FROM @cohort_database_schema.@table
      WHERE cohort_definition_id = @cohort_id;
    "
  inclusionRules <- queryContext(
    context,
    sql = sql,
    cohort_database_schema = cohortDatabaseSchema,
    table = CohortGenerator::getCohortTableNames(cohortTable)$cohortInclusionTable,
    cohort_id = cohortId
  )

  if (nrow(inclusionRules) > 0) {
    sql <- "
      SELECT *
      FROM @cohort_database_schema.@table
      WHERE cohort_definition_id = @cohort_id;
    "
    inclusionResults <- queryContext(
      context,
      sql = sql,
      cohort_database_schema = cohortDatabaseSchema,
      table = CohortGenerator::getCohortTableNames(cohortTable)$cohortInclusionResultTable,
      cohort_id = cohortId
    )
    cohortInclusionStats <- queryContext(
      context,
      sql = sql,
      cohort_database_schema = cohortDatabaseSchema,
      table = CohortGenerator::getCohortTableNames(cohortTable)$cohortInclusionStatsTable,
      cohort_id = cohortId
    )
    inclusionRules <- bind_rows(inclusionRules,
                                tibble(cohortDefinitionId = cohortId,
                                       ruleSequence = -1,
                                       name = "Initial event"))
    counts <- CohortGenerator::computeCohortAttrition(inclusionResults, inclusionRules) |>
      filter(.data$modeId == 0, .data$cohortEntry == 0) |>
      right_join(inclusionRules, by = join_by("cohortDefinitionId", "ruleSequence")) |>
      select("ruleSequence", "name", incrementalPersons = "personCount")

    counts <- counts |>
      left_join(cohortInclusionStats |>
                  filter(.data$modeId == 0) |>
                  select("ruleSequence", marginalPerson = "personCount", "gainCount"),
                by = join_by("ruleSequence")) |>
      mutate(ruleSequence  = .data$ruleSequence + 1) |>
      mutate(incrementalPersons = if_else(is.na(.data$incrementalPersons), 0, .data$incrementalPersons),
             marginalPerson = if_else(is.na(.data$marginalPerson) & .data$ruleSequence != 0, 0, .data$marginalPerson),
             gainCount = if_else(is.na(.data$gainCount) & .data$ruleSequence != 0, 0, .data$gainCount)) |>
      arrange(.data$ruleSequence)
  } else {
    sql <- "
      SELECT COUNT(DISTINCT subject_id) AS person_count,
        COUNT(*) AS entry_count
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @cohort_id;
    "
    counts <- queryContext(
      context,
      sql = sql,
      cohort_database_schema = cohortDatabaseSchema,
      cohort_table = cohortTable,
      cohort_id = cohortId
    )
  }
  counts <- counts |>
    mutate(database = databaseRow$databaseName)
  return(counts)
}

getDatabaseDescription <- function(context, databaseName = NULL) {
  selected <- selectDatabases(context, databaseName)
  if (nrow(selected) == 1L) {
    return(selected$databaseDescription)
  }
  return(paste(sprintf("%s: %s", selected$databaseName, selected$databaseDescription), collapse = "\n\n"))
}

countConceptSetPersonOverlap <- function(
    context,
    caprCode,
    cohortId,
    database = NULL,
    timeWindows = list(
      list(name = "Prior year", startDay = -365L, endDay = -31L),
      list(name = "Prior month", startDay = -30L, endDay = -1L),
      list(name = "Index date", startDay = 0L, endDay = 0L),
      list(name = "Following month", startDay = 1L, endDay = 30L),
      list(name = "Following year", startDay = 31L, endDay = 365L)
    )) {
  if (is.atomic(timeWindows) && length(timeWindows) > 0L) {
    if (length(timeWindows) %% 3L != 0L) {
      stop("Each time window must contain name, startDay, and endDay")
    }
    timeWindows <- lapply(seq.int(1L, length(timeWindows), by = 3L), function(fieldIndex) {
      list(
        name = as.character(timeWindows[[fieldIndex]]),
        startDay = suppressWarnings(as.numeric(timeWindows[[fieldIndex + 1L]])),
        endDay = suppressWarnings(as.numeric(timeWindows[[fieldIndex + 2L]]))
      )
    })
  }
  if (is.data.frame(timeWindows)) {
    timeWindows <- lapply(seq_len(nrow(timeWindows)), function(rowIndex) {
      as.list(timeWindows[rowIndex, , drop = FALSE])
    })
  }
  if (!is.list(timeWindows) || length(timeWindows) < 1L) {
    stop("timeWindows must contain at least one time window")
  }
  requiredWindowFields <- c("name", "startDay", "endDay")
  if (any(!vapply(timeWindows, function(window) {
    is.list(window) && all(requiredWindowFields %in% names(window))
  }, logical(1)))) {
    stop("Each time window must contain name, startDay, and endDay")
  }
  windowsToCount <- tibble(
    inputOrder = seq_along(timeWindows),
    name = vapply(timeWindows, `[[`, character(1), "name"),
    startDay = vapply(timeWindows, `[[`, numeric(1), "startDay"),
    endDay = vapply(timeWindows, `[[`, numeric(1), "endDay")
  )
  if (any(!nzchar(windowsToCount$name)) || anyDuplicated(windowsToCount$name)) {
    stop("Each time window must have a non-empty, unique name")
  }
  if (any(!is.finite(windowsToCount$startDay)) ||
      any(!is.finite(windowsToCount$endDay)) ||
      any(windowsToCount$startDay != as.integer(windowsToCount$startDay)) ||
      any(windowsToCount$endDay != as.integer(windowsToCount$endDay)) ||
      any(windowsToCount$startDay > windowsToCount$endDay)) {
    stop("Time window bounds must be finite integers with startDay less than or equal to endDay")
  }

  compiledConceptSets <- compileCaprConceptSetsViaWorker(caprCode)
  conceptSetsToCount <- tibble(
    inputOrder = seq_along(compiledConceptSets),
    name = vapply(compiledConceptSets, `[[`, character(1), "name"),
    json = vapply(compiledConceptSets, `[[`, character(1), "json")
  )
  if (any(!nzchar(conceptSetsToCount$name)) || anyDuplicated(conceptSetsToCount$name)) {
    stop("Each concept set must have a non-empty, unique name")
  }

  counts <- collectPerDatabase(selectDatabases(context, database), function(databaseRow) {
    countConceptSetPersonOverlapForDatabase(context, conceptSetsToCount, windowsToCount, cohortId, databaseRow)
  })

  identityColumns <- c("database", "conceptSetName", "windowName", "startDay", "endDay")
  overallColumns <- c("overallPersons", "overallCohortPersons")
  domainColumns <- intersect(
    c(
      "conditionPersons", "conditionCohortPersons",
      "procedurePersons", "procedureCohortPersons",
      "drugPersons", "drugCohortPersons",
      "measurementPersons", "measurementCohortPersons",
      "observationPersons", "observationCohortPersons",
      "visitPersons", "visitCohortPersons"
    ),
    names(counts)
  )
  nonZeroDomainColumns <- domainColumns[vapply(
    counts[domainColumns],
    function(column) any(!is.na(column) & column != 0),
    logical(1)
  )]
  columnsToInclude <- c(identityColumns, nonZeroDomainColumns, overallColumns)
  compactCounts <- counts[, columnsToInclude, drop = FALSE]

  return(formatPipeTable(compactCounts, zeroAsBlank = nonZeroDomainColumns))
}

countConceptSetPersonOverlapForDatabase <- function(context, conceptSetsToCount, windowsToCount, cohortId, databaseRow) {
  cohortDatabaseSchema <- databaseRow$cohortDatabaseSchema
  cohortTable <- databaseRow$cohortTable
  conceptSetTable <- databaseRow$conceptSetTable
  cdmDatabaseSchema <- databaseRow$cdmDatabaseSchema

  conceptSetsToCount <- ensureConceptSetsExist(context, conceptSetsToCount, databaseRow)

  requestedConceptSets <- paste(
    sprintf(
      "SELECT %s AS concept_set_hash, %d AS input_order, %s AS concept_set_name",
      quoteSqlString(conceptSetsToCount$conceptSetHash),
      conceptSetsToCount$inputOrder,
      quoteSqlString(conceptSetsToCount$name)
    ),
    collapse = " UNION ALL "
  )
  requestedWindows <- paste(
    sprintf(
      "SELECT %d AS window_order, %s AS window_name, %d AS start_day, %d AS end_day",
      windowsToCount$inputOrder,
      quoteSqlString(windowsToCount$name),
      as.integer(windowsToCount$startDay),
      as.integer(windowsToCount$endDay)
    ),
    collapse = " UNION ALL "
  )
  sql <- "
    WITH requested_concept_sets AS (
      @requested_concept_sets
    ),
    requested_windows AS (
      @requested_windows
    ),
    cohort_entries AS (
      SELECT DISTINCT subject_id AS person_id, cohort_start_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @cohort_id
    ),
    domain_occurrences AS (
      SELECT DISTINCT requested.concept_set_hash, occurrence.person_id,
        occurrence.condition_start_date AS occurrence_date, 'Condition' AS domain_id
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.condition_occurrence occurrence
        ON concept_set.concept_id = occurrence.condition_concept_id

      UNION ALL

      SELECT DISTINCT requested.concept_set_hash, occurrence.person_id,
        occurrence.procedure_date AS occurrence_date, 'Procedure' AS domain_id
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.procedure_occurrence occurrence
        ON concept_set.concept_id = occurrence.procedure_concept_id

      UNION ALL

      SELECT DISTINCT requested.concept_set_hash, occurrence.person_id,
        occurrence.drug_exposure_start_date AS occurrence_date, 'Drug' AS domain_id
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.drug_exposure occurrence
        ON concept_set.concept_id = occurrence.drug_concept_id

      UNION ALL

      SELECT DISTINCT requested.concept_set_hash, occurrence.person_id,
        occurrence.measurement_date AS occurrence_date, 'Measurement' AS domain_id
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.measurement occurrence
        ON concept_set.concept_id = occurrence.measurement_concept_id

      UNION ALL

      SELECT DISTINCT requested.concept_set_hash, occurrence.person_id,
        occurrence.observation_date AS occurrence_date, 'Observation' AS domain_id
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.observation occurrence
        ON concept_set.concept_id = occurrence.observation_concept_id

      UNION ALL

      SELECT DISTINCT requested.concept_set_hash, occurrence.person_id,
        occurrence.visit_start_date AS occurrence_date, 'Visit' AS domain_id
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.visit_occurrence occurrence
        ON concept_set.concept_id = occurrence.visit_concept_id
    ),
    population_counts AS (
      SELECT concept_set_hash, domain_id, COUNT(DISTINCT person_id) AS person_count
      FROM domain_occurrences
      GROUP BY concept_set_hash, domain_id
    ),
    population_overall_counts AS (
      SELECT concept_set_hash, COUNT(DISTINCT person_id) AS person_count
      FROM domain_occurrences
      GROUP BY concept_set_hash
    ),
    cohort_counts AS (
      SELECT occurrence.concept_set_hash, window.window_order, occurrence.domain_id,
        COUNT(DISTINCT occurrence.person_id) AS person_count
      FROM domain_occurrences occurrence
      INNER JOIN cohort_entries cohort
        ON occurrence.person_id = cohort.person_id
      CROSS JOIN requested_windows window
      WHERE DATEDIFF(DAY, cohort.cohort_start_date, occurrence.occurrence_date)
        BETWEEN window.start_day AND window.end_day
      GROUP BY occurrence.concept_set_hash, window.window_order, occurrence.domain_id
    ),
    cohort_overall_counts AS (
      SELECT occurrence.concept_set_hash, window.window_order,
        COUNT(DISTINCT occurrence.person_id) AS person_count
      FROM domain_occurrences occurrence
      INNER JOIN cohort_entries cohort
        ON occurrence.person_id = cohort.person_id
      CROSS JOIN requested_windows window
      WHERE DATEDIFF(DAY, cohort.cohort_start_date, occurrence.occurrence_date)
        BETWEEN window.start_day AND window.end_day
      GROUP BY occurrence.concept_set_hash, window.window_order
    )
    SELECT requested.concept_set_name,
      window.window_name,
      window.start_day,
      window.end_day,
      COALESCE(MAX(CASE WHEN population.domain_id = 'Condition' THEN population.person_count END), 0) AS condition_persons,
      COALESCE(MAX(CASE WHEN cohort.domain_id = 'Condition' THEN cohort.person_count END), 0) AS condition_cohort_persons,
      COALESCE(MAX(CASE WHEN population.domain_id = 'Procedure' THEN population.person_count END), 0) AS procedure_persons,
      COALESCE(MAX(CASE WHEN cohort.domain_id = 'Procedure' THEN cohort.person_count END), 0) AS procedure_cohort_persons,
      COALESCE(MAX(CASE WHEN population.domain_id = 'Drug' THEN population.person_count END), 0) AS drug_persons,
      COALESCE(MAX(CASE WHEN cohort.domain_id = 'Drug' THEN cohort.person_count END), 0) AS drug_cohort_persons,
      COALESCE(MAX(CASE WHEN population.domain_id = 'Measurement' THEN population.person_count END), 0) AS measurement_persons,
      COALESCE(MAX(CASE WHEN cohort.domain_id = 'Measurement' THEN cohort.person_count END), 0) AS measurement_cohort_persons,
      COALESCE(MAX(CASE WHEN population.domain_id = 'Observation' THEN population.person_count END), 0) AS observation_persons,
      COALESCE(MAX(CASE WHEN cohort.domain_id = 'Observation' THEN cohort.person_count END), 0) AS observation_cohort_persons,
      COALESCE(MAX(CASE WHEN population.domain_id = 'Visit' THEN population.person_count END), 0) AS visit_persons,
      COALESCE(MAX(CASE WHEN cohort.domain_id = 'Visit' THEN cohort.person_count END), 0) AS visit_cohort_persons,
      COALESCE(MAX(population_overall.person_count), 0) AS overall_persons,
      COALESCE(MAX(cohort_overall.person_count), 0) AS overall_cohort_persons
    FROM requested_concept_sets requested
    CROSS JOIN requested_windows window
    LEFT JOIN population_counts population
      ON requested.concept_set_hash = population.concept_set_hash
    LEFT JOIN cohort_counts cohort
      ON requested.concept_set_hash = cohort.concept_set_hash
        AND window.window_order = cohort.window_order
        AND population.domain_id = cohort.domain_id
    LEFT JOIN population_overall_counts population_overall
      ON requested.concept_set_hash = population_overall.concept_set_hash
    LEFT JOIN cohort_overall_counts cohort_overall
      ON requested.concept_set_hash = cohort_overall.concept_set_hash
        AND window.window_order = cohort_overall.window_order
    GROUP BY requested.concept_set_hash, requested.concept_set_name, requested.input_order,
      window.window_name, window.start_day, window.end_day, window.window_order
    ORDER BY requested.input_order, window.window_order;
  "
  counts <- queryContext(
    context,
    sql = sql,
    requested_concept_sets = requestedConceptSets,
    requested_windows = requestedWindows,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id = cohortId,
    concept_set_table = conceptSetTable,
    cdm_database_schema = cdmDatabaseSchema
  )
  counts$database <- databaseRow$databaseName
  return(counts)
}

describeMeasurementValues <- function(context, caprCode, database = NULL) {
  compiledConceptSets <- compileCaprConceptSetsViaWorker(caprCode)
  conceptSetsToDescribe <- tibble(
    inputOrder = seq_along(compiledConceptSets),
    name = vapply(compiledConceptSets, `[[`, character(1), "name"),
    json = vapply(compiledConceptSets, `[[`, character(1), "json")
  )
  if (any(!nzchar(conceptSetsToDescribe$name)) || anyDuplicated(conceptSetsToDescribe$name)) {
    stop("Each concept set must have a non-empty, unique name")
  }

  descriptives <- collectPerDatabase(selectDatabases(context, database), function(databaseRow) {
    describeMeasurementValuesForDatabase(context, conceptSetsToDescribe, databaseRow)
  })
  if (nrow(descriptives) == 0) {
    return("No measurements found for the provided concept set(s)")
  }
  return(jsonlite::toJSON(descriptives, auto_unbox = TRUE, pretty = TRUE, na = "null"))
}

describeMeasurementValuesForDatabase <- function(context, conceptSetsToDescribe, databaseRow) {
  cohortDatabaseSchema <- databaseRow$cohortDatabaseSchema
  conceptSetTable <- databaseRow$conceptSetTable
  cdmDatabaseSchema <- databaseRow$cdmDatabaseSchema

  conceptSetsToDescribe <- ensureConceptSetsExist(context, conceptSetsToDescribe, databaseRow)

  requestedConceptSets <- paste(
    sprintf(
      "SELECT %s AS concept_set_hash, %d AS input_order, %s AS concept_set_name",
      quoteSqlString(conceptSetsToDescribe$conceptSetHash),
      conceptSetsToDescribe$inputOrder,
      quoteSqlString(conceptSetsToDescribe$name)
    ),
    collapse = " UNION ALL "
  )
  sql <- "
    WITH requested_concept_sets AS (
      @requested_concept_sets
    ),
    measurements AS (
      SELECT requested.concept_set_hash,
        requested.concept_set_name,
        requested.input_order,
        COALESCE(measurement.unit_concept_id, 0) AS unit_concept_id,
        measurement.person_id,
        measurement.value_as_number
      FROM requested_concept_sets requested
      INNER JOIN @cohort_database_schema.@concept_set_table concept_set
        ON requested.concept_set_hash = concept_set.concept_set_hash
      INNER JOIN @cdm_database_schema.measurement measurement
        ON concept_set.concept_id = measurement.measurement_concept_id
    ),
    unit_counts AS (
      SELECT concept_set_hash,
        concept_set_name,
        input_order,
        unit_concept_id,
        COUNT(*) AS measurement_count,
        COUNT(DISTINCT person_id) AS person_count,
        SUM(CASE WHEN value_as_number IS NULL THEN 1 ELSE 0 END) AS missing_value_count,
        MIN(value_as_number) AS min_value,
        MAX(value_as_number) AS max_value,
        AVG(value_as_number) AS mean_value,
        STDEV(value_as_number) AS sd_value
      FROM measurements
      GROUP BY concept_set_hash, concept_set_name, input_order, unit_concept_id
    ),
    ranked_values AS (
      SELECT concept_set_hash,
        unit_concept_id,
        value_as_number,
        ROW_NUMBER() OVER (
          PARTITION BY concept_set_hash, unit_concept_id
          ORDER BY value_as_number
        ) AS value_order,
        COUNT(*) OVER (PARTITION BY concept_set_hash, unit_concept_id) AS value_count
      FROM measurements
      WHERE value_as_number IS NOT NULL
    ),
    percentiles AS (
      SELECT concept_set_hash,
        unit_concept_id,
        MIN(CASE WHEN value_order >= 0.10 * value_count THEN value_as_number END) AS p10_value,
        MIN(CASE WHEN value_order >= 0.25 * value_count THEN value_as_number END) AS p25_value,
        MIN(CASE WHEN value_order >= 0.50 * value_count THEN value_as_number END) AS median_value,
        MIN(CASE WHEN value_order >= 0.75 * value_count THEN value_as_number END) AS p75_value,
        MIN(CASE WHEN value_order >= 0.90 * value_count THEN value_as_number END) AS p90_value
      FROM ranked_values
      GROUP BY concept_set_hash, unit_concept_id
    )
    SELECT unit_counts.concept_set_name,
      unit_counts.unit_concept_id,
      CASE
        WHEN unit_counts.unit_concept_id = 0 THEN 'No unit'
        ELSE COALESCE(concept.concept_name, 'Unknown concept')
      END AS unit_concept_name,
      unit_counts.measurement_count,
      unit_counts.person_count,
      unit_counts.missing_value_count,
      unit_counts.min_value,
      percentiles.p10_value,
      percentiles.p25_value,
      percentiles.median_value,
      percentiles.p75_value,
      percentiles.p90_value,
      unit_counts.max_value,
      unit_counts.mean_value,
      unit_counts.sd_value
    FROM unit_counts
    LEFT JOIN percentiles
      ON unit_counts.concept_set_hash = percentiles.concept_set_hash
        AND unit_counts.unit_concept_id = percentiles.unit_concept_id
    LEFT JOIN @cdm_database_schema.concept concept
      ON unit_counts.unit_concept_id = concept.concept_id
    ORDER BY unit_counts.input_order, unit_counts.measurement_count DESC;
  "
  descriptives <- queryContext(
    context,
    sql = sql,
    requested_concept_sets = requestedConceptSets,
    cohort_database_schema = cohortDatabaseSchema,
    concept_set_table = conceptSetTable,
    cdm_database_schema = cdmDatabaseSchema
  )
  descriptives$database <- databaseRow$databaseName
  return(descriptives)
}

computeIncidenceRate <- function(context, cohortId, database = NULL) {
  rates <- collectPerDatabase(selectDatabases(context, database), function(databaseRow) {
    computeIncidenceRateForDatabase(context, cohortId, databaseRow)
  })
  if (nrow(rates) == 0) {
    return("No observation time found in the database")
  }
  return(jsonlite::toJSON(rates, auto_unbox = TRUE, pretty = TRUE, na = "null"))
}

computeIncidenceRateForDatabase <- function(context, cohortId, databaseRow) {
  cohortDatabaseSchema <- databaseRow$cohortDatabaseSchema
  cohortTable <- databaseRow$cohortTable
  cdmDatabaseSchema <- databaseRow$cdmDatabaseSchema
  sql <- "
    WITH first_entry AS (
      SELECT subject_id AS person_id,
        MIN(cohort_start_date) AS cohort_start_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @cohort_id
      GROUP BY subject_id
    ),
    time_at_risk AS (
      SELECT observation_period.person_id,
        CASE
          WHEN YEAR(observation_period.observation_period_start_date) - person.year_of_birth < 0 THEN 0
          ELSE FLOOR((YEAR(observation_period.observation_period_start_date) - person.year_of_birth) / 10) * 10
        END AS age_group_start,
        CASE
          WHEN person.gender_concept_id = 8507 THEN 'Male'
          WHEN person.gender_concept_id = 8532 THEN 'Female'
          ELSE 'Other or unknown'
        END AS sex,
        CASE
          WHEN first_entry.cohort_start_date IS NULL THEN 0
          WHEN first_entry.cohort_start_date < observation_period.observation_period_start_date THEN 0
          WHEN first_entry.cohort_start_date > observation_period.observation_period_end_date THEN 0
          ELSE 1
        END AS event_count,
        DATEDIFF(DAY, observation_period.observation_period_start_date,
          CASE
            WHEN first_entry.cohort_start_date IS NULL THEN observation_period.observation_period_end_date
            WHEN first_entry.cohort_start_date < observation_period.observation_period_start_date THEN observation_period.observation_period_end_date
            WHEN first_entry.cohort_start_date > observation_period.observation_period_end_date THEN observation_period.observation_period_end_date
            ELSE first_entry.cohort_start_date
          END) + 1 AS days_at_risk
      FROM @cdm_database_schema.observation_period observation_period
      INNER JOIN @cdm_database_schema.person person
        ON observation_period.person_id = person.person_id
      LEFT JOIN first_entry
        ON observation_period.person_id = first_entry.person_id
    )
    SELECT 'Overall' AS stratum,
      'Overall' AS stratum_name,
      0 AS stratum_order,
      COUNT(DISTINCT person_id) AS persons,
      SUM(event_count) AS events,
      SUM(days_at_risk) / 365.25 AS person_years
    FROM time_at_risk

    UNION ALL

    SELECT 'Age' AS stratum,
      CONCAT(CAST(age_group_start AS VARCHAR), '-', CAST(age_group_start + 9 AS VARCHAR)) AS stratum_name,
      age_group_start AS stratum_order,
      COUNT(DISTINCT person_id) AS persons,
      SUM(event_count) AS events,
      SUM(days_at_risk) / 365.25 AS person_years
    FROM time_at_risk
    GROUP BY age_group_start

    UNION ALL

    SELECT 'Sex' AS stratum,
      sex AS stratum_name,
      0 AS stratum_order,
      COUNT(DISTINCT person_id) AS persons,
      SUM(event_count) AS events,
      SUM(days_at_risk) / 365.25 AS person_years
    FROM time_at_risk
    GROUP BY sex
    ORDER BY stratum, stratum_order, stratum_name;
  "
  rates <- queryContext(
    context,
    sql = sql,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_id = cohortId,
    cdm_database_schema = cdmDatabaseSchema
  )
  rates <- rates |>
    mutate(incidenceRatePer1000PersonYears = if_else(.data$personYears > 0,
                                                     1000 * .data$events / .data$personYears,
                                                     NA_real_),
           database = databaseRow$databaseName) |>
    select(-"stratumOrder")
  return(rates)
}

evaluateCohort <- function(context, cohortId, phenotype, database = NULL) {
  metrics <- collectPerDatabase(selectDatabases(context, database), function(databaseRow) {
    evaluateCohortForDatabase(context, cohortId, phenotype, databaseRow)
  })
  json <- jsonlite::toJSON(metrics, pretty = TRUE)
  return(json)
}

evaluateCohortForDatabase <- function(context, cohortId, phenotype, databaseRow) {
  referenceCohortDefinitionId <- getKeeperReferenceCohortId(context, phenotype, databaseRow)

  # Cannot call Keeper::computeCohortOperatingCharacteristics() on a connection pool
  metrics <- context$withConnection(function(connection) {
    Keeper::computeCohortOperatingCharacteristics(
      connection = connection,
      cohortDatabaseSchema = databaseRow$cohortDatabaseSchema,
      cohortTable = databaseRow$cohortTable,
      cohortDefinitionId = cohortId,
      referenceCohortDatabaseSchema = databaseRow$referenceCohortDatabaseSchema,
      referenceCohortTableNames = Keeper::createReferenceCohortTableNames(databaseRow$referenceCohortTable),
      referenceCohortDefinitionId = referenceCohortDefinitionId
    )
  })
  metrics <- metrics |>
    select("sensitivity",
           specificity = "specificityOverall",
           "ppv",
           "tp",
           "fp",
           "tn",
           "fn") |>
    mutate(database = databaseRow$databaseName, .before = 1)
  return(metrics)
}

# With several candidate databases, samples from a random one that has patients of the requested type.
samplePatientProfile <- function(context, cohortId, phenotype, type, database = NULL) {

  type <- tolower(type)
  if (!type %in% c("tp", "fp", "tn", "fn")) {
    return("Error: type must have value 'TP', 'FP', 'TN', or 'FN'")
  }

  databaseRows <- selectDatabases(context, database)
  for (i in sample(nrow(databaseRows))) {
    result <- samplePatientProfileForDatabase(context, cohortId, phenotype, type, databaseRows[i, ])
    if (!is.null(result)) {
      return(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE))
    }
  }
  return(sprintf("No patients of type '%s' found", toupper(type)))
}

# Returns NULL when the database has no patients of the requested type.
samplePatientProfileForDatabase <- function(context, cohortId, phenotype, type, databaseRow) {
  referenceCohortDatabaseSchema <- databaseRow$referenceCohortDatabaseSchema
  referenceCohortTable <- databaseRow$referenceCohortTable
  referenceCohortProfilesTable <- databaseRow$referenceCohortProfilesTable
  cohortDatabaseSchema <- databaseRow$cohortDatabaseSchema
  cohortTable <- databaseRow$cohortTable

  referenceCohortDefinitionId <- getKeeperReferenceCohortId(context, phenotype, databaseRow)

  sql <- "
    SELECT CAST(subject_id AS VARCHAR) AS subject_id
    FROM (
      SELECT subject_id,
        is_case,
        MAX(has_match) AS has_match,
        MAX(within_window) as within_window
      FROM (
        SELECT reference_cohort.subject_id,
          is_case,
          CASE WHEN cohort.subject_id IS NULL THEN 0 ELSE 1 END AS has_match,
          CASE
            WHEN DATEDIFF(DAY, reference_cohort.cohort_start_date, cohort.cohort_start_date) <= 30
              AND DATEDIFF(DAY, reference_cohort.cohort_start_date, cohort.cohort_start_date) >= -30
            THEN 1
            ELSE 0
          END AS within_window
        FROM @reference_cohort_database_schema.@reference_cohort_table reference_cohort
        LEFT JOIN @cohort_database_schema.@cohort_table cohort
          ON reference_cohort.subject_id = cohort.subject_id
            AND cohort.cohort_definition_id = @cohort_definition_id
            AND cohort.cohort_start_date >= observation_period_start_date
            AND cohort.cohort_start_date <= observation_period_end_date
        WHERE reference_cohort.cohort_definition_id = @reference_cohort_definition_id
          AND reference_cohort.cohort_start_date IS NOT NULL
      ) tmp
      GROUP BY subject_id,
      is_case
    ) tmp2
    {@type == 'tp'} ? {WHERE is_case = 1 AND has_match = 1 AND within_window = 1;}
    {@type == 'tn'} ? {WHERE is_case = 0 AND has_match = 0;}
    {@type == 'fp'} ? {WHERE is_case = 0 AND has_match = 1 AND within_window = 1;}
    {@type == 'fn'} ? {WHERE is_case = 1 AND has_match = 0;}
  "
  personIds <- queryContext(
    context,
    sql = sql,
    reference_cohort_database_schema = referenceCohortDatabaseSchema,
    reference_cohort_table = referenceCohortTable,
    reference_cohort_definition_id = referenceCohortDefinitionId,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_definition_id = cohortId,
    type = type
  )
  if (nrow(personIds) == 0) {
    return(NULL)
  }
  personId <- sample(personIds$subjectId, size = 1)

  sql <- "
    SELECT *
    FROM @reference_cohort_database_schema.@reference_cohort_profiles_table
    WHERE person_id = @person_id
      AND cohort_definition_id = @reference_cohort_definition_id;
  "
  profile <- queryContext(
    context,
    sql = sql,
    reference_cohort_database_schema = referenceCohortDatabaseSchema,
    reference_cohort_profiles_table = referenceCohortProfilesTable,
    reference_cohort_definition_id = referenceCohortDefinitionId,
    person_id = personId
  )
  result <- list(
    database = databaseRow$databaseName,
    type = toupper(type),
    patientProfile = profile$profile,
    rationale = profile$rationale
  )
  return(result)
}

# Tools ----------------------------------------------------------------------------------------------------------------
# Returns a copy of a tool function that no longer has the context argument, so it can be registered with ellmer.
bindContext <- function(fun, context) {
  toolArguments <- formals(fun)[-1]
  bound <- function() NULL
  formals(bound) <- toolArguments
  forwardedArguments <- lapply(names(toolArguments), as.name)
  names(forwardedArguments) <- names(toolArguments)
  body(bound) <- as.call(c(list(quote(fun), quote(context)), forwardedArguments))
  environment(bound) <- list2env(list(fun = fun, context = context), parent = environment(fun))
  return(bound)
}

createAgentTools <- function(context) {
  return(list(
    listConceptSetsTool(context),
    getConceptSetsCaprTool(context),
    validateCaprTool(context),
    convertCaprToJsonTool(context),
    generateCohortTool(context),
    getCohortCountTool(context),
    getDatabaseDescriptionTool(context),
    countConceptSetPersonOverlapTool(context),
    describeMeasurementValuesTool(context),
    computeIncidenceRateTool(context),
    evaluateCohortTool(context),
    samplePatientProfileTool(context)
  ))
}

listConceptSetsTool <- function(context) ellmer::tool(
  bindContext(listConceptSets, context),
  name = "list_concept_sets",
  description = paste(
    "Retrieve the concept sets associated with a phenotype.",
    "Returns a markdown table with three columns: concept set name, whether the set includes descendants, and the",
    "number of unique persons with at least one of the concepts in the set.",
    "A count of 0 means nobody has any of the concepts."
  ),
  arguments = list(
    phenotype = type_string("Name of the phenotype for which concept sets should be returned.")
  )
)

getConceptSetsCaprTool <- function(context) ellmer::tool(
  bindContext(getConceptSetsCapr, context),
  name = "get_concept_sets_capr",
  description = paste("Returns the Capr R code for one or more concept sets, and unique person counts per domain",
                      "(only non-zero domains)."),
  arguments = list(
    phenotype = type_string("Name of the phenotype."),
    conceptSetNames = type_array(type_string("Names of the concept sets.")),
    detail = type_enum(c("code", "code_and_counts", "full_reference"),
                       paste("Level of detail to return.",
                             "'code' returns the Capr code,",
                             "'code_and_counts' additionally returns person count per domain, and",
                             "'full_reference' also includes a reference for the concept IDs used in the code.")
    )
  )
)

validateCaprTool <- function(context) ellmer::tool(
  bindContext(validateCapr, context),
  name = "validate_capr",
  description = "Validate the provided Capr code. Either returns 'Valid' or an informative error message.",
  arguments = list(
    caprCode = type_string(paste(
      "A single Capr cohort definition as R code: one cohort(...) expression with all concept",
      "sets inlined and no assignments. Compiled server-side - do not pass JSON."
    ))
  )
)

createDatabaseArgument <- function(context) {
  ellmer::type_array(
    ellmer::type_string(),
    description = paste("Names of the databases to use. Defaults to all databases. Available:",
                        paste(context$databases$databaseName, collapse = "; ")),
    required = FALSE
  )
}

convertCaprToJsonTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(convertCaprToJson, context),
  name = "convert_capr_to_json",
  description = paste("Convert Capr code to JSON, including full concept information.",
                      "(This can be a lot of text). Concept details come from the first selected database."),
  arguments = list(
    caprCode = type_string(paste(
      "A single Capr cohort definition as R code: one cohort(...) expression with all concept",
      "sets inlined and no assignments. Compiled server-side - do not pass JSON."
    )),
    database = databaseArgument
  )
)

generateCohortTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(generateCohort, context),
  name = "generate_cohort",
  description = paste("Generate the cohort in the selected database(s) (default: all) and return the cohort ID.",
                      "The same cohort ID is used in every database."),
  arguments = list(
    caprCode = type_string(paste(
      "A single Capr cohort definition as R code: one cohort(...) expression with all concept",
      "sets inlined and no assignments. Compiled server-side - do not pass JSON."
    )),
    database = databaseArgument
  )
)

getCohortCountTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(getCohortCount, context),
  name = "get_cohort_count",
  description = paste("Returns cohort sizes per database. For cohorts with attrition rules, also returns the number of persons",
                      "remaining after each rule is applied (incrementalPersons), the number of persons who satisfy",
                      "both the initial event and the rule (marginalPersons), and the number of persons that would be",
                      "added back if the rule were removed (gainCount)."),
  arguments = list(
    cohortId = type_integer("The cohort ID as returned by the `generate_cohort` tool."),
    database = databaseArgument
  )
)

getDatabaseDescriptionTool <- function(context) ellmer::tool(
  bindContext(getDatabaseDescription, context),
  name = "get_database_description",
  description = "Returns a short description of one or more databases (default: all).",
  arguments = list(
    databaseName = type_string("Name of the database. Omit for all databases.", required = FALSE)
  )
)

countConceptSetPersonOverlapTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(countConceptSetPersonOverlap, context),
  name = "count_concept_set_person_overlap",
  description = paste(
    "Instantiate one or more Capr concept sets and count distinct people with their concepts",
    "in the general population and within inclusive day windows relative to generated cohort",
    "index dates. Returns per-domain and overall counts for each window."
  ),
  arguments = list(
    caprCode = type_array(type_string(paste(
      "A standalone Capr cs(...) expression. Supply one or more expressions; include a",
      "name argument in each expression."
    ))),
    cohortId = type_integer("The cohort ID as returned by the `generate_cohort` tool."),
    database = databaseArgument,
    timeWindows = type_array(
      type_object(
        "An inclusive time window relative to the cohort index date.",
        name = type_string("A unique name for the window."),
        startDay = type_integer("First included day relative to index; negative values are before index."),
        endDay = type_integer("Last included day relative to index; positive values are after index.")
      ),
      description = paste(
        "Time windows to count. Unless you have a specific reason to change them, pass the standard",
        "windows: prior year (-365 to -31), prior month (-30 to -1), index date (0 to 0),",
        "following month (1 to 30), and following year (31 to 365)."
      )
    )
  )
)

describeMeasurementValuesTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(describeMeasurementValues, context),
  name = "describe_measurement_values",
  description = paste(
    "For a measurement concept set, enumerate all units observed in the database and return",
    "descriptives of the measurement value distribution per unit: number of measurements,",
    "number of distinct persons, number of measurements without a value, minimum, 10th, 25th,",
    "50th, 75th and 90th percentile, maximum, mean, and standard deviation. Measurements without",
    "a unit are reported as unit concept ID 0 ('No unit')."
  ),
  arguments = list(
    caprCode = type_array(type_string(paste(
      "A standalone Capr cs(...) expression. Supply one or more expressions; include a",
      "name argument in each expression."
    ))),
    database = databaseArgument
  )
)

computeIncidenceRateTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(computeIncidenceRate, context),
  name = "compute_incidence_rate",
  description = paste(
    "Compute a simple incidence rate for a generated cohort across the entire database population.",
    "Only the first cohort entry per person is counted as an event, and person-time runs from the",
    "start of each observation period until the event or the end of the observation period.",
    "Returns persons, events, person-years, and the incidence rate per 1,000 person-years overall,",
    "stratified by 10-year age group (age at observation period start), and stratified by sex.",
    "Age and sex strata are computed separately, not crossed."
  ),
  arguments = list(
    cohortId = type_integer("The cohort ID as returned by the `generate_cohort` tool."),
    database = databaseArgument
  )
)

evaluateCohortTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(evaluateCohort, context),
  name = "evaluate_cohort",
  description = paste(
    "Evaluate the cohort using a 10,000 person KEEPER reference cohort.",
    "Returns sensitivity, specificity (adjusted for sampling), PPV, and the confusion matrix."
  ),
  arguments = list(
    cohortId = type_integer("The cohort ID as returned by the `generate_cohort` tool."),
    phenotype = type_string("Name of the phenotype."),
    database = databaseArgument
  )
)

samplePatientProfileTool <- function(context, databaseArgument = createDatabaseArgument(context)) ellmer::tool(
  bindContext(samplePatientProfile, context),
  name = "sample_patient_profile",
  description = paste(
    "Return the patient profile and rationale for one random patient in the KEEPER reference set.",
    "If several databases are selected (default: all), a random database with such patients is used.",
    "Call multiple times to sample multiple patients."
  ),
  arguments = list(
    cohortId = type_integer("The cohort ID as returned by the `generate_cohort` tool."),
    phenotype = type_string("Name of the phenotype. Used to fetch the gold standard."),
    type = type_enum(c("TP", "FP", "TN", "FN"), paste(
      "Type, based on classification status, using the KEEPER reference cohort as gold standard.",
      "Options: TP, FP, TN, FN"
    )),
    database = databaseArgument
  )
)
