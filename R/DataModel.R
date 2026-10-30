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

#' Create the data model on the server
#'
#' @template PegasusConnectionDetails
#'
#' @returns
#' Does not return anything. Creates the tables of the data model on the server.
#'
#' @export
createDataModel <- function(pegasusConnectionDetails,
                            pegasusDatabaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)

  connection <- DatabaseConnector::connect(pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  message("Creating data model in schema ", pegasusDatabaseSchema)
  sql <- SqlRender::loadRenderTranslateSql(
    "CreateDataModel.sql",
    packageName = "Pegasus",
    dbms = DatabaseConnector::dbms(connection),
    database_schema = pegasusDatabaseSchema
  )
  DatabaseConnector::executeSql(connection, sql)
}

#' Download phenotypes from the server
#'
#' @template PegasusConnectionDetails
#'
#' @returns
#' A tibble with one row per phenotype, including its ID, name, and clinical definition.
#'
#' @export
downloadPhenotypes <- function(pegasusConnectionDetails,
                               pegasusDatabaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)

  connection <- DatabaseConnector::connect(pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "
    SELECT *
    FROM @database_schema.phenotype;
  "
  phenotypes <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  return(phenotypes)
}

#' Get the current user name
#'
#' @returns
#' A string containing the name of the user in this R session.
#'
#' @export
getUser <- function() {
  user <- Sys.getenv("USERNAME")  # Windows
  if (user == "") {
    user <- Sys.getenv("USER")    # macOS / Linux
  }
  return(user)
}
# Yes, the data model does make a nice contract, allowing them to be separated. Let me think a l

#' Upload phenotypes and their clinical definitions
#'
#' @param phenotypes A tibble with at least two columns: `phenotypeName`, and `clinicalDefinition`. Other allowed
#'                   columns are `creator`, `timeCreated`, `draft`, and `valid`.
#' @param creator The name of the user who created the clinical definitions. Not used when already specified in
#'                `phenotypes`.
#' @param timeCreated The time the clinical definitions were created. Not used when already specified in `phenotypes`.
#' @param draft Whether the clinical definition should be considered a draft (1) or not (0). Not used when already
#'              specified in `phenotypes`.
#' @param valid Whether the entries are still considered valid (1). Not used when already specified in `phenotypes`.
#' @template PegasusConnectionDetails
#'
#' @returns
#' Returns nothing. Is executed for uploading the data to the server.
#'
#' @export
uploadPhenotypes <- function(phenotypes,
                             creator = getUser(),
                             timeCreated = Sys.time(),
                             draft = 0,
                             valid = 1,
                             pegasusConnectionDetails,
                             pegasusDatabaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertCharacter(creator, len = 1, add = errorMessages)
  checkmate::assertPOSIXct(timeCreated, len = 1, add = errorMessages)
  checkmate::assertInt(draft, lower = 0, upper = 1, add = errorMessages)
  checkmate::assertInt(valid, lower = 0, upper = 1, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)

  connection <- DatabaseConnector::connect(pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "
    SELECT phenotype_id,
      phenotype_name
    FROM @database_schema.phenotype;
  "
  existingPhenotypes <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  if (nrow(existingPhenotypes) == 0) {
    lastId <- 0
  } else {
    lastId <- max(existingPhenotypes$phenotypeId)
  }

  overlap <- existingPhenotypes |>
    filter(.data$phenotypeName %in% phenotypes$phenotypeName)
  if (nrow(overlap) > 0) {
    stop(sprintf("Overlapping phenotype names found: '%s'", paste(overlap$phenotypeName, collapse = "', '")))
  }

  if ("phenotypeId" %in% names(phenotypes)) {
    overlap <- existingPhenotypes |>
      filter(.data$phenotypeId %in% phenotypes$phenotypeId)
    if (nrow(overlap) > 0) {
      stop(sprintf("Overlapping phenotype IDs found: %s", paste(overlap$phenotypeId, collapse = ", ")))
    }
  } else {
    phenotypes <- phenotypes |>
      mutate(phenotypeId = row_number() + lastId)
  }
  if (!"creator" %in% names(phenotypes)) {
    phenotypes <- phenotypes |>
      mutate(creator = !!creator)
  }
  if (!"timeCreated" %in% names(phenotypes)) {
    phenotypes <- phenotypes |>
      mutate(timeCreated = !!timeCreated)
  }
  if (!"draft" %in% names(phenotypes)) {
    phenotypes <- phenotypes |>
      mutate(draft = !!draft)
  }
  if (!"valid" %in% names(phenotypes)) {
    phenotypes <- phenotypes |>
      mutate(valid = !!valid)
  }
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = pegasusDatabaseSchema,
    tableName = "phenotype",
    data = phenotypes,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  message(sprintf("Uploaded %d record(s)", nrow(phenotypes)))
}

addPhenotypeIdIfMissing <- function(data, connection, pegasusDatabaseSchema) {
  if (!"phenotypeId" %in% colnames(data)) {
    sql <- "
      SELECT phenotype_id,
        phenotype_name
      FROM @database_schema.phenotype;
    "
    existingPhenotypes <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      database_schema = pegasusDatabaseSchema,
      snakeCaseToCamelCase = TRUE
    )
    missingPhenotypes <- data |>
      distinct(.data$phenotypeName) |>
      filter(!.data$phenotypeName %in% existingPhenotypes$phenotypeName)
    if (nrow(missingPhenotypes) > 0) {
      stop(sprintf("These phenotypes do not yet exist in the phenotype table: '%s'. Please upload them first.",
                   paste(missingPhenotypes$phenotypeName, collapse = "', '")))
    }
    data <- data |>
      inner_join(existingPhenotypes, by = join_by("phenotypeName"))
  }
  return(data)
}

uploadDraftConceptSetTargets <- function(draftConceptSetTargets,
                                         pegasusConnectionDetails,
                                         pegasusDatabaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(draftConceptSetTargets, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(draftConceptSetTargets), must.include = c(
    "phenotypeName",
    "pertains",
    "category",
    "term",
    "conceptId",
    "conceptName",
    "domainId",
    "conceptClassId",
    "vocabularyId",
    "match"
  ), add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)

  connection <- DatabaseConnector::connect(pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "
    SELECT phenotype.phenotype_id,
      phenotype_name,
      pertains,
      category,
      term,
      concept_id,
      concept_name
    FROM @database_schema.draft_concept_set_target
    INNER JOIN @database_schema.phenotype
      ON draft_concept_set_target.phenotype_id = phenotype.phenotype_id;
  "
  existingDraftConceptSetTargets <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  overlap <- existingDraftConceptSetTargets |>
    inner_join(draftConceptSetTargets, by = join_by("phenotypeName", "pertains", "category", "conceptId", "conceptName"))
  if (nrow(overlap) > 0) {
    stop(sprintf("%s rows of data overlap with existing data in the draft_concept_set_target table Aborting",
                 nrow(overlap)))
  }
  draftConceptSetTargets <- addPhenotypeIdIfMissing(draftConceptSetTargets,
                                                    connection = connection,
                                                    pegasusDatabaseSchema = pegasusDatabaseSchema)
  dataToUpload <- draftConceptSetTargets |>
    select("phenotypeId",
           "pertains",
           "category",
           "term",
           "conceptId",
           "conceptName",
           "domainId",
           "conceptClassId",
           "vocabularyId",
           matchType = "match")

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = pegasusDatabaseSchema,
    tableName = "draft_concept_set_target",
    data = dataToUpload,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  message(sprintf("Uploaded %d records", nrow(draftConceptSetTargets)))
}


#' Upload concept sets
#'
#' @param conceptSets A data frame as created by the `generateConceptSets()` function.
#' @param valid Whether the entries are still considered valid (1). Not used when already specified in `conceptSets`.
#' @template PegasusConnectionDetails
#'
#' @returns
#' Returns nothing. Is executed for uploading the data to the server.
#'
#' @export
uploadConceptSets <- function(conceptSets,
                              valid = 1,
                              pegasusConnectionDetails,
                              pegasusDatabaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(conceptSets, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(conceptSets), must.include = c(
    "phenotypeName",
    "pertains",
    "category",
    "conceptId",
    "conceptName",
    "conceptSetId",
    "conceptSetName",
    "clinicalDefinition",
    "sourceType",
    "circeConceptSetExpression",
    "caprConceptSetExpression"
  ), add = errorMessages)
  checkmate::assertInt(valid, lower = 0, upper = 1, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)

  connection <- DatabaseConnector::connect(pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  conceptSets <- addPhenotypeIdIfMissing(conceptSets,
                                         connection = connection,
                                         pegasusDatabaseSchema = pegasusDatabaseSchema)

  # Upload new concept sets to concept_set table
  newConceptSets <- conceptSets |>
    filter(is.na(.data$conceptSetId))
  sql <- "
    SELECT MAX(concept_set_id) AS last_id
    FROM @database_schema.concept_set;
  "
  lastId <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )[1, 1]
  if (is.na(lastId)) {
    lastId <- 0
  }
  newConceptSets <- newConceptSets |>
    mutate(conceptSetId = row_number() + lastId)
  if (!"valid" %in% names(newConceptSets)) {
    newConceptSets <- newConceptSets |>
      mutate(valid = !!valid)
  }
  dataToUpload <- newConceptSets |>
    select("conceptSetId",
           "conceptSetName",
           "circeConceptSetExpression",
           "caprConceptSetExpression",
           "hasDescendants",
           "clinicalDefinition",
           "conceptId",
           "valid",
           csSourceType = "sourceType")
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = pegasusDatabaseSchema,
    tableName = "concept_set",
    data = dataToUpload,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  message(sprintf("Uploaded %d records to the concept_set table", nrow(dataToUpload)))

  # Upload to phenotype_to_concept_set table
  conceptSets <- conceptSets |>
    filter(!is.na(.data$conceptSetId)) |>
    bind_rows(newConceptSets)

  dataToUpload <- conceptSets |>
    select("phenotypeId",
           "pertains",
           "category",
           "conceptSetId")
  sql <- "
    SELECT *
    FROM @database_schema.phenotype_to_concept_set;
  "
  existingData <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  overlap <- dataToUpload |>
    inner_join(existingData, by = join_by("phenotypeId", "pertains", "category", "conceptSetId"))
  if (nrow(overlap) > 0) {
    stop(sprintf("Found %d existing records overlapping with new data in the phenotype_to_concept_set table.",
                 nrow(overlap)))
  }
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = pegasusDatabaseSchema,
    tableName = "phenotype_to_concept_set",
    data = dataToUpload,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  message(sprintf("Uploaded %d records to the phenotype_to_concept_set table", nrow(dataToUpload)))

  # Upload to draft_target_to_concept_set table
  dataToUpload <- conceptSets |>
    select("phenotypeId",
           "pertains",
           "category",
           "conceptId",
           "conceptSetId")
  sql <- "
    SELECT *
    FROM @database_schema.draft_target_to_concept_set;
  "
  existingData <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  overlap <- dataToUpload |>
    inner_join(existingData, by = join_by("phenotypeId", "pertains", "category", "conceptId", "conceptSetId"))
  if (nrow(overlap) > 0) {
    stop(sprintf("Found %d existing records overlapping with new data in the draft_target_to_concept_set table.",
                 nrow(overlap)))
  }
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = pegasusDatabaseSchema,
    tableName = "draft_target_to_concept_set",
    data = dataToUpload,
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  message(sprintf("Uploaded %d records to the draft_target_to_concept_set table", nrow(dataToUpload)))
}
