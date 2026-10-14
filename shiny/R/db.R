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

withConnection <- function(connectionPool, fun) {
  connection <- pool::poolCheckout(connectionPool)
  on.exit(pool::poolReturn(connection))
  fun(connection)
}

getPhenotypes <- function(connection, databaseSchema) {
  sql <- "SELECT phenotype_id,
    phenotype_name,
    clinical_definition,
    creator,
    time_created
  FROM @database_schema.phenotype
  ORDER BY phenotype_name;"
  DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseSchema,
    snakeCaseToCamelCase = TRUE
  )
}

normalizeName <- function(name) {
  tolower(trimws(name))
}

phenotypeNameExists <- function(name, existingNames) {
  normalizeName(name) %in% normalizeName(existingNames)
}

findSimilarNames <- function(name, existingNames) {
  if (length(existingNames) == 0) {
    return(character(0))
  }
  similar <- agrepl(normalizeName(name), normalizeName(existingNames), max.distance = 0.2, fixed = TRUE)
  existingNames[similar]
}

getNextPhenotypeId <- function(connection, databaseSchema) {
  sql <- "SELECT MAX(phenotype_id) AS max_id FROM @database_schema.phenotype;"
  maxId <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseSchema,
    snakeCaseToCamelCase = TRUE
  )$maxId
  if (length(maxId) == 0 || is.na(maxId)) 1 else maxId + 1
}

savePhenotype <- function(connection, databaseSchema, name, clinicalDefinition, creator, parentPhenotypeId = NA_integer_) {
  name <- trimws(name)
  clinicalDefinition <- trimws(clinicalDefinition)
  if (!nzchar(name)) {
    stop("The phenotype name cannot be empty.")
  }
  if (!nzchar(clinicalDefinition)) {
    stop("The clinical definition cannot be empty.")
  }
  existingNames <- getPhenotypes(connection, databaseSchema)$phenotypeName
  if (phenotypeNameExists(name, existingNames)) {
    stop(sprintf("A phenotype named '%s' already exists.", name))
  }
  timeCreated <- Sys.time()
  attr(timeCreated, "tzone") <- "UTC"
  phenotypeId <- getNextPhenotypeId(connection, databaseSchema)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = databaseSchema,
    tableName = "phenotype",
    data = data.frame(
      phenotypeId = as.integer(phenotypeId),
      phenotypeName = name,
      clinicalDefinition = clinicalDefinition,
      creator = creator,
      timeCreated = timeCreated,
      draft = 1L,
      valid = 0L
    ),
    dropTableIfExists = FALSE,
    createTable = FALSE,
    tempTable = FALSE,
    camelCaseToSnakeCase = TRUE,
    progressBar = FALSE
  )
  if (!is.na(parentPhenotypeId)) {
    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = databaseSchema,
      tableName = "phenotype_lineage",
      data = data.frame(
        phenotypeId = as.integer(phenotypeId),
        parentPhenotypeId = as.integer(parentPhenotypeId)
      ),
      dropTableIfExists = FALSE,
      createTable = FALSE,
      tempTable = FALSE,
      camelCaseToSnakeCase = TRUE,
      progressBar = FALSE
    )
  }
  invisible(phenotypeId)
}
