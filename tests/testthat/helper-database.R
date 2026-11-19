library(dplyr)

testSchema <- "main"

# Creates a temporary DuckDB file; the file is removed when `envir` exits.
createTestConnectionDetails <- function(envir = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".duckdb", .local_envir = envir)
  DatabaseConnector::createConnectionDetails(dbms = "duckdb", server = file)
}

# Returns an open connection to an empty Pegasus database; disconnects when `envir` exits.
createTestPegasusConnection <- function(envir = parent.frame()) {
  connection <- DatabaseConnector::connect(createTestConnectionDetails(envir))
  withr::defer(DatabaseConnector::disconnect(connection), envir = envir)
  Pegasus::createDataModel(pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  connection
}

# Tiny CDM:
# - Concept 1 is the ancestor of concepts 2 and 3. Concept 4 is unrelated.
# - Person 1: concepts 1 and 2. Person 2: concept 2. Person 3: concept 3. Person 4: concept 4.
createTestCdmConnectionDetails <- function(envir = parent.frame()) {
  connectionDetails <- createTestConnectionDetails(envir)
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  concept <- data.frame(
    concept_id = 1:4,
    concept_name = paste("Concept", 1:4),
    domain_id = "Condition",
    vocabulary_id = "TEST",
    concept_class_id = "Clinical Finding",
    standard_concept = "S",
    concept_code = paste0("C", 1:4),
    valid_start_date = as.Date("1970-01-01"),
    valid_end_date = as.Date("2099-12-31"),
    invalid_reason = NA_character_
  )
  conceptAncestor <- data.frame(
    ancestor_concept_id = c(1:4, 1, 1),
    descendant_concept_id = c(1:4, 2, 3),
    min_levels_of_separation = c(0, 0, 0, 0, 1, 1),
    max_levels_of_separation = c(0, 0, 0, 0, 1, 1)
  )
  conceptRelationship <- data.frame(
    concept_id_1 = integer(),
    concept_id_2 = integer(),
    relationship_id = character(),
    invalid_reason = character()
  )
  conditionOccurrence <- data.frame(
    person_id = c(1, 1, 2, 3, 4),
    condition_concept_id = c(1, 2, 2, 3, 4)
  )
  tables <- list(concept = concept,
                 concept_ancestor = conceptAncestor,
                 concept_relationship = conceptRelationship,
                 condition_occurrence = conditionOccurrence)
  for (tableName in names(tables)) {
    DatabaseConnector::insertTable(connection = connection,
                                   databaseSchema = testSchema,
                                   tableName = tableName,
                                   data = tables[[tableName]],
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = FALSE)
  }
  connectionDetails
}

# Builds a Circe concept set expression. Each item is c(id, excluded, descendants).
createCirceConceptSet <- function(...) {
  items <- lapply(list(...), function(x) {
    list(concept = list(CONCEPT_ID = x[[1]],
                        CONCEPT_NAME = paste("Concept", x[[1]]),
                        STANDARD_CONCEPT = "S",
                        STANDARD_CONCEPT_CAPTION = "Standard",
                        INVALID_REASON = "V",
                        INVALID_REASON_CAPTION = "Valid",
                        CONCEPT_CODE = paste0("C", x[[1]]),
                        DOMAIN_ID = "Condition",
                        VOCABULARY_ID = "TEST",
                        CONCEPT_CLASS_ID = "Clinical Finding"),
         isExcluded = x[[2]],
         includeDescendants = x[[3]],
         includeMapped = FALSE)
  })
  as.character(jsonlite::toJSON(list(items = items), auto_unbox = TRUE, null = "null"))
}
