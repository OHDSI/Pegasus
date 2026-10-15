# Fills the concept_set_item table from the Circe expressions in concept_set. Concept search in the Shiny app
# reads this table, so it is empty (and every concept search finds nothing) until this has been run.
# Safe to re-run: the table is emptied first.
library(DatabaseConnector)

connectionDetails <- createConnectionDetails(
  dbms = Sys.getenv("PEGASUS_DBMS"),
  server = Sys.getenv("PEGASUS_SERVER"),
  connectionString = Sys.getenv("PEGASUS_CONNECTION_STRING"),
  user = Sys.getenv("PEGASUS_DB_USER"),
  password = Sys.getenv("PEGASUS_DB_PASSWORD")
)
databaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")

# One row per item of a Circe concept set expression ({"items": [{"concept": {"CONCEPT_ID": ...}, ...}]}).
conceptSetItems <- function(conceptSetId, circeExpression) {
  items <- jsonlite::fromJSON(circeExpression, simplifyVector = FALSE)$items
  if (length(items) == 0) {
    return(NULL)
  }
  data.frame(
    conceptSetId = conceptSetId,
    conceptId = vapply(items, function(item) as.integer(item$concept$CONCEPT_ID), 1L),
    isExcluded = vapply(items, function(item) as.integer(isTRUE(item$isExcluded)), 1L),
    includeDescendants = vapply(items, function(item) as.integer(isTRUE(item$includeDescendants)), 1L),
    includeMapped = vapply(items, function(item) as.integer(isTRUE(item$includeMapped)), 1L)
  )
}

connection <- connect(connectionDetails)
on.exit(disconnect(connection))

conceptSets <- renderTranslateQuerySql(
  connection,
  "SELECT concept_set_id, circe_concept_set_expression FROM @database_schema.concept_set;",
  database_schema = databaseSchema,
  snakeCaseToCamelCase = TRUE
)
items <- do.call(rbind, Map(conceptSetItems, conceptSets$conceptSetId, conceptSets$circeConceptSetExpression))

renderTranslateExecuteSql(connection, "DELETE FROM @database_schema.concept_set_item;", database_schema = databaseSchema)
insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "concept_set_item",
  data = items,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)
message("Inserted ", nrow(items), " items for ", length(unique(items$conceptSetId)), " concept sets.")
