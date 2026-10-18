# Fills cohort_definition_to_concept_set from the ConceptSets inside each cohort's Circe JSON, for data that was
# migrated without that link table filled in. The cohort page and the concept set page (which cohort definitions
# use this concept set) read this table. Concept sets are matched to stored ones by their items, or by name.
# Needs concept_set_item to be filled first (extras/PopulateConceptSetItems.R). Existing links are kept, so it is
# safe to re-run. Run it from the repository root.
#
# The matching runs inside the database in one query: the items of every cohort's concept sets are uploaded to a
# temporary table and compared with concept_set_item. A concept set matches when it has exactly the same items.
#
# Settings: matchByName = FALSE matches on content only (no name fallback); onDuplicates decides what happens when
# several stored concept sets have identical content; dryRun = TRUE only reports, without changing the database.
matchByName <- TRUE
onDuplicates <- "all" # or "lowest": what to do when several stored concept sets have identical content
dryRun <- FALSE
library(DatabaseConnector)

source("shiny/R/db.R")
source("shiny/R/modEvaluation.R")
source("shiny/R/cohortConceptSets.R")

connectionDetails <- createConnectionDetails(
  dbms = Sys.getenv("PEGASUS_DBMS"),
  server = Sys.getenv("PEGASUS_SERVER"),
  connectionString = Sys.getenv("PEGASUS_CONNECTION_STRING"),
  user = Sys.getenv("PEGASUS_DB_USER"),
  password = Sys.getenv("PEGASUS_DB_PASSWORD")
)
databaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")

connection <- connect(connectionDetails)
on.exit(disconnect(connection))

cohorts <- queryPegasus(
  connection, databaseSchema,
  "SELECT cohort_definition_id, circe_cohort_expression FROM @database_schema.cohort_definition;"
)
existing <- queryPegasus(
  connection, databaseSchema,
  "SELECT cohort_definition_id, concept_set_id FROM @database_schema.cohort_definition_to_concept_set;"
)

result <- matchCohortConceptSetsInDatabase(connection, databaseSchema, cohorts, matchByName, onDuplicates)
report <- result$report
message(
  "Concept sets inside cohort definitions: ", report$conceptSets, ". Matched by content: ", report$matchedByContent,
  ", by name: ", report$matchedByName, ", not matched: ", nrow(report$unmatched), "."
)
if (report$duplicateContent > 0) {
  message(
    report$duplicateContent, " concept set(s) in cohorts have identical content in several stored concept sets (",
    if (onDuplicates == "all") "all of them are linked" else "only the lowest ID is linked", ")."
  )
}
if (nrow(report$unmatched) > 0) {
  message("Concept sets without a match (first 20):")
  print(utils::head(report$unmatched, 20))
}

links <- result$links
if (nrow(links) == 0) {
  stop("No concept sets in any cohort definition could be matched to a stored concept set.")
}
links <- links[!paste(links$cohortDefinitionId, links$conceptSetId) %in% paste(existing$cohortDefinitionId, existing$conceptSetId), ]

if (dryRun) {
  message("Dry run: would add ", nrow(links), " links for ", length(unique(links$cohortDefinitionId)), " of ", nrow(cohorts), " cohort definitions.")
  print(utils::head(links, 20))
  quit(save = "no")
}

insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "cohort_definition_to_concept_set",
  data = links,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)
message(
  "Added ", nrow(links), " links for ", length(unique(links$cohortDefinitionId)), " of ", nrow(cohorts),
  " cohort definitions."
)
