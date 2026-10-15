# Times each step of using the vocabulary database, outside the Shiny app, to see where a slow or hanging
# concept search spends its time. Run from the repo root with the PEGASUS_* variables loaded:
#   Rscript extras/TestVocabularyConnection.R [search term]
source("shiny/R/config.R")
source("shiny/R/vocabulary.R")

term <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(term)) {
  term <- "asthma"
}

timed <- function(description, expression) {
  started <- Sys.time()
  cat(sprintf("%s ...\n", description))
  result <- tryCatch(expression, error = function(e) {
    cat(sprintf("  FAILED after %.1f s: %s\n", as.numeric(Sys.time() - started, units = "secs"), conditionMessage(e)))
    NULL
  })
  if (!is.null(result)) {
    cat(sprintf("  ok in %.1f s\n", as.numeric(Sys.time() - started, units = "secs")))
  }
  invisible(result)
}

vocab <- loadVocabularyConfig()
if (is.null(vocab)) {
  stop("PEGASUS_VOCAB_DBMS is not set, so no vocabulary database is configured.")
}
cat(sprintf("dbms: %s, schema: %s\n", vocab$dbms, vocab$schema))
if (!vocab$dbms %in% c("duckdb", "sqlite")) {
  cat(sprintf("JDBC folder: %s\n", Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")))
}

connection <- timed("Connecting", DatabaseConnector::connect(vocab$connectionDetails))
if (is.null(connection)) {
  quit(status = 1)
}
on.exit(DatabaseConnector::disconnect(connection))

version <- timed("Reading the vocabulary version", getVocabularyVersion(connection, vocab$schema))
if (!is.null(version)) cat(sprintf("  version: %s\n", version))

concept <- timed("Looking up concept 317009 by ID", getConcepts(connection, vocab$schema, 317009L))
if (!is.null(concept)) print(concept)

found <- timed(sprintf("Searching concept names for '%s'", term), searchConcepts(connection, vocab$schema, term))
if (!is.null(found)) {
  cat(sprintf("  %d concepts\n", nrow(found)))
  print(utils::head(found, 5))
}

timed("Reading ancestors of concept 317009", getConceptAncestors(connection, vocab$schema, 317009L))
