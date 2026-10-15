# Keeps the app's activity logging out of the test output.
options(pegasus.quiet = TRUE)

createTestPool <- function() {
  pool <- pool::poolCreate(
    factory = function() {
      DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
    },
    minSize = 1,
    maxSize = 1
  )
  withConnection(pool, function(connection) {
    # Uses the working-tree copy of the data model so tests track the real schema.
    sql <- SqlRender::render(
      SqlRender::readSql(file.path("..", "..", "..", "inst", "sql", "CreateDataModel.sql")),
      database_schema = "main"
    )
    sql <- SqlRender::translate(sql, targetDialect = "duckdb")
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  })
  pool
}

# Loads the development seed data (see extras/DevSeedData.sql) into the test database.
loadSeedData <- function(pool) {
  sql <- paste(readLines(file.path("..", "..", "..", "extras", "DevSeedData.sql"), encoding = "UTF-8"), collapse = "\n")
  sql <- gsub("@database_schema", "main", sql, fixed = TRUE)
  withConnection(pool, function(connection) {
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  })
  invisible(pool)
}

# Creates the tables of a minimal OMOP vocabulary on a connection.
populateVocabulary <- function(connection) {
  DatabaseConnector::executeSql(
    connection,
    "CREATE TABLE main.concept (
       concept_id INT, concept_name VARCHAR, concept_code VARCHAR, domain_id VARCHAR, vocabulary_id VARCHAR,
       concept_class_id VARCHAR, standard_concept VARCHAR, invalid_reason VARCHAR
     );
     INSERT INTO main.concept VALUES
       (317009, 'Asthma', '195967001', 'Condition', 'SNOMED', 'Clinical Finding', 'S', NULL),
       (4051466, 'Childhood asthma', '233678006', 'Condition', 'SNOMED', 'Clinical Finding', 'S', NULL),
       (255573, 'Chronic obstructive lung disease', '13645005', 'Condition', 'SNOMED', 'Clinical Finding', 'S', NULL),
       (9001, 'Asthma (source code)', 'J45.9', 'Condition', 'ICD10CM', 'ICD10 Code', NULL, NULL),
       (9002, 'Old asthma code', 'J45.909', 'Condition', 'ICD10CM', 'ICD10 Code', NULL, 'D'),
       (9003, 'Predominantly allergic asthma', 'J45.0', 'Condition', 'ICD10CM', 'ICD10 Code', NULL, NULL),
       (9004, 'Asthma, unspecified', '493.90', 'Condition', 'ICD9CM', 'ICD9 Code', NULL, NULL);
     CREATE TABLE main.vocabulary (
       vocabulary_id VARCHAR, vocabulary_name VARCHAR, vocabulary_version VARCHAR
     );
     INSERT INTO main.vocabulary VALUES
       ('None', 'OMOP Standardized Vocabularies', 'v5.0 TEST-RELEASE'), ('SNOMED', 'SNOMED', 'SNOMED 2025');
     CREATE TABLE main.concept_ancestor (
       ancestor_concept_id INT, descendant_concept_id INT, min_levels_of_separation INT, max_levels_of_separation INT
     );
     INSERT INTO main.concept_ancestor VALUES
       (317009, 317009, 0, 0), (4051466, 4051466, 0, 0), (255573, 255573, 0, 0), (317009, 4051466, 1, 1);
     CREATE TABLE main.concept_relationship (
       concept_id_1 INT, concept_id_2 INT, relationship_id VARCHAR, invalid_reason VARCHAR
     );
     INSERT INTO main.concept_relationship VALUES
       (9001, 317009, 'Maps to', NULL), (9001, 9001, 'Maps to', NULL), (9002, 317009, 'Maps to', 'D'),
       (9003, 4051466, 'Maps to', NULL), (9004, 317009, 'Maps to', NULL);",
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}

# A minimal OMOP vocabulary in its own in-memory database, standing in for the separate vocabulary platform.
createVocabularyPool <- function() {
  pool <- pool::poolCreate(
    factory = function() {
      DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
    },
    minSize = 1,
    maxSize = 1
  )
  withConnection(pool, populateVocabulary)
  pool
}

# The same vocabulary in a DuckDB file, which a separate R process can open. Returns the file path.
createVocabularyFile <- function() {
  file <- tempfile(fileext = ".duckdb")
  connection <- DatabaseConnector::connect(dbms = "duckdb", server = file)
  populateVocabulary(connection)
  DatabaseConnector::disconnect(connection)
  file
}

# Runs vocabulary tasks in this process against a pool, with the same interface as the background runner.
inlineVocabularyRunner <- function(vocabularyPool) {
  function(task, args) {
    outcome <- tryCatch(
      list(ok = TRUE, value = withConnection(vocabularyPool, function(connection) {
        runVocabularyTask(task, args, connection, "main")
      })),
      error = function(e) list(ok = FALSE, error = conditionMessage(e))
    )
    list(
      poll = function() c(list(done = TRUE, lines = c("12:00:00  (inline) task ran")), outcome),
      kill = function() NULL
    )
  }
}
