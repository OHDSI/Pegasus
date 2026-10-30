library(testthat)

createTestDatabases <- function() {
  tibble(
    databaseName = c("Database A", "Database B"),
    databaseDescription = c("First test database.", "Second test database."),
    cdmDatabaseSchema = "main",
    referenceCohortDatabaseSchema = "main",
    referenceCohortTable = "reference_cohort",
    referenceCohortProfilesTable = "reference_cohort_profiles",
    cohortDatabaseSchema = "main",
    cohortTable = c("cohort_a", "cohort_b"),
    conceptSetTable = "concept_set",
    conceptSetDefinitionTable = "concept_set_definition"
  )
}

createTestContext <- function(env = parent.frame()) {
  databaseFile <- tempfile(fileext = ".duckdb")
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "duckdb", server = databaseFile)

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, "
    CREATE TABLE concept_set_expression_plus (
      concept_set_name VARCHAR, hypernym INT, overall_persons INT
    );
    INSERT INTO concept_set_expression_plus VALUES ('Asthma', 0, 100), ('Wheeze', 1, 0);
    CREATE TABLE phenotype_to_concept_set (
      phenotype VARCHAR, concept_set_name VARCHAR, hypernym INT
    );
    INSERT INTO phenotype_to_concept_set VALUES ('Asthma', 'Asthma', 0), ('Asthma', 'Wheeze', 1);
    CREATE TABLE cohort_a (
      cohort_definition_id INT, subject_id INT, cohort_start_date DATE, cohort_end_date DATE
    );
    INSERT INTO cohort_a VALUES (1, 1, DATE '2020-01-01', DATE '2020-02-01'),
      (1, 1, DATE '2020-06-01', DATE '2020-07-01'),
      (1, 2, DATE '2020-03-01', DATE '2020-04-01');
    CREATE TABLE cohort_a_inclusion (
      cohort_definition_id INT, rule_sequence INT, name VARCHAR, description VARCHAR
    );
  ", progressBar = FALSE, reportOverallTime = FALSE)
  DatabaseConnector::disconnect(connection)

  context <- PegasusAgentContext$new(
    connectionDetails = connectionDetails,
    databases = createTestDatabases(),
    conceptSetDatabaseSchema = "main"
  )
  withr::defer({
    context$close()
    unlink(databaseFile)
  }, envir = env)
  return(context)
}

test_that("constructor validates its arguments", {
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "duckdb", server = tempfile())
  databases <- createTestDatabases()

  expect_error(
    PegasusAgentContext$new(
      connectionDetails = connectionDetails,
      databases = databases[, -1],
      conceptSetDatabaseSchema = "main"
    ),
    "databaseName"
  )
  expect_error(
    PegasusAgentContext$new(
      connectionDetails = "not connection details",
      databases = databases,
      conceptSetDatabaseSchema = "main"
    )
  )
  expect_error(
    PegasusAgentContext$new(
      connectionDetails = connectionDetails,
      databases = rbind(databases, databases),
      conceptSetDatabaseSchema = "main"
    )
  )
})

test_that("selectDatabases returns the requested databases", {
  context <- createTestContext()

  expect_equal(selectDatabases(context)$databaseName, c("Database A", "Database B"))
  expect_equal(selectDatabases(context, "Database B")$databaseName, "Database B")
  expect_equal(selectDatabases(context, list("Database B", "Database A"))$databaseName, c("Database B", "Database A"))
  expect_error(selectDatabases(context, "Unknown"), "Unknown database")
})

test_that("getDatabaseDescription describes the selected databases", {
  context <- createTestContext()

  expect_equal(getDatabaseDescription(context, "Database A"), "First test database.")
  expect_equal(
    getDatabaseDescription(context),
    "Database A: First test database.\n\nDatabase B: Second test database."
  )
})

test_that("listConceptSets returns a markdown table", {
  context <- createTestContext()

  table <- listConceptSets(context, "asthma")
  expect_equal(
    strsplit(table, "\n")[[1]][3:4],
    c("| Asthma | Y | 100 |", "| Wheeze | N | 0 |")
  )
})

test_that("getCohortCount counts persons and entries", {
  context <- createTestContext()

  counts <- jsonlite::fromJSON(getCohortCount(context, 1, database = "Database A"))
  expect_equal(counts$personCount, 2)
  expect_equal(counts$entryCount, 3)
  expect_equal(counts$database, "Database A")
})

test_that("withConnection returns the connection to the pool", {
  context <- createTestContext()

  result <- context$withConnection(function(connection) {
    DatabaseConnector::querySql(connection, "SELECT 1 AS value;")
  })
  expect_equal(unname(unlist(result)), 1)
})

test_that("tools are valid ellmer tools without a context argument", {
  context <- createTestContext()

  tools <- context$tools()
  expect_length(tools, 12)
  for (tool in tools) {
    expect_s3_class(tool, "ellmer::ToolDef")
  }
  toolNames <- vapply(tools, function(tool) tool@name, character(1))
  expect_true(all(c("list_concept_sets", "generate_cohort", "get_cohort_count") %in% toolNames))

  getCohortCountTool <- tools[[which(toolNames == "get_cohort_count")]]
  expect_false("context" %in% names(formals(getCohortCountTool)))
  counts <- jsonlite::fromJSON(getCohortCountTool(cohortId = 1, database = list("Database A")))
  expect_equal(counts$personCount, 2)
})

test_that("close shuts down the pool", {
  context <- createTestContext()

  context$close()
  expect_false(pool::dbIsValid(context$connectionPool))
  expect_silent(context$close())
})
