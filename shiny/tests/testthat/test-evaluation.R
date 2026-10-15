validCohortJson <- function(limit = "First") {
  sprintf(
    '{"ConceptSets":[],"PrimaryCriteria":{"CriteriaList":[],"ObservationWindow":{"PriorDays":0,"PostDays":0},"PrimaryCriteriaLimit":{"Type":"%s"}},"QualifiedLimit":{"Type":"First"}}',
    limit
  )
}

test_that("validateCirceJson accepts Circe-shaped JSON and explains why other input is rejected", {
  expect_true(validateCirceJson(validCohortJson())$ok)
  expect_false(is.null(validateCirceJson(validCohortJson())$canonical))

  expect_false(validateCirceJson("")$ok)
  expect_false(validateCirceJson(NULL)$ok)
  expect_match(validateCirceJson("not json")$message, "not a valid JSON")
  expect_match(validateCirceJson("[1, 2]")$message, "not a valid JSON")
  expect_match(validateCirceJson('{"a": 1}')$message, "PrimaryCriteria")
})

test_that("canonical JSON ignores whitespace so a re-pasted definition is recognised", {
  compact <- validateCirceJson(validCohortJson())$canonical
  spaced <- validateCirceJson(paste0("\n  ", gsub(",", ",\n  ", validCohortJson(), fixed = TRUE), "\n"))$canonical
  expect_equal(compact, spaced)
  expect_false(identical(compact, validateCirceJson(validCohortJson("All"))$canonical))
})

submitTo <- function(connection, json = validCohortJson(), name = "Liver custom", phenotypeId = 5L,
                     databases = c(1, 2), types = "keeper + llm", notes = "") {
  submitEvaluationRequest(connection, "main", phenotypeId, name, json, NA, databases, types, "tester", notes)
}

test_that("submitEvaluationRequest saves a user cohort and queues one request per database and type", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  result <- withConnection(pool, function(connection) submitTo(connection, notes = "first try"))
  expect_false(result$reused)
  expect_equal(nrow(result$queued), 2)
  expect_equal(nrow(result$skipped), 0)

  cohort <- withConnection(pool, function(connection) {
    DatabaseConnector::querySql(
      connection,
      sprintf("SELECT * FROM main.cohort_definition WHERE cohort_definition_id = %d", result$cohortDefinitionId)
    )
  })
  expect_equal(cohort$COHORT_DEFINITION_NAME, "Liver custom")
  expect_equal(cohort$PHENOTYPE_ID, 5)
  expect_equal(cohort$PA_SOURCE_TYPE, "user")
  expect_equal(cohort$VALID, 0)

  requests <- withConnection(pool, function(connection) {
    DatabaseConnector::querySql(connection, "SELECT * FROM main.evaluation_request ORDER BY evaluation_request_id")
  })
  expect_equal(requests$EVALUATION_REQUEST_ID, c(1, 2))
  expect_equal(requests$DATABASE_ID, c(1, 2))
  expect_equal(unique(requests$REQUESTED_BY), "tester")
  expect_equal(unique(requests$NOTES), "first try")
})

test_that("pasting the same definition again reuses the cohort and skips existing requests", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  first <- withConnection(pool, function(connection) submitTo(connection))
  again <- withConnection(pool, function(connection) submitTo(connection, json = paste0("  ", validCohortJson(), "\n"), name = "Another name"))
  expect_true(again$reused)
  expect_equal(again$cohortDefinitionId, first$cohortDefinitionId)
  expect_equal(again$cohortDefinitionName, "Liver custom")
  expect_equal(nrow(again$queued), 0)
  expect_equal(nrow(again$skipped), 2)

  more <- withConnection(pool, function(connection) submitTo(connection, types = c("keeper + llm", "phevaluator")))
  expect_equal(nrow(more$queued), 2)
  expect_equal(nrow(more$skipped), 2)
  expect_equal(withConnection(pool, function(connection) {
    DatabaseConnector::querySql(connection, "SELECT COUNT(*) AS n FROM main.cohort_definition")$N
  }), 11)
  expect_equal(withConnection(pool, function(connection) {
    DatabaseConnector::querySql(connection, "SELECT MAX(evaluation_request_id) AS m FROM main.evaluation_request")$M
  }), 4)
})

test_that("submitEvaluationRequest rejects bad input", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  expect_error(withConnection(pool, function(connection) submitTo(connection, json = "nope")), "not a valid JSON")
  expect_error(withConnection(pool, function(connection) submitTo(connection, name = " ")), "cannot be empty")
  expect_error(withConnection(pool, function(connection) submitTo(connection, name = "asthma - TEMPLATE")), "already exists")
  expect_error(withConnection(pool, function(connection) submitTo(connection, databases = integer(0))), "at least one database")
  expect_error(withConnection(pool, function(connection) submitTo(connection, types = character(0))), "at least one database")
  expect_equal(withConnection(pool, function(connection) {
    DatabaseConnector::querySql(connection, "SELECT COUNT(*) AS n FROM main.cohort_definition")$N
  }), 10)
})

test_that("queueEvaluations skips combinations that already have results", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  # Seed cohort 1 already has keeper + llm evaluations for databases 1 and 2.
  result <- withConnection(pool, function(connection) {
    queueEvaluations(connection, "main", 1L, c(1, 2, 3), "keeper + llm", "tester")
  })
  expect_equal(result$queued$databaseId, 3)
  expect_equal(nrow(result$skipped), 2)
})

test_that("getEvaluationRequests derives the status from stored evaluations", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  result <- withConnection(pool, function(connection) submitTo(connection))
  requests <- withConnection(pool, function(connection) getEvaluationRequests(connection, "main"))
  expect_equal(nrow(requests), 2)
  expect_equal(requests$status, c("Queued", "Queued"))
  expect_equal(unique(requests$phenotypeName), "Acute liver injury")

  withConnection(pool, function(connection) {
    DatabaseConnector::executeSql(
      connection,
      sprintf(
        "INSERT INTO main.cohort_definition_evaluation VALUES (%d, 1, 100, 90, 5, 80, 10, 800, 20, 0.89, 0.8, 0.95, 0.8, 0.7, 0.9, 'keeper + llm')",
        result$cohortDefinitionId
      ),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  })
  requests <- withConnection(pool, function(connection) getEvaluationRequests(connection, "main"))
  expect_equal(requests$status[requests$cdmSourceAbbreviation == "DEVCLM"], "Complete")
  expect_equal(requests$status[requests$cdmSourceAbbreviation == "DEVEHR"], "Queued")
})

test_that("Saving a phenotype reports its ID to the onSaved callback", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  module <- createTestModule(pool)
  savedId <- NULL
  module$args$onSaved <- function(phenotypeId) savedId <<- phenotypeId

  testServer(createServer, args = module$args, {
    session$setInputs(saveName = "Asthma", definition = "Chronic airway disease.")
    session$setInputs(save = 1)
  })
  expect_equal(savedId, 1)
})
