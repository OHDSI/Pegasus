test_that("deriveStatus follows the documented precedence", {
  status <- deriveStatus(
    draft = c(0, 0, 0, 0, 0, 1, 1, 0),
    valid = c(1, 1, 1, 1, 1, 0, 1, 0),
    conceptSetCount = c(2, 2, 2, 1, 0, 0, 3, 3),
    cohortCount = c(2, 2, 1, 0, 0, 0, 1, 1),
    evaluationCount = c(5, 0, 0, 0, 0, 0, 2, 4)
  )
  expect_equal(
    status,
    c("Evaluated", "Cohort defined", "Cohort defined", "Concept sets only", "Definition only",
      "Draft (queued)", "Draft (queued)", "Invalid")
  )
  expect_length(deriveStatus(integer(0), integer(0), integer(0), integer(0), integer(0)), 0)
})

test_that("getPhenotypeSummary derives status and metrics from the seed data", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  summary <- withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))
  expect_equal(nrow(summary), 9)
  expect_named(summary, names(emptyPhenotypeSummary()), ignore.order = TRUE)
  statusOf <- function(name) summary$status[summary$phenotypeName == name]
  expect_equal(statusOf("Asthma"), "Evaluated")
  expect_equal(statusOf("Acute liver injury"), "Cohort defined")
  expect_equal(statusOf("Psoriasis"), "Concept sets only")
  expect_equal(statusOf("Chronic kidney disease"), "Draft (queued)")
  expect_equal(statusOf("Asthma with eosinophilic phenotype"), "Draft (queued)")
  expect_equal(statusOf("Gout (retired)"), "Invalid")

  asthma <- summary[summary$phenotypeName == "Asthma", ]
  expect_equal(asthma$conceptSetCount, 2)
  expect_equal(asthma$cohortCount, 3)
  expect_equal(asthma$evaluationCount, 7)
  expect_equal(asthma$databaseCount, 3)
  expect_equal(asthma$bestPpv, 120 / 132, tolerance = 1e-3)
  expect_equal(asthma$bestSensitivity, 150 / 165, tolerance = 1e-3)

  psoriasis <- summary[summary$phenotypeName == "Psoriasis", ]
  expect_true(is.na(psoriasis$bestPpv))
  expect_equal(psoriasis$evaluationCount, 0)
})

test_that("getPhenotypeSummary works on an empty database", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))

  summary <- withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))
  expect_equal(nrow(summary), 0)
  expect_true("status" %in% names(summary))
})

test_that("getPhenotypeDetails returns every component for a phenotype", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  asthma <- withConnection(pool, function(connection) getPhenotypeDetails(connection, "main", 1))
  expect_equal(nrow(asthma$conceptSets), 2)
  expect_equal(nrow(asthma$cohorts), 3)
  expect_equal(nrow(asthma$evaluations), 7)
  expect_setequal(asthma$personCounts$cdmSourceAbbreviation, c("DEVCLM", "DEVEHR", "DEVREG"))
  expect_equal(nrow(asthma$parents), 0)
  expect_equal(asthma$children$phenotypeName, "Asthma with eosinophilic phenotype")
  expect_match(asthma$atlasBaseUrl, "^https://")

  clone <- withConnection(pool, function(connection) getPhenotypeDetails(connection, "main", 8))
  expect_equal(clone$parents$phenotypeName, "Asthma")
  expect_equal(nrow(clone$cohorts), 0)

  draft <- withConnection(pool, function(connection) getPhenotypeDetails(connection, "main", 7))
  expect_equal(nrow(draft$draftTargets), 2)
})

test_that("addPersonCountColumns adds one column per database", {
  conceptSets <- data.frame(conceptSetId = c(1, 2))
  counts <- data.frame(
    conceptSetId = c(1, 2, 1),
    cdmSourceAbbreviation = c("A", "A", "B"),
    overallPersons = c(10, 20, 30)
  )
  result <- addPersonCountColumns(conceptSets, counts)
  expect_equal(result$A, c(10, 20))
  expect_equal(result$B, c(30, NA))
})

test_that("atlasLink builds links only when possible", {
  expect_equal(atlasLink(NULL, "cohort", c(1, 2)), c("", ""))
  links <- atlasLink("https://host/atlas/", "cohort", c(5, NA))
  expect_match(links[1], 'href="https://host/atlas/#/cohortdefinition/5"', fixed = TRUE)
  expect_equal(links[2], "")
  expect_match(atlasLink("https://host/atlas", "conceptSet", 7), "#/conceptset/7/details", fixed = TRUE)
})

test_that("statusBadge covers every status level", {
  expect_true(all(statusLevels %in% names(statusBadgeClasses)))
  expect_length(statusBadge(character(0)), 0)
  expect_match(statusBadge("Evaluated"), "badge text-bg-success")
})

test_that("browse server filters by status and creator and selects a phenotype", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main")))

  testServer(
    browseServer,
    args = list(
      connectionPool = pool,
      databaseSchema = "main",
      phenotypes = phenotypes,
      refreshPhenotypes = function() NULL,
      selectedId = reactiveVal(NULL),
      shortlist = reactiveVal(integer(0))
    ),
    {
      session$setInputs(status = statusLevels, creator = NULL)
      expect_equal(nrow(filtered()), 9)

      session$setInputs(status = c("Evaluated", "Invalid"))
      expect_equal(nrow(filtered()), 5)

      session$setInputs(creator = "nobody")
      expect_equal(nrow(filtered()), 0)

      session$setInputs(status = statusLevels, creator = NULL)
      session$setInputs(table_rows_selected = 1)
      expect_equal(selectedId(), filtered()$phenotypeId[1])

      session$setInputs(`detail-back` = 1)
      expect_null(selectedId())
    }
  )
})
