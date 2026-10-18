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
  expect_true("DEVEHR" %in% asthma$databaseNames)
  expect_true(all(c("overallPersons", "conditionPersons", "visitPersons") %in% names(asthma$personCounts)))
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

  withDatabases <- addPersonCountColumns(conceptSets, counts[0, ], c("A", "C"))
  expect_equal(withDatabases$A, c(NA_real_, NA_real_))
  expect_equal(withDatabases$C, c(NA_real_, NA_real_))
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

test_that("browse server filters by status and creator and navigates to a phenotype", {
  navigated <- NULL
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
      route = reactiveVal(NULL),
      navigate = function(page = NULL, id = NULL) navigated <<- list(page = page, id = id),
      shortlist = reactiveVal(integer(0)),
      onRefine = function(id) NULL
    ),
    {
      session$setInputs(status = statusLevels, creator = NULL)
      expect_equal(nrow(filtered()), 9)

      session$setInputs(status = c("Evaluated", "Invalid"))
      expect_equal(nrow(filtered()), 5)

      session$setInputs(creator = "nobody")
      expect_equal(nrow(filtered()), 0)

      session$setInputs(status = statusLevels, creator = NULL)
      session$setInputs(openPhenotype = 5)
      expect_equal(navigated, list(page = "phenotype", id = 5))

      session$setInputs(`phenotype-back` = 1)
      expect_null(navigated$page)
    }
  )
})

test_that("prettyJson indents JSON and falls back to the original text", {
  expect_match(prettyJson('{"a":1}'), "\n", fixed = TRUE)
  expect_equal(prettyJson("not json"), "not json")
  expect_equal(prettyJson(NA_character_), "Not available")
})

test_that("useDropdownFilters turns low-cardinality plain text columns into factors", {
  data <- data.frame(
    status = c("Valid", "Draft", "Valid", NA),
    name = paste("Name", 1:4),
    html = c("<b>a</b>", "<b>a</b>", "<b>b</b>", "<b>b</b>"),
    amp = c("A &amp; B", "A &amp; B", "C", "C"),
    count = 1:4,
    stringsAsFactors = FALSE
  )
  result <- useDropdownFilters(data, maxValues = 3)
  expect_s3_class(result$status, "factor")
  expect_equal(levels(result$status), c("Draft", "Valid"))
  expect_true(is.na(result$status[4]))
  expect_type(result$name, "character")
  expect_type(result$html, "character")
  expect_type(result$amp, "character")
  expect_type(result$count, "integer")
  expect_equal(nrow(useDropdownFilters(data.frame(x = character(0)))), 0)
})

test_that("statusBadgeRender knows a badge class for every status", {
  js <- as.character(statusBadgeRender())
  for (status in statusLevels) {
    expect_match(js, status, fixed = TRUE)
  }
  expect_match(js, "text-bg-success", fixed = TRUE)
})
