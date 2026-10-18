test_that("getEvaluations can be limited to one cohort definition", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  all <- withConnection(pool, function(connection) getEvaluations(connection, "main"))
  one <- withConnection(pool, function(connection) getEvaluations(connection, "main", cohortDefinitionId = 1))
  expect_true(nrow(one) > 0 && nrow(one) < nrow(all))
  expect_true(all(one$cohortDefinitionId == 1))
  both <- withConnection(pool, function(connection) getEvaluations(connection, "main", phenotypeId = 2, cohortDefinitionId = 1))
  expect_equal(nrow(both), 0)
})

test_that("cohort definition data includes the phenotype and its concept sets", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  cohort <- withConnection(pool, function(connection) getCohortDefinition(connection, "main", 3))
  expect_equal(nrow(cohort), 1)
  expect_equal(cohort$phenotypeName, "Asthma")
  expect_match(cohort$circeCohortExpression, "ConceptSets", fixed = TRUE)
  sets <- withConnection(pool, function(connection) getCohortConceptSets(connection, "main", 3))
  expect_equal(sets$conceptSetName, "Asthma (exact)")
  expect_equal(nrow(withConnection(pool, function(connection) getCohortDefinition(connection, "main", 999))), 0)
})

test_that("concept set data includes reverse lookups to cohort definitions and phenotypes", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  conceptSet <- withConnection(pool, function(connection) getConceptSet(connection, "main", 1))
  expect_equal(conceptSet$conceptSetName, "Asthma")
  expect_match(conceptSet$circeConceptSetExpression, "includeDescendants", fixed = TRUE)

  cohorts <- withConnection(pool, function(connection) getConceptSetCohorts(connection, "main", 1))
  expect_setequal(cohorts$cohortDefinitionId, c(1, 2))
  expect_true(all(cohorts$phenotypeName == "Asthma"))

  phenotypes <- withConnection(pool, function(connection) getConceptSetPhenotypes(connection, "main", 1))
  expect_equal(phenotypes$phenotypeId, 1)
  expect_equal(phenotypes$via, "Phenotype concept set")

  counts <- withConnection(pool, function(connection) getConceptSetCounts(connection, "main", 1))
  expect_setequal(counts$cdmSourceAbbreviation, c("DEVCLM", "DEVEHR", "DEVREG"))
  expect_equal(nrow(withConnection(pool, function(connection) getConceptSet(connection, "main", 999))), 0)
})

test_that("a phenotype that only reaches a concept set through a cohort is reported as such", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  withConnection(pool, function(connection) {
    DatabaseConnector::executeSql(
      connection,
      "INSERT INTO main.cohort_definition_to_concept_set (cohort_definition_id, concept_set_id) VALUES (8, 1);",
      progressBar = FALSE, reportOverallTime = FALSE
    )
  })
  phenotypes <- withConnection(pool, function(connection) getConceptSetPhenotypes(connection, "main", 1))
  expect_equal(nrow(phenotypes), 2)
  expect_equal(phenotypes$via[phenotypes$phenotypeId == 4], "Cohort definition")
})

test_that("parseConceptSetItems reads Circe expressions and tolerates bad input", {
  expression <- paste0(
    '{"items":[{"concept":{"CONCEPT_ID":317009,"CONCEPT_NAME":"Asthma","DOMAIN_ID":"Condition","VOCABULARY_ID":"SNOMED",',
    '"CONCEPT_CODE":"195967001","STANDARD_CONCEPT":"S"},"isExcluded":false,"includeDescendants":true,"includeMapped":false},',
    '{"concept":{"CONCEPT_ID":255573},"isExcluded":true}]}'
  )
  items <- parseConceptSetItems(expression)
  expect_equal(items$conceptId, c(317009, 255573))
  expect_equal(items$conceptName[1], "Asthma")
  expect_true(is.na(items$conceptName[2]))
  expect_equal(items$includeDescendants, c(1, 0))
  expect_equal(items$isExcluded, c(0, 1))
  expect_equal(nrow(parseConceptSetItems("not json")), 0)
  expect_equal(nrow(parseConceptSetItems('{"items":[]}')), 0)
})

test_that("specificity, cohort summaries and the evaluation table", {
  expect_equal(specificity(90, 10), 0.9)
  expect_true(is.na(specificity(0, 0)))
  expect_true(is.na(specificity(NA, 1)))

  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  evaluations <- withConnection(pool, function(connection) getEvaluations(connection, "main", phenotypeId = 1))
  cohorts <- withConnection(pool, function(connection) getCohortDefinitions(connection, "main", 1))
  summary <- summarizeCohorts(cohorts, evaluations)
  expect_equal(summary$evaluationCount[summary$cohortDefinitionId == 1], sum(evaluations$cohortDefinitionId == 1))
  expect_equal(summary$bestPpv[summary$cohortDefinitionId == 1], max(evaluations$ppv[evaluations$cohortDefinitionId == 1]))

  linked <- evaluationTable(evaluations)
  expect_match(linked$Cohort[1], "entity-link", fixed = TRUE)
  expect_true("Specificity %" %in% names(linked))
  expect_false("Cohort" %in% names(evaluationTable(evaluations, linkCohorts = FALSE)))
})

test_that("buildScatter can color the points by cohort definition", {
  skip_if_not_installed("plotly")
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  evaluations <- withConnection(pool, function(connection) getEvaluations(connection, "main", phenotypeId = 1))
  traces <- plotly::plotly_build(buildScatter(evaluations, colorBy = "cohort"))$x$data
  expect_setequal(vapply(traces, function(trace) trace$name, ""), unique(evaluations$cohortDefinitionName))
})

test_that("markdownToHtml renders tables and drops raw HTML", {
  skip_if_not_installed("commonmark")
  html <- as.character(markdownToHtml("| a | b |\n|---|---|\n| 1 | 2 |\n\n<script>alert(1)</script>\n\n**bold**"))
  expect_match(html, "<table>", fixed = TRUE)
  expect_match(html, "<strong>bold</strong>", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
})

test_that("cohort page loads its data, shows the phenotype link and falls back to JSON without a readable description", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  id <- reactiveVal(3L)
  goneBack <- FALSE
  testServer(
    cohortServer,
    args = list(
      connectionPool = pool, databaseSchema = "main", cohortDefinitionId = id,
      navigate = function(page = NULL, id = NULL) goneBack <<- is.null(page)
    ),
    {
      expect_equal(cohort()$cohortDefinitionName, "Asthma - user")
      header <- as.character(output$header$html)
      expect_match(header, "Asthma - user", fixed = TRUE)
      expect_match(header, 'data-page="phenotype"', fixed = TRUE)
      expect_match(header, "pegasusCopyLink('cohort_definition', 3", fixed = TRUE)
      expect_match(as.character(output$json$html), "cohortName", fixed = TRUE)
      expect_true(nzchar(as.character(output$logic$html)))
      session$setInputs(back = 1)
      expect_true(goneBack)

      id(999L)
      expect_match(as.character(output$header$html), "was not found", fixed = TRUE)
    }
  )
})

test_that("concept set page shows the expression, reverse lookups and a not-found message", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  id <- reactiveVal(1L)
  testServer(
    conceptSetServer,
    args = list(connectionPool = pool, databaseSchema = "main", conceptSetId = id, navigate = function(page = NULL, id = NULL) NULL),
    {
      expect_equal(conceptSet()$conceptSetName, "Asthma")
      header <- as.character(output$header$html)
      expect_match(header, "Asthma", fixed = TRUE)
      expect_match(header, "pegasusCopyLink('concept_set', 1", fixed = TRUE)
      raw <- as.character(output$raw$html)
      expect_match(raw, "Circe expression", fixed = TRUE)
      expect_match(raw, "Capr expression", fixed = TRUE)
      expect_match(raw, "includeDescendants", fixed = TRUE)

      id(999L)
      expect_match(as.character(output$header$html), "was not found", fixed = TRUE)
    }
  )
})

test_that("phenotype page lists cohort definitions as links and reports an unknown phenotype", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main")))
  id <- reactiveVal(1L)
  testServer(
    phenotypeDetailServer,
    args = list(
      connectionPool = pool, databaseSchema = "main", phenotypes = phenotypes, phenotypeId = id,
      shortlist = reactiveVal(integer(0)), navigate = function(page = NULL, id = NULL) NULL,
      onRefine = function(id) NULL
    ),
    {
      expect_equal(nrow(details()$cohorts), 3)
      header <- as.character(output$header$html)
      expect_match(header, "Clinical definition", fixed = TRUE)
      expect_match(header, 'data-page="phenotype"', fixed = TRUE) # refined into the clone

      id(999L)
      expect_match(as.character(output$header$html), "was not found", fixed = TRUE)
    }
  )
})
