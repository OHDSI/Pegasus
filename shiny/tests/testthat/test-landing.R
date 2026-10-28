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
      json <- as.character(output$json$html)
      expect_match(json, "cohortName", fixed = TRUE)
      expect_match(json, "pegasusCopyText", fixed = TRUE)
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

test_that("getConceptSetSummaries returns the listed concept sets and handles an empty list", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  summary <- withConnection(pool, function(connection) getConceptSetSummaries(connection, "main", c(1L, 4L)))
  expect_setequal(summary$conceptSetName, c("Asthma", "COPD"))
  expect_equal(nrow(withConnection(pool, function(connection) getConceptSetSummaries(connection, "main", integer(0)))), 0)
})

addIdenticalConceptSet <- function(pool) {
  withConnection(pool, function(connection) {
    # A second stored concept set with the same content as concept set 1.
    DatabaseConnector::executeSql(
      connection,
      "INSERT INTO main.concept_set (concept_set_id, concept_set_name, circe_concept_set_expression, has_descendants, valid)
         VALUES (20, 'Asthma copy', '{\"items\": []}', 1, 1);
       INSERT INTO main.concept_set_item VALUES (20, 317009, 0, 1, 0);",
      progressBar = FALSE, reportOverallTime = FALSE
    )
  })
}

test_that("getIdenticalConceptSets finds other stored sets with exactly the same content", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  expect_equal(nrow(withConnection(pool, function(connection) getIdenticalConceptSets(connection, "main", 1))), 0)
  addIdenticalConceptSet(pool)
  expect_equal(withConnection(pool, function(connection) getIdenticalConceptSets(connection, "main", 1))$conceptSetId, 20)
  expect_equal(withConnection(pool, function(connection) getIdenticalConceptSets(connection, "main", 20))$conceptSetId, 1)
  # Concept set 8 has an extra, excluded item, so it is not the same as 1 or 20.
  expect_equal(nrow(withConnection(pool, function(connection) getIdenticalConceptSets(connection, "main", 8))), 0)
})

test_that("the concept set page points to identical concept sets", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  addIdenticalConceptSet(pool)
  testServer(
    conceptSetServer,
    args = list(connectionPool = pool, databaseSchema = "main", conceptSetId = reactiveVal(1L), navigate = function(page = NULL, id = NULL) NULL),
    {
      header <- as.character(output$header$html)
      expect_match(header, "same content", fixed = TRUE)
      expect_match(header, "Asthma copy", fixed = TRUE)
      expect_match(header, 'data-id="20"', fixed = TRUE)
    }
  )
  testServer(
    conceptSetServer,
    args = list(connectionPool = pool, databaseSchema = "main", conceptSetId = reactiveVal(8L), navigate = function(page = NULL, id = NULL) NULL),
    {
      expect_false(grepl("same content", as.character(output$header$html), fixed = TRUE))
    }
  )
})

test_that("cohortConceptSetTable names the matching JSON concept set and marks identical stored sets", {
  conceptSets <- data.frame(
    conceptSetId = c(1L, 20L, 7L), conceptSetName = c("Asthma", "Asthma copy", "Psoriasis"), csSourceType = "User",
    valid = 1L, hasDescendants = 1L, atlasConceptSetId = NA_integer_
  )
  matches <- data.frame(
    cohortDefinitionId = 5L, setIndex = 1L, conceptSetId = c(1L, 20L), how = "content", name = "Atlas asthma",
    conceptSetName = c("Asthma", "Asthma copy")
  )
  table <- cohortConceptSetTable(conceptSets, matches, NULL)
  expect_equal(table[["In the cohort JSON as"]], c("Atlas asthma", "Atlas asthma", ""))
  expect_match(table$Note[1], "Same content as", fixed = TRUE)
  expect_match(table$Note[1], "Asthma copy", fixed = TRUE)
  expect_match(table$Note[2], "Asthma</a>", fixed = TRUE)
  expect_equal(table$Note[3], "Not found in the cohort's JSON")

  withoutMatches <- cohortConceptSetTable(conceptSets, NULL, NULL)
  expect_false("Note" %in% names(withoutMatches))
})

test_that("the cohort page finds concept sets from its JSON when the link table has no rows for it", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  withConnection(pool, function(connection) {
    DatabaseConnector::executeSql(
      connection,
      paste0(
        "UPDATE main.cohort_definition SET circe_cohort_expression = '",
        '{"ConceptSets":[{"id":0,"name":"Asthma","expression":{"items":[{"concept":{"CONCEPT_ID":317009},"isExcluded":false,"includeDescendants":true,"includeMapped":false}]}}],"PrimaryCriteria":{}}',
        "' WHERE cohort_definition_id = 10;"
      ),
      progressBar = FALSE, reportOverallTime = FALSE
    )
  })
  testServer(
    cohortServer,
    args = list(connectionPool = pool, databaseSchema = "main", cohortDefinitionId = reactiveVal(10L), navigate = function(page = NULL, id = NULL) NULL),
    {
      expect_equal(details()$conceptSets$conceptSetName, "Asthma")
      expect_true(details()$conceptSetsFromJson)
      expect_match(as.character(output$conceptSetNote$html), "cohort_definition_to_concept_set", fixed = TRUE)
    }
  )
  testServer(
    cohortServer,
    args = list(connectionPool = pool, databaseSchema = "main", cohortDefinitionId = reactiveVal(3L), navigate = function(page = NULL, id = NULL) NULL),
    {
      expect_false(details()$conceptSetsFromJson)
      expect_equal(details()$conceptSets$conceptSetName, "Asthma (exact)")
    }
  )
})

circeSet <- function(name, ...) {
  items <- vapply(list(...), function(item) {
    sprintf(
      '{"concept":{"CONCEPT_ID":%d},"isExcluded":%s,"includeDescendants":%s,"includeMapped":false}',
      item[[1]], tolower(as.character(item[[2]])), tolower(as.character(item[[3]]))
    )
  }, "")
  sprintf('{"id":0,"name":"%s","expression":{"items":[%s]}}', name, paste(items, collapse = ","))
}

cohortWithSets <- function(id, ...) {
  data.frame(
    cohortDefinitionId = id,
    circeCohortExpression = sprintf('{"ConceptSets":[%s],"PrimaryCriteria":{}}', paste(c(...), collapse = ",")),
    stringsAsFactors = FALSE
  )
}

test_that("matchCohortConceptSetsInDatabase matches concept sets by identical content in one query", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  addIdenticalConceptSet(pool)
  cohorts <- rbind(
    # Same content as sets 1 and 20 (under another name), and as set 8 (items in another order).
    cohortWithSets(
      1L,
      circeSet("Whatever", list(317009, FALSE, TRUE)),
      circeSet("Also whatever", list(255573, TRUE, TRUE), list(317009, FALSE, TRUE))
    ),
    # No content match for the first set but the same name as set 4; the second one has nothing to match.
    cohortWithSets(2L, circeSet("copd", list(999, FALSE, TRUE)), circeSet("Nothing", list(888, FALSE, FALSE))),
    # Two items where stored sets have one (set 1) or a different exclusion flag (set 8): not the same content.
    cohortWithSets(3L, circeSet("Close", list(317009, FALSE, TRUE), list(255573, FALSE, TRUE))),
    # No concept sets, and invalid JSON.
    data.frame(cohortDefinitionId = 4L, circeCohortExpression = '{"ConceptSets":[]}', stringsAsFactors = FALSE),
    data.frame(cohortDefinitionId = 5L, circeCohortExpression = "not json", stringsAsFactors = FALSE)
  )

  result <- withConnection(pool, function(connection) matchCohortConceptSetsInDatabase(connection, "main", cohorts))
  links <- paste(result$links$cohortDefinitionId, result$links$conceptSetId)
  expect_setequal(links, c("1 1", "1 20", "1 8", "2 4"))
  expect_equal(result$report$conceptSets, 5)
  expect_equal(result$report$matchedByContent, 2)
  expect_equal(result$report$matchedByName, 1)
  expect_equal(result$report$duplicateContent, 1)
  expect_setequal(result$report$unmatched$name, c("Nothing", "Close"))

  # Which concept set in each cohort's JSON matched which stored set, and how.
  matches <- result$matches
  expect_setequal(paste(matches$cohortDefinitionId, matches$conceptSetId, matches$how), c("1 1 content", "1 20 content", "1 8 content", "2 4 name"))
  expect_equal(matches$name[matches$conceptSetId == 20], "Whatever")
  expect_equal(matches$conceptSetName[matches$conceptSetId == 20], "Asthma copy")
  expect_equal(matches$conceptSetName[matches$conceptSetId == 4], "COPD")

  noNames <- withConnection(pool, function(connection) matchCohortConceptSetsInDatabase(connection, "main", cohorts, matchByName = FALSE))
  expect_false("2 4" %in% paste(noNames$links$cohortDefinitionId, noNames$links$conceptSetId))
  expect_equal(noNames$report$matchedByName, 0)

  lowest <- withConnection(pool, function(connection) matchCohortConceptSetsInDatabase(connection, "main", cohorts, onDuplicates = "lowest"))
  expect_setequal(paste(lowest$links$cohortDefinitionId, lowest$links$conceptSetId), c("1 1", "1 8", "2 4"))

  # The temporary table is gone afterwards, so the function can run again on the same connection.
  again <- withConnection(pool, function(connection) matchCohortConceptSetsInDatabase(connection, "main", cohorts))
  expect_equal(nrow(again$links), 4)

  none <- withConnection(pool, function(connection) matchCohortConceptSetsInDatabase(connection, "main", cohorts[4:5, ]))
  expect_equal(nrow(none$links), 0)
  expect_equal(none$report$conceptSets, 0)
})

test_that("the cohort page marks identical stored concept sets and reports JSON sets without a match", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  addIdenticalConceptSet(pool)
  withConnection(pool, function(connection) {
    DatabaseConnector::executeSql(
      connection,
      paste0(
        "UPDATE main.cohort_definition SET circe_cohort_expression = '",
        '{"ConceptSets":[',
        '{"id":0,"name":"Asthma in Atlas","expression":{"items":[{"concept":{"CONCEPT_ID":317009},"isExcluded":false,"includeDescendants":true,"includeMapped":false}]}},',
        '{"id":1,"name":"Only in the JSON","expression":{"items":[{"concept":{"CONCEPT_ID":888}}]}}',
        '],"PrimaryCriteria":{}}',
        "' WHERE cohort_definition_id = 10;"
      ),
      progressBar = FALSE, reportOverallTime = FALSE
    )
  })
  testServer(
    cohortServer,
    args = list(connectionPool = pool, databaseSchema = "main", cohortDefinitionId = reactiveVal(10L), navigate = function(page = NULL, id = NULL) NULL),
    {
      expect_setequal(details()$conceptSets$conceptSetId, c(1, 20))
      table <- cohortConceptSetTable(details()$conceptSets, details()$conceptSetMatches, details()$atlasBaseUrl)
      expect_true(all(table[["In the cohort JSON as"]] == "Asthma in Atlas"))
      expect_true(all(grepl("Same content as", table$Note, fixed = TRUE)))
      note <- as.character(output$conceptSetNote$html)
      expect_match(note, "Only in the JSON", fixed = TRUE)
      expect_match(note, "no stored match", fixed = TRUE)
      expect_match(note, "identical content", fixed = TRUE)
    }
  )
})
