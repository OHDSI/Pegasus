setItem <- function(id, name, excluded = 0, descendants = 0, mapped = 0) {
  list(concept = list(CONCEPT_ID = id, CONCEPT_NAME = name), isExcluded = excluded == 1,
       includeDescendants = descendants == 1, includeMapped = mapped == 1)
}

expression <- function(...) as.character(jsonlite::toJSON(list(items = list(...)), auto_unbox = TRUE))

sets <- function(phenotypeId, definitions) {
  data.frame(
    phenotypeId = phenotypeId, conceptSetId = seq_along(definitions), conceptSetName = names(definitions),
    pertains = "Disease of interest", category = "Disease of interest",
    circeConceptSetExpression = unlist(definitions, use.names = FALSE), stringsAsFactors = FALSE
  )
}

test_that("wordDiff marks added and removed words", {
  diff <- wordDiff("chronic airway disease with wheeze", "chronic airway inflammation with wheeze and cough")
  expect_equal(diff$token, c("chronic", "airway", "disease", "inflammation", "with", "wheeze", "and", "cough"))
  expect_equal(diff$op, c("same", "same", "del", "add", "same", "same", "add", "add"))
  expect_true(all(wordDiff("same words", "same words")$op == "same"))
  expect_equal(nrow(wordDiff("", "new text")), 2)
  expect_null(wordDiff(strrep("a ", 900), "b", maxTokens = 800))
})

test_that("diffHtml escapes the text and marks changes", {
  html <- diffHtml("a <b> c", "a <i> c")
  expect_match(html, '<del class="diff-del">&lt;b&gt;</del>', fixed = TRUE)
  expect_match(html, '<ins class="diff-add">&lt;i&gt;</ins>', fixed = TRUE)
  expect_false(grepl("<i>", html, fixed = TRUE))
  expect_match(diffHtml(strrep("a ", 900), "b"), "too long")
})

test_that("compareConceptSets tells same, changed, renamed, and one-sided sets apart", {
  asthma <- expression(setItem(317009, "Asthma", descendants = 1))
  asthmaNoDescendants <- expression(setItem(317009, "Asthma"))
  asthmaExtra <- expression(setItem(317009, "Asthma", descendants = 1), setItem(255573, "COPD", excluded = 1))
  base <- sets(1, list(Asthma = asthma, Wheeze = expression(setItem(1, "Wheeze")), "Only base" = expression(setItem(9, "Nine"))))
  other <- sets(2, list(Asthma = asthmaExtra, Wheezing = expression(setItem(1, "Wheeze")), "Only other" = expression(setItem(8, "Eight"))))

  result <- compareConceptSets(base, other)
  byOther <- setNames(result$change, ifelse(nzchar(result$otherName), result$otherName, result$baselineName))
  expect_equal(byOther[["Asthma"]], "Changed")
  expect_equal(byOther[["Wheezing"]], "Same")
  expect_equal(byOther[["Only other"]], "Only here")
  expect_equal(byOther[["Only base"]], "Only in baseline")
  expect_match(result$details[result$otherName == "Asthma"], "Added: COPD (255573)", fixed = TRUE)
  expect_match(result$details[result$otherName == "Wheezing"], "different name", fixed = TRUE)

  flags <- compareConceptSets(sets(1, list(Asthma = asthma)), sets(2, list(Asthma = asthmaNoDescendants)))
  expect_equal(flags$change, "Changed")
  expect_match(flags$details, "Asthma (317009): descendants off", fixed = TRUE)

  expect_equal(nrow(compareConceptSets(sets(1, list(A = asthma))[0, ], sets(2, list(A = asthma))[0, ])), 0)
})

test_that("a comparison covers definitions, concept sets and performance for one database and type", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))
  evaluations <- withConnection(pool, function(connection) getEvaluations(connection, "main"))
  expressions <- withConnection(pool, function(connection) getPhenotypeConceptSetExpressions(connection, "main", c(1L, 2L)))
  expect_setequal(expressions$conceptSetId, c(1, 2, 3))

  comparison <- assembleComparison(phenotypes, evaluations, expressions, c(1L, 2L), 1L, "DEVCLM", "KEEPER + llm")
  expect_equal(comparison$phenotypes$phenotypeId, c(1, 2))
  # Phenotype 1 has three cohort definitions with results here and phenotype 2 has two, so each also gets a median row.
  metrics <- comparison$metrics
  expect_equal(sum(metrics$phenotypeId == 1), 4)
  expect_equal(sum(metrics$phenotypeId == 2), 3)
  expect_equal(sum(metrics$isSummary & metrics$phenotypeId == 1), 1)
  baselineRow <- metrics[metrics$phenotypeId == 1 & metrics$isSummary, ]
  otherRow <- metrics[metrics$phenotypeId == 2 & metrics$isSummary, ]
  expect_match(baselineRow$cohortDefinitionName, "Median of 3 cohort definitions")
  expect_equal(baselineRow$ppv, median(evaluations$ppv[evaluations$phenotypeId == 1 & evaluations$cdmSourceAbbreviation == "DEVCLM" & evaluations$evaluationType == "KEEPER + llm"]))
  expect_equal(otherRow$ppv, median(evaluations$ppv[evaluations$phenotypeId == 2 & evaluations$cdmSourceAbbreviation == "DEVCLM" & evaluations$evaluationType == "KEEPER + llm"]))
  expect_true(all(is.na(metrics$ppvDelta[metrics$phenotypeId == 1])))
  expect_equal(otherRow$ppvDelta, 100 * (otherRow$ppv - baselineRow$ppv))
  expect_equal(otherRow$sensitivityDelta, 100 * (otherRow$sensitivity - baselineRow$sensitivity))
  expect_false(anyNA(metrics$specificity))
  expect_length(comparison$notes, 1)
  expect_match(comparison$notes, "PPV [+-][0-9.]+ pts, sensitivity [+-][0-9.]+ pts \\(median of 2 cohort definitions\\)")
  expect_match(comparison$notes, "concept sets: ")
  expect_equal(names(comparison$definitionHtml), c("1", "2"))

  none <- assembleComparison(phenotypes, evaluations, expressions, c(1L, 2L), 1L, "DEVEHR", "PheValuator")
  expect_match(none$notes, "no evaluation for the chosen database and evaluation type")
  expect_true(all(is.na(none$metrics$cohortDefinitionName)))
})

test_that("defaultMetricChoice picks the pair covering the most phenotypes", {
  evaluations <- data.frame(
    phenotypeId = c(1, 2, 2, 3), cdmSourceAbbreviation = c("A", "A", "B", "B"),
    evaluationType = c("KEEPER + llm", "KEEPER + llm", "PheValuator", "PheValuator"), stringsAsFactors = FALSE
  )
  choice <- defaultMetricChoice(evaluations[evaluations$phenotypeId %in% c(2, 3), ])
  expect_equal(choice$database, "B")
  expect_equal(choice$evaluationType, "PheValuator")
  expect_null(defaultMetricChoice(evaluations[0, ])$database)
})

test_that("the report is a standalone page with the comparison in it", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))
  evaluations <- withConnection(pool, function(connection) getEvaluations(connection, "main"))
  expressions <- withConnection(pool, function(connection) getPhenotypeConceptSetExpressions(connection, "main", c(1L, 2L)))
  comparison <- assembleComparison(phenotypes, evaluations, expressions, c(1L, 2L), 1L, "DEVCLM", "KEEPER + llm")
  html <- comparisonReport(comparison)
  expect_match(html, "^<!DOCTYPE html>")
  expect_match(html, "Phenotype comparison", fixed = TRUE)
  expect_match(html, phenotypes$phenotypeName[phenotypes$phenotypeId == 2], fixed = TRUE)
  expect_match(html, "(baseline)", fixed = TRUE)
  expect_false(grepl("http", html))
})

test_that("the Compare module builds the comparison from the chosen phenotypes", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main")))
  evaluations <- reactiveVal(withConnection(pool, function(connection) getEvaluations(connection, "main")))
  preselect <- reactiveVal(NULL)

  testServer(
    compareServer,
    args = list(
      connectionPool = pool, databaseSchema = "main", phenotypes = phenotypes, evaluations = evaluations,
      preselect = preselect, onOpenPhenotype = function(id) NULL
    ),
    {
      session$setInputs(phenotypes = c("1", "2"), baseline = "1", database = "DEVCLM", evaluationType = "KEEPER + llm")
      result <- comparison()
      expect_equal(result$phenotypes$phenotypeId, c(1, 2))
      expect_equal(result$baselineId, 1)
      expect_equal(result$database, "DEVCLM")

      # Without CirceR (or with a placeholder cohort) the pretty-printed JSON is compared, starting on each
      # phenotype's first cohort definition.
      expect_true(comparison()$cohortDiffs[["2"]]$available)
      expect_equal(comparison()$cohortDiffs[["2"]]$baselineName, "Asthma - template")
      expect_equal(comparison()$cohortDiffs[["2"]]$otherName, "T2DM - template")
      session$setInputs(cohort_2 = "5")
      expect_equal(comparison()$cohortDiffs[["2"]]$otherName, "T2DM - AI")
      session$setInputs(cohort_2 = "1")
      expect_equal(comparison()$cohortDiffs[["2"]]$otherName, "T2DM - template")

      session$setInputs(baseline = "2")
      expect_equal(comparison()$baselineId, 2)
    }
  )
})

test_that("Shortlist and Performance hand their phenotypes to the compare callback", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main")))
  evaluations <- reactiveVal(withConnection(pool, function(connection) getEvaluations(connection, "main")))
  compared <- NULL

  testServer(
    shortlistServer,
    args = list(
      connectionPool = pool, databaseSchema = "main", phenotypes = phenotypes, evaluations = evaluations,
      shortlist = reactiveVal(c(1L, 2L, 8L)), onOpenPhenotype = function(id) NULL,
      onCompare = function(ids) compared <<- ids
    ),
    {
      session$setInputs(compare = 1)
    }
  )
  expect_setequal(compared, c(1, 2, 8))

  compared <- NULL
  testServer(
    shortlistServer,
    args = list(
      connectionPool = pool, databaseSchema = "main", phenotypes = phenotypes, evaluations = evaluations,
      shortlist = reactiveVal(1L), onOpenPhenotype = function(id) NULL, onCompare = function(ids) compared <<- ids
    ),
    {
      session$setInputs(compare = 1)
    }
  )
  expect_null(compared)
})

test_that("the summary row of a phenotype is the median of its cohort definitions", {
  evaluations <- data.frame(
    phenotypeId = c(1, 1, 1, 2), phenotypeName = c("A", "A", "A", "B"), cohortDefinitionName = c("c1", "c2", "c3", "d1"),
    cdmSourceAbbreviation = "DB", evaluationType = "T", ppv = c(0.5, 0.9, 0.7, 0.6), sensitivity = c(0.2, 0.4, 0.3, 0.9),
    trueNegatives = 90, falsePositives = 10, falseNegatives = 10, stringsAsFactors = FALSE
  )
  phenotypes <- data.frame(phenotypeId = c(1, 2), phenotypeName = c("A", "B"))
  metrics <- comparisonMetrics(evaluations, phenotypes, c(1, 2), 1, "DB", "T")
  expect_equal(nrow(metrics), 5)
  summary <- metrics[metrics$isSummary, ]
  expect_equal(summary$ppv, c(0.7, 0.6))
  expect_equal(summary$sensitivity, c(0.3, 0.9))
  expect_equal(summary$ppvDelta[2], 100 * (0.6 - 0.7))
  expect_equal(summary$specificity, c(0.9, 0.9))
  # A median ignores missing measures, and is missing only when every cohort definition lacks it.
  evaluations$ppv[1:2] <- NA
  expect_equal(comparisonMetrics(evaluations, phenotypes, c(1, 2), 1, "DB", "T")$ppv[1], 0.7)
  evaluations$ppv[3] <- NA
  expect_true(is.na(comparisonMetrics(evaluations, phenotypes, c(1, 2), 1, "DB", "T")$ppv[1]))
})

test_that("cohort descriptions are compared line by line and shown with context", {
  old <- "# Cohort\n\nEntry: asthma\nExit: end of observation\nAge >= 18\n"
  new <- "# Cohort\n\nEntry: asthma\nExit: 30 days later\nAge >= 18\nNo COPD"
  diff <- cohortLineDiff(old, new)
  expect_equal(diff$op, c("same", "same", "del", "add", "same", "add"))
  expect_equal(diff$line[diff$op == "del"], "Exit: end of observation")
  expect_null(cohortLineDiff(paste(rep("a", 900), collapse = "\n"), "b"))

  html <- lineDiffHtml(diff)
  expect_match(html, '<div class="line-add">+ No COPD</div>', fixed = TRUE)
  expect_match(html, '<div class="line-del">- Exit: end of observation</div>', fixed = TRUE)
  long <- cohortLineDiff(paste0("line", 1:30, collapse = "\n"), paste0(c(paste0("line", 1:30), "extra"), collapse = "\n"))
  trimmed <- lineDiffHtml(long, contextLines = 2)
  expect_match(trimmed, "28 unchanged lines hidden", fixed = TRUE)
  expect_match(lineDiffHtml(cohortLineDiff("a", "a"), contextLines = 2), "read the same")
  expect_match(lineDiffHtml(cohortLineDiff("<b>", "<i>")), "&lt;i&gt;", fixed = TRUE)
  expect_match(lineDiffHtml(NULL), "too long")
})

test_that("cohortTextsFor uses the readable text and falls back to the JSON", {
  expressions <- data.frame(
    cohortDefinitionId = c(10L, 11L), cohortDefinitionName = c("First", "Second"), phenotypeId = c(1L, 2L),
    circeCohortExpression = c('{"a": 1}', '{"b": 2}'), stringsAsFactors = FALSE
  )
  readable <- cohortTextsFor(expressions, list("1" = 10L, "2" = 11L), render = function(json) paste("Readable", json))
  expect_equal(readable[["1"]]$text, 'Readable {"a": 1}')
  expect_true(readable[["1"]]$readable)
  expect_equal(readable[["2"]]$name, "Second")

  fallback <- cohortTextsFor(expressions, list("1" = 10L), render = function(json) stop("no Java"))
  expect_false(fallback[["1"]]$readable)
  expect_match(fallback[["1"]]$text, '"a"', fixed = TRUE)

  # A choice that does not belong to the phenotype is left out.
  expect_length(cohortTextsFor(expressions, list("1" = 11L), render = identity), 0)
})

test_that("the comparison includes the cohort definition differences and the report shows them", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  phenotypes <- withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))
  evaluations <- withConnection(pool, function(connection) getEvaluations(connection, "main"))
  expressions <- withConnection(pool, function(connection) getPhenotypeConceptSetExpressions(connection, "main", c(1L, 2L)))
  texts <- list(
    "1" = list(cohortDefinitionId = 1L, name = "Asthma - template", text = "Entry: asthma\nExit: end", readable = TRUE),
    "2" = list(cohortDefinitionId = 4L, name = "T2DM - template", text = "Entry: diabetes\nExit: end\nAge >= 18", readable = TRUE)
  )
  comparison <- assembleComparison(phenotypes, evaluations, expressions, c(1L, 2L), 1L, "DEVCLM", "KEEPER + llm", cohortTexts = texts)
  expect_true(comparison$cohortDiffs[["2"]]$available)
  expect_equal(comparison$cohortDiffs[["2"]]$baselineName, "Asthma - template")
  expect_match(comparison$notes, "cohort definition: 2 lines added, 1 removed", fixed = TRUE)

  html <- comparisonReport(comparison)
  expect_match(html, "T2DM - template compared with Asthma - template", fixed = TRUE)
  expect_match(html, '<div class="line-add">+ Age &gt;= 18</div>', fixed = TRUE)

  missing <- assembleComparison(phenotypes, evaluations, expressions, c(1L, 2L), 1L, "DEVCLM", "KEEPER + llm", cohortTexts = texts[1])
  expect_false(missing$cohortDiffs[["2"]]$available)
  expect_match(comparisonReport(missing), "cohort definition is missing")
})
