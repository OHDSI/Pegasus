items <- function(...) {
  rows <- list(...)
  data.frame(
    conceptSetId = vapply(rows, function(r) r[[1]], 1),
    conceptId = vapply(rows, function(r) r[[2]], 1),
    isExcluded = vapply(rows, function(r) r[[3]], 1),
    includeDescendants = vapply(rows, function(r) r[[4]], 1),
    includeMapped = vapply(rows, function(r) r[[5]], 1)
  )
}

test_that("sanitizeSearchTerm keeps plain text only", {
  expect_equal(sanitizeSearchTerm("  Type 2 diabetes  "), "Type 2 diabetes")
  expect_equal(sanitizeSearchTerm("a'; DROP TABLE x; --"), "a DROP TABLE x -")
  expect_equal(sanitizeSearchTerm("50%_off\\"), "50off")
  expect_equal(nchar(sanitizeSearchTerm(strrep("a", 300))), 100)
  expect_false(grepl("--", sanitizeSearchTerm("a---b"), fixed = TRUE))
})

test_that("matchConceptSetItems follows direct, descendant and mapped rules", {
  # Concept 10 has ancestors 10 and 5.
  sets <- items(
    c(1, 10, 0, 0, 0), # lists the concept itself
    c(2, 5, 0, 1, 0),  # ancestor with descendants
    c(3, 5, 0, 0, 0),  # ancestor without descendants: does not cover
    c(4, 99, 0, 0, 1), # lists a concept 10 maps to
    c(5, 7, 0, 0, 0)   # unrelated
  )
  result <- matchConceptSetItems(sets, 10, ancestorIds = c(10, 5), mappedIds = 99)
  expect_equal(result$conceptSetId, c(1, 2, 4))
  expect_equal(result$matchType, c("Direct", "Descendant", "Mapped"))
})

test_that("matchConceptSetItems drops concept sets that exclude the concept", {
  sets <- items(
    c(1, 5, 0, 1, 0), c(1, 6, 1, 1, 0), # includes ancestor 5, excludes ancestor 6 with descendants
    c(2, 5, 0, 1, 0), c(2, 6, 1, 0, 0), # excludes ancestor 6 without descendants: does not cover
    c(3, 5, 0, 1, 0), c(3, 10, 1, 0, 0) # excludes the concept itself
  )
  result <- matchConceptSetItems(sets, 10, ancestorIds = c(10, 5, 6))
  expect_equal(result$conceptSetId, 2)
})

test_that("matchConceptSetItems reports each set once by its strongest match", {
  sets <- items(c(1, 5, 0, 1, 0), c(1, 10, 0, 0, 0))
  result <- matchConceptSetItems(sets, 10, ancestorIds = c(10, 5))
  expect_equal(nrow(result), 1)
  expect_equal(result$matchType, "Direct")
  expect_equal(nrow(matchConceptSetItems(emptyItems(), 10, 10)), 0)
})

test_that("vocabulary queries find concepts, ancestors and mappings", {
  vocabulary <- createVocabularyPool()
  on.exit(pool::poolClose(vocabulary))

  found <- withConnection(vocabulary, function(connection) searchConcepts(connection, "main", "ASTHMA"))
  expect_equal(found$conceptId[1:2], c(317009, 4051466))
  expect_false(9002 %in% found$conceptId)
  expect_equal(nrow(withConnection(vocabulary, function(connection) searchConcepts(connection, "main", "''; --"))), 0)

  ancestors <- withConnection(vocabulary, function(connection) getConceptAncestors(connection, "main", 4051466))
  expect_setequal(ancestors, c(4051466, 317009))
  mapped <- withConnection(vocabulary, function(connection) getMappedConcepts(connection, "main", 9001))
  expect_equal(mapped, 317009)
  expect_length(withConnection(vocabulary, function(connection) getMappedConcepts(connection, "main", 9002)), 0)
})

test_that("findPhenotypesByConcept finds phenotypes through descendants, and only exact matches without a vocabulary", {
  pool <- createTestPool()
  vocabulary <- createVocabularyPool()
  on.exit({
    pool::poolClose(pool)
    pool::poolClose(vocabulary)
  })
  loadSeedData(pool)

  # Without a vocabulary: only concept sets listing asthma itself (sets 1, 2 and 8).
  exact <- withConnection(pool, function(connection) findPhenotypesByConcept(connection, "main", 317009))
  expect_equal(nrow(exact), 3)
  expect_setequal(exact$phenotypeId, c(1, 8))
  expect_true(all(exact$matchType == "Direct"))

  # A descendant is found through concept sets with "include descendants" (1 and 8), not the exact-only set 2.
  descendants <- withConnection(pool, function(pool_connection) {
    withConnection(vocabulary, function(vocabularyConnection) {
      findPhenotypesByConcept(pool_connection, "main", 4051466, vocabularyConnection, "main")
    })
  })
  expect_setequal(descendants$conceptSetName, c("Asthma", "Asthma with exclusion of COPD"))
  expect_true(all(descendants$matchType == "Descendant"))
  expect_setequal(descendants$phenotypeId, c(1, 8))

  # Without a vocabulary a descendant cannot be resolved.
  expect_equal(nrow(withConnection(pool, function(connection) findPhenotypesByConcept(connection, "main", 4051466))), 0)
})

test_that("getVocabularyVersion reads the release from the vocabulary table", {
  vocabulary <- createVocabularyPool()
  on.exit(pool::poolClose(vocabulary))
  expect_equal(withConnection(vocabulary, function(connection) getVocabularyVersion(connection, "main")), "v5.0 TEST-RELEASE")

  withConnection(vocabulary, function(connection) {
    DatabaseConnector::executeSql(connection, "DELETE FROM main.vocabulary;", progressBar = FALSE, reportOverallTime = FALSE)
  })
  expect_true(is.na(withConnection(vocabulary, function(connection) getVocabularyVersion(connection, "main"))))
})

test_that("parseConceptId accepts only digits", {
  expect_equal(parseConceptId(" 317009 "), 317009L)
  expect_true(is.na(parseConceptId("asthma")))
  expect_true(is.na(parseConceptId("31.7")))
  expect_true(is.na(parseConceptId("1234567890")))
  expect_true(is.na(parseConceptId("")))
})

test_that("runVocabularyTask covers every task the app uses", {
  vocabulary <- createVocabularyPool()
  on.exit(pool::poolClose(vocabulary))
  run <- function(task, args = list()) {
    withConnection(vocabulary, function(connection) runVocabularyTask(task, args, connection, "main"))
  }

  expect_equal(run("check"), "v5.0 TEST-RELEASE")
  expect_equal(run("lookup", list(conceptId = 317009L))$conceptName, "Asthma")
  expect_equal(run("search", list(term = "asthma"))$conceptId[1:2], c(317009, 4051466))

  context <- run("context", list(conceptId = 4051466L))
  expect_setequal(context$context$ancestorIds, c(4051466, 317009))
  expect_length(context$context$mappedIds, 0)
  expect_setequal(context$concepts$conceptId, c(4051466, 317009))

  mapped <- run("context", list(conceptId = 9001L))
  expect_equal(mapped$context$mappedIds, 317009)
  expect_true(317009 %in% mapped$context$mappedAncestorIds)

  expect_error(run("nonsense"), "Unknown vocabulary task")
})

test_that("the background runner runs a task in a separate R process and relays its log", {
  skip_if_not_installed("callr")
  skip_if_not_installed("duckdb")
  file <- createVocabularyFile()
  environment <- c(PEGASUS_VOCAB_DBMS = "duckdb", PEGASUS_VOCAB_SERVER = file, PEGASUS_VOCAB_SCHEMA = "main")
  previous <- Sys.getenv(names(environment), unset = NA)
  do.call(Sys.setenv, as.list(environment))
  on.exit({
    for (name in names(environment)) {
      if (is.na(previous[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, stats::setNames(list(previous[[name]]), name))
    }
    unlink(file)
  })

  runner <- backgroundVocabularyRunner(normalizePath(file.path("..", "..")))
  waitFor <- function(handle) {
    lines <- character(0)
    deadline <- Sys.time() + 120
    repeat {
      polled <- handle$poll()
      lines <- c(lines, polled$lines)
      if (isTRUE(polled$done)) {
        return(c(polled, list(allLines = lines)))
      }
      if (Sys.time() > deadline) {
        handle$kill()
        stop("The background task did not finish in time.")
      }
      Sys.sleep(0.2)
    }
  }

  checked <- waitFor(runner("check", list()))
  expect_true(checked$ok)
  expect_equal(checked$value, "v5.0 TEST-RELEASE")
  expect_true(any(grepl("Connecting to the vocabulary database", checked$allLines, fixed = TRUE)))
  expect_true(any(grepl("Query finished", checked$allLines, fixed = TRUE)))

  searched <- waitFor(runner("search", list(term = "asthma")))
  expect_true(searched$ok)
  expect_equal(searched$value$conceptId[1:2], c(317009, 4051466))

  failed <- waitFor(runner("nonsense", list()))
  expect_false(failed$ok)
  expect_match(failed$error, "Unknown vocabulary task")
})

conceptsTestArgs <- function(pool, vocabulary) {
  list(
    connectionPool = pool,
    databaseSchema = "main",
    vocabularyRunner = if (is.null(vocabulary)) NULL else inlineVocabularyRunner(vocabulary),
    phenotypes = reactiveVal(withConnection(pool, function(connection) getPhenotypeSummary(connection, "main"))),
    shortlist = reactiveVal(integer(0)),
    onOpenPhenotype = function(id) NULL
  )
}

test_that("concepts server: the connection check reports the status and vocabulary version", {
  pool <- createTestPool()
  vocabulary <- createVocabularyPool()
  on.exit({
    pool::poolClose(pool)
    pool::poolClose(vocabulary)
  })
  loadSeedData(pool)

  testServer(conceptsServer, args = conceptsTestArgs(pool, vocabulary), {
    session$setInputs(checkVocabulary = 1)
    expect_equal(vocabStatus()$state, "connected")
    expect_equal(vocabStatus()$version, "v5.0 TEST-RELEASE")
    expect_null(activeJob())
  })
})

test_that("concepts server: ID mode looks the concept up directly", {
  pool <- createTestPool()
  vocabulary <- createVocabularyPool()
  on.exit({
    pool::poolClose(pool)
    pool::poolClose(vocabulary)
  })
  loadSeedData(pool)

  testServer(conceptsServer, args = conceptsTestArgs(pool, vocabulary), {
    session$setInputs(mode = "id", term = "317009", search = 1)
    expect_equal(selectedConcept()$id, 317009L)
    expect_equal(selectedConcept()$name, "Asthma")
    expect_equal(nrow(candidates()), 0)
    expect_equal(matchState(), "ready")
    expect_equal(nrow(matches()), 3)
    expect_true("Asthma" %in% matches()$itemConceptName)

    # Text in ID mode is rejected without a lookup.
    session$setInputs(term = "asthma", search = 2)
    expect_null(selectedConcept())
    expect_equal(matchState(), "idle")
  })
})

test_that("concepts server: name mode fuzzy-matches concept names into candidates", {
  pool <- createTestPool()
  vocabulary <- createVocabularyPool()
  on.exit({
    pool::poolClose(pool)
    pool::poolClose(vocabulary)
  })
  loadSeedData(pool)

  testServer(conceptsServer, args = conceptsTestArgs(pool, vocabulary), {
    session$setInputs(mode = "name", term = "asthma", search = 1)
    expect_equal(candidates()$conceptId[1:2], c(317009, 4051466))
    expect_null(selectedConcept())

    session$setInputs(candidateTable_rows_selected = 2)
    expect_equal(selectedConcept()$id, 4051466)
    expect_setequal(matches()$phenotypeId, c(1, 8))
  })
})

test_that("concepts server: without a vocabulary only ID lookups work", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)

  testServer(conceptsServer, args = conceptsTestArgs(pool, NULL), {
    expect_equal(vocabStatus()$state, "none")
    session$setInputs(mode = "name", term = "asthma", search = 1)
    expect_equal(nrow(candidates()), 0)
    expect_null(selectedConcept())

    session$setInputs(mode = "id", term = "317009", search = 2)
    expect_equal(selectedConcept()$id, 317009L)
    expect_equal(nrow(matches()), 3)
  })
})

test_that("concepts server: a failing vocabulary task is reported without blocking", {
  pool <- createTestPool()
  on.exit(pool::poolClose(pool))
  loadSeedData(pool)
  failingRunner <- function(task, args) {
    list(
      poll = function() list(done = TRUE, lines = character(0), ok = FALSE, error = "connection refused"),
      kill = function() NULL
    )
  }
  args <- conceptsTestArgs(pool, NULL)
  args$vocabularyRunner <- failingRunner

  testServer(conceptsServer, args = args, {
    session$setInputs(mode = "name", term = "asthma", search = 1)
    expect_equal(vocabStatus()$state, "error")
    expect_null(activeJob())
    expect_equal(nrow(candidates()), 0)
  })
})
