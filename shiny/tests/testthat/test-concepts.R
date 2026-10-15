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
