test_that("normalizeName ignores case, punctuation and plural/inflected forms", {
  expect_equal(Pegasus:::normalizeName("Asthma, Chronic"), Pegasus:::normalizeName("asthma   chronic"))
  expect_equal(Pegasus:::normalizeName("Heart Diseases"), Pegasus:::normalizeName("heart disease"))
  expect_equal(Pegasus:::normalizeNames(character()), character())
})

test_that("quoteSqlString escapes single quotes", {
  expect_equal(Pegasus:::quoteSqlString("Crohn's"), "'Crohn''s'")
})

test_that("withCache evaluates once per file and reuses the cached value", {
  folder <- withr::local_tempdir()
  calls <- 0
  compute <- function() {
    calls <<- calls + 1
    calls
  }
  first <- Pegasus:::withCache(compute(), cacheFolder = folder, fileName = "value.rds")
  second <- Pegasus:::withCache(compute(), cacheFolder = folder, fileName = "value.rds")
  expect_equal(first, second)
  expect_equal(calls, 1)

  expect_equal(Pegasus:::withCache(compute(), cacheFolder = NULL, fileName = "value.rds"), 2)
})

test_that("convertCirceConceptSetToCapr produces code that reflects the item flags", {
  json <- createCirceConceptSet(list(1, FALSE, TRUE), list(2, FALSE, FALSE), list(3, TRUE, FALSE))
  result <- Pegasus:::convertCirceConceptSetToCapr(json, "Asthma")
  code <- if (is.data.frame(result)) result$caprConceptSetExpression[1] else as.character(result)[1]
  expect_match(code, "descendants\\(1\\)")
  expect_match(code, "Asthma")
  expect_match(code, "exclude\\(3\\)|3")
})
