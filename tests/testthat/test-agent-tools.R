library(dplyr)

tools <- asNamespace("Pegasus")

parsePipeTable <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  cells <- lapply(lines[-2], function(line) trimws(strsplit(gsub("^\\| | \\|$", "", line), " | ", fixed = TRUE)[[1]]))
  table <- as.data.frame(do.call(rbind, cells[-1]), stringsAsFactors = FALSE)
  colnames(table) <- cells[[1]]
  table
}

# Pure helpers ---------------------------------------------------------------------------------------------------------

test_that("formatPipeTable escapes cells, blanks zeros on request, and handles empty data", {
  data <- data.frame(name = c("a|b", "line1\nline2"), count = c(0, 3))
  lines <- strsplit(tools$formatPipeTable(data, zeroAsBlank = "count"), "\n")[[1]]

  expect_equal(lines[1], "| name | count |")
  expect_equal(lines[3], "| a\\|b |  |")
  expect_equal(lines[4], "| line1<br>line2 | 3 |")
  expect_length(strsplit(tools$formatPipeTable(data[0, ]), "\n")[[1]], 2)
})

test_that("conceptSetItemKeys is independent of item order", {
  a <- jsonlite::fromJSON(createCirceConceptSet(list(1, FALSE, TRUE), list(2, TRUE, FALSE)), simplifyVector = FALSE)
  b <- jsonlite::fromJSON(createCirceConceptSet(list(2, TRUE, FALSE), list(1, FALSE, TRUE)), simplifyVector = FALSE)
  c <- jsonlite::fromJSON(createCirceConceptSet(list(1, FALSE, FALSE), list(2, TRUE, FALSE)), simplifyVector = FALSE)

  expect_equal(tools$conceptSetItemKeys(a), tools$conceptSetItemKeys(b))
  expect_false(identical(tools$conceptSetItemKeys(a), tools$conceptSetItemKeys(c)))
  expect_length(tools$conceptSetItemKeys(list(items = list())), 0)
})

test_that("verifyConceptSetsAreStored accepts exact matches and rejects everything else", {
  stored <- tibble(conceptSetName = "Disease A",
                           caprConceptSetExpression = "cs(descendants(1))",
                           items = list("1|0|1|0"))

  expect_null(tools$verifyConceptSetsAreStored(list(list(name = "disease a", items = "1|0|1|0")), stored))
  expect_error(tools$verifyConceptSetsAreStored(list(list(name = "Disease A", items = "1|0|0|0")), stored),
               "does not match")
  expect_error(tools$verifyConceptSetsAreStored(list(list(name = "Disease X", items = "1|0|1|0")), stored,
                                                allNames = "Disease A"),
               "Disease A")
  expect_error(tools$verifyConceptSetsAreStored(list(list(name = "", items = character())), stored),
               "no name")
})

test_that("bindContext removes the context argument and forwards the remaining ones", {
  fun <- function(context, x, y = 2) paste(context, x, y)
  bound <- tools$bindContext(fun, "ctx")

  expect_equal(names(formals(bound)), c("x", "y"))
  expect_equal(bound(1), "ctx 1 2")
  expect_equal(bound(1, y = 3), "ctx 1 3")
})

test_that("tool input errors are reported before any database is touched", {
  expect_match(tools$samplePatientProfile(context = NULL, cohortId = 1, type = "XX"), "^Error")
  expect_error(tools$getConceptSetsCapr(context = NULL, conceptSetNames = "x", detail = "all"), "detail")
  windowError <- function(timeWindows) {
    expect_error(tools$countConceptSetPersonOverlap(context = NULL, caprCode = "", cohortId = 1,
                                                    timeWindows = timeWindows))
  }
  windowError(list(list(name = "a", startDay = 0)))
  windowError(list(list(name = "a", startDay = 0, endDay = 0), list(name = "a", startDay = 1, endDay = 1)))
  windowError(list(list(name = "a", startDay = 5, endDay = 1)))
  windowError(list(list(name = "a", startDay = 0.5, endDay = 1)))
  windowError(c("a", 0))
})

# Tools against a test CDM ---------------------------------------------------------------------------------------------

test_that("the agent context resolves databases by name and ID", {
  context <- createAgentContext()

  expect_equal(context$getDatabaseNames(), "TestCdm")
  expect_equal(unname(context$getDatabaseIds()), 1)
  expect_equal(context$getDatabaseReferenceFromId(1)$databaseName, "TestCdm")
  expect_error(context$getDatabaseReferenceFromId(99), "not found")
  expect_named(context$selectDatabaseReferences(NULL), "TestCdm")
  expect_named(context$selectDatabaseReferences("TestCdm"), "TestCdm")
  expect_error(context$selectDatabaseReferences("Nope"), "Valid databases: TestCdm")
  expect_length(context$tools(), 12)
})

test_that("listConceptSets lists the phenotype's concept sets with counts for the context's databases only", {
  context <- createAgentContext()

  table <- parsePipeTable(tools$listConceptSets(context))

  expect_setequal(table$conceptSetName, c("Disease A", "Disease B", "Lab test"))
  expect_true("persons in TestCdm" %in% colnames(table))
  expect_false("persons in OtherCdm" %in% colnames(table))
  expect_equal(table$`persons in TestCdm`[table$conceptSetName == "Disease B"], "21")
})

test_that("getConceptSetsCapr returns the requested concept sets in input order, case-insensitively", {
  context <- createAgentContext()

  code <- jsonlite::fromJSON(tools$getConceptSetsCapr(context, c("lab test", "DISEASE A"), detail = "code"))
  expect_equal(code$name, c("Lab test", "Disease A"))
  expect_equal(code$hasDescendants, c("N", "Y"))
  expect_false("countsInDatabase" %in% colnames(code))

  full <- jsonlite::fromJSON(tools$getConceptSetsCapr(context, "Disease A", detail = "full_reference"),
                             simplifyVector = FALSE)[[1]]
  expect_equal(full$countsInDatabase$TestCdm[[1]]$conditionPersons, 11)
  expect_null(full$countsInDatabase$OtherCdm)
  expect_equal(full$conceptReference[[1]]$conceptName, "Disease A")
})

test_that("validateCapr only accepts predefined concept sets used verbatim", {
  context <- createAgentContext()

  expect_equal(tools$validateCapr(context, diseaseACohortCapr), "Valid")
  modified <- sub("descendants(1)", "descendants(1, 4)", diseaseACohortCapr, fixed = TRUE)
  expect_match(tools$validateCapr(context, modified), "^Error: .*does not match")
  unknown <- sub('"Disease A"', '"Disease X"', diseaseACohortCapr, fixed = TRUE)
  expect_match(tools$validateCapr(context, unknown), "^Error: .*not a predefined concept set")
  expect_match(tools$validateCapr(context, 'system("ls")'), "^Error")
})

test_that("convertCaprToJson fills in concept details from the vocabulary", {
  context <- createAgentContext()

  cohort <- jsonlite::fromJSON(tools$convertCaprToJson(context, diseaseACohortCapr), simplifyVector = FALSE)

  concept <- cohort$ConceptSets[[1]]$expression$items[[1]]$concept
  expect_equal(concept$CONCEPT_ID, 1)
  expect_equal(concept$CONCEPT_NAME, "Disease A")
  expect_equal(concept$STANDARD_CONCEPT_CAPTION, "Standard")
})

test_that("generated cohorts are reused, counted, and characterized", {
  context <- createAgentContext()

  # CohortGenerator warns when a cohort has no inclusion rules:
  cohortId <- suppressWarnings(tools$generateCohort(context, diseaseACohortCapr))
  expect_equal(tools$generateCohort(context, diseaseACohortCapr), cohortId)
  otherCohort <- sub("descendants(1)", "4", sub('"Disease A"', '"Disease B"', diseaseACohortCapr, fixed = TRUE),
                     fixed = TRUE)
  expect_false(suppressWarnings(tools$generateCohort(context, otherCohort)) == cohortId)

  counts <- jsonlite::fromJSON(tools$getCohortCount(context, cohortId))
  expect_equal(counts$personCount, 2)

  rates <- jsonlite::fromJSON(tools$computeIncidenceRate(context, cohortId))
  overall <- rates[rates$stratum == "Overall", ]
  expect_equal(overall$persons, 4)
  expect_equal(overall$events, 2)
  expect_setequal(rates$stratumName[rates$stratum == "Sex"], c("Male", "Female"))

  # Disease B: persons 1 and 3; only person 1 is in the cohort, 17 days before index.
  overlap <- parsePipeTable(tools$countConceptSetPersonOverlap(context, 'cs(4, name = "Disease B")', cohortId))
  expect_equal(nrow(overlap), 5)
  expect_true(all(overlap$overallPersons == "2"))
  expect_equal(overlap$windowName[overlap$overallCohortPersons == "1"], "Prior month")

  customWindows <- list(list(name = "Around index", startDay = -30, endDay = 30))
  overlap <- parsePipeTable(tools$countConceptSetPersonOverlap(context, 'cs(4, name = "Disease B")', cohortId,
                                                               timeWindows = customWindows))
  expect_equal(overlap$overallCohortPersons, "1")
})

test_that("describeMeasurementValues summarizes values per unit", {
  context <- createAgentContext()

  values <- jsonlite::fromJSON(tools$describeMeasurementValues(context, 'cs(10, name = "Lab test")'))

  mgdl <- values[values$unitConceptName == "mg/dL", ]
  expect_equal(mgdl$measurementCount, 3)
  expect_equal(mgdl$missingValueCount, 1)
  expect_equal(c(mgdl$minValue, mgdl$maxValue, mgdl$meanValue), c(5, 7, 6))
  expect_equal(values$measurementCount[values$unitConceptName == "No unit"], 1)
})

test_that("getDatabaseDescription reports the CDM source description and person count", {
  context <- createAgentContext()

  description <- jsonlite::fromJSON(tools$getDatabaseDescription(context, "TestCdm"))

  expect_equal(description$description, "A tiny test CDM")
  expect_equal(description$personCount, 4)
})
