library(testthat)

item <- function(id, excluded = FALSE, descendants = FALSE, mapped = FALSE) {
  list(concept = list(CONCEPT_ID = id),
       isExcluded = excluded,
       includeDescendants = descendants,
       includeMapped = mapped)
}

stored <- data.frame(
  conceptSetName = c("Asthma", "Asthma", "Copd"),
  caprConceptSetExpression = c("cs(descendants(1), name = \"Asthma\")",
                               "cs(1, name = \"Asthma\")",
                               "cs(5, name = \"Copd\")"),
  stringsAsFactors = FALSE
)
stored$items <- list(
  Pegasus:::conceptSetItemKeys(list(items = list(item(1, descendants = TRUE), item(2)))),
  Pegasus:::conceptSetItemKeys(list(items = list(item(1), item(2)))),
  Pegasus:::conceptSetItemKeys(list(items = list(item(5))))
)
allNames <- c("Asthma", "Copd")

submit <- function(name, ...) {
  list(list(name = name, items = Pegasus:::conceptSetItemKeys(list(items = list(...)))))
}

test_that("an exact match of either stored variant passes, ignoring order and name case", {
  expect_null(Pegasus:::verifyConceptSetsAreStored(submit("asthma", item(2), item(1, descendants = TRUE)), stored))
  expect_null(Pegasus:::verifyConceptSetsAreStored(submit("ASTHMA", item(1), item(2)), stored))
})

test_that("extra, missing and flag-changed items fail and show the stored code", {
  expect_error(Pegasus:::verifyConceptSetsAreStored(submit("Asthma", item(1), item(2), item(3)), stored),
               "cs\\(descendants\\(1\\)")
  expect_error(Pegasus:::verifyConceptSetsAreStored(submit("Asthma", item(1)), stored), "predefined definition")
  expect_error(Pegasus:::verifyConceptSetsAreStored(submit("Asthma", item(1, excluded = TRUE), item(2)), stored),
               "predefined definition")
})

test_that("unknown names fail and suggest close names", {
  expect_error(Pegasus:::verifyConceptSetsAreStored(submit("Asthmaa", item(1)), stored, allNames),
               "Closest predefined names: 'Asthma'")
  expect_error(Pegasus:::verifyConceptSetsAreStored(submit("", item(1)), stored, allNames), "no name")
})
