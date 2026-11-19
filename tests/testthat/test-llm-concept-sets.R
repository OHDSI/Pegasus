library(dplyr)

createTestDatabaseReference <- function(connectionDetails) {
  Pegasus::createDatabaseReference(
    databaseName = "Test",
    connectionDetails = connectionDetails,
    tempEmulationSchema = NULL,
    cdmDatabaseSchema = testSchema,
    sensitiveCohortDatabaseSchema = testSchema, sensitiveCohortTable = "sensitive",
    referenceCohortDatabaseSchema = testSchema, referenceCohortTable = "reference",
    candidateCohortDatabaseSchema = testSchema, candidateCohortTable = "candidate"
  )
}

targetsHandler <- function(prompt, systemPrompt, type) {
  if (startsWith(prompt, "Phenotype:")) {
    list(concepts = tibble(category = c("Hypernym", "Clinical presentation", "Alternative diagnosis"),
                                   terms = list("Concept 1", c("Concept 2", "Wheezing", "Gibberish"), "Concept 4")))
  } else if (startsWith(prompt, "Phenotypes:")) {
    list(concepts = tibble(category = "Hypernym", terms = list("Concept 3")))
  } else {
    # Selecting the best vector search match
    tibble(term = "Wheezing", conceptId = 3L)
  }
}

mockVectorSearch <- function(term, domains, conceptClasses = NULL, limit = 10, ...) {
  if (term == "Wheezing") {
    tibble(term = term, recordCount = 100, conceptId = 3, conceptName = "Concept 3",
                   domainId = "Condition", conceptClassId = "Clinical Finding", vocabularyId = "TEST")
  } else {
    tibble(recordCount = numeric(), conceptId = numeric(), conceptName = character(),
                   domainId = character(), conceptClassId = character(), vocabularyId = character())
  }
}

test_that("generateConceptSetTargets maps terms by verbatim match, LLM-selected vector match, or not at all", {
  local_mocked_bindings(vectorSearch = mockVectorSearch, .package = "Pegasus")
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  client <- createMockLlmClient(targetsHandler)
  tracker <- Pegasus::createCostTracker()
  phenotypes <- data.frame(phenotypeId = 1, phenotypeName = "Asthma", clinicalDefinition = "def a")

  targets <- suppressMessages(Pegasus::generateConceptSetTargets(phenotypes,
                                                                 databaseReference = databaseReference,
                                                                 llmClient = client,
                                                                 costTracker = tracker))
  disease <- targets |> dplyr::filter(.data$pertains == "Disease of interest")
  alternatives <- targets |> dplyr::filter(.data$pertains == "Alternative diagnoses")

  matchOf <- function(term) disease$match[disease$term == term]
  expect_equal(matchOf("Concept 1"), "Verbatim")
  expect_equal(matchOf("Concept 2"), "Verbatim")
  expect_equal(matchOf("Wheezing"), "Vector")
  expect_equal(matchOf("Gibberish"), "None")
  expect_equal(disease$conceptId[disease$term == "Wheezing"], 3)
  expect_equal(disease$category[disease$term == "Concept 1"], "Hypernym")
  expect_true(all(targets$phenotypeId == 1))

  expect_equal(alternatives$term, "Concept 3")
  expect_equal(alternatives$conceptId, 3)
  expect_gt(tracker$amount, 0)
})

test_that("generateConceptSetTargets skips phenotypes that already have targets and re-uses cached results", {
  local_mocked_bindings(vectorSearch = mockVectorSearch, .package = "Pegasus")
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  pegasusConnection <- createTestPegasusConnection()
  phenotypes <- data.frame(phenotypeId = 1:2,
                           phenotypeName = c("Asthma", "Copd"),
                           clinicalDefinition = c("def a", "def c"))
  Pegasus::uploadPhenotypes(phenotypes, pegasusConnection = pegasusConnection, pegasusDatabaseSchema = testSchema)
  DatabaseConnector::executeSql(
    pegasusConnection,
    "INSERT INTO main.draft_concept_set_target (phenotype_id, pertains, category, term, match_type)
     VALUES (1, 'Disease of interest', 'Hypernym', 'asthma', 'Verbatim');"
  )
  client <- createMockLlmClient(targetsHandler)
  cacheFolder <- withr::local_tempdir()
  generate <- function() {
    suppressMessages(Pegasus::generateConceptSetTargets(phenotypes,
                                                        databaseReference = databaseReference,
                                                        llmClient = client,
                                                        cacheFolder = cacheFolder,
                                                        pegasusConnection = pegasusConnection,
                                                        pegasusDatabaseSchema = testSchema))
  }
  targets <- generate()
  expect_equal(unique(targets$phenotypeId), 2)
  nCalls <- length(client$calls)
  expect_gt(nCalls, 0)

  generate()
  expect_length(client$calls, nCalls)
})

test_that("generateConceptSetTargets returns an empty result when every phenotype already has targets", {
  pegasusConnection <- createTestPegasusConnection()
  DatabaseConnector::executeSql(
    pegasusConnection,
    "INSERT INTO main.draft_concept_set_target (phenotype_id, pertains, category, term, match_type)
     VALUES (1, 'Disease of interest', 'Hypernym', 'asthma', 'Verbatim');"
  )
  client <- createMockLlmClient(function(prompt, systemPrompt, type) stop("the LLM was called"))
  targets <- suppressMessages(Pegasus::generateConceptSetTargets(
    data.frame(phenotypeId = 1, phenotypeName = "Asthma", clinicalDefinition = "def a"),
    databaseReference = NULL,
    llmClient = client,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = testSchema
  ))
  expect_equal(nrow(targets), 0)
  expect_length(client$calls, 0)
})

createTargetsForDefinitions <- function() {
  data.frame(
    phenotypeId = 1,
    pertains = "Disease of interest",
    category = c("Hypernym", "Clinical presentation", "Clinical presentation"),
    term = c("Concept 1", "Concept 2", "Concept 3"),
    conceptId = 1:3,
    conceptName = paste("Concept", 1:3),
    domainId = "Condition",
    conceptClassId = "Clinical Finding",
    vocabularyId = "TEST",
    match = "Verbatim"
  )
}

test_that("generateNewDefinitions assigns one definition to all contexts when the LLM says 'all'", {
  contexts <- tibble(phenotypeId = 1:2,
                             phenotypeName = c("Asthma", "Copd"),
                             conceptId = 2,
                             conceptName = "Concept 2",
                             pertains = "Disease of interest",
                             category = "Clinical presentation",
                             context = c("Clinical presentation of Asthma", "Clinical presentation of Copd"))
  client <- createMockLlmClient(function(prompt, systemPrompt, type) {
    tibble(label = "Wheezing", definition = "Wheezing of any kind", coveredContexts = list("all"))
  })
  result <- Pegasus:::generateNewDefinitions(contexts, llmClient = client, costTracker = Pegasus::createCostTracker())
  expect_equal(nrow(result), 2)
  expect_equal(unique(result$conceptSetName), "Wheezing")
  expect_equal(unique(result$clinicalDefinition), "Wheezing of any kind")
  expect_true(all(is.na(result$conceptSetId)))
  expect_match(client$calls[[1]]$prompt, "Concept 2")
  expect_match(client$calls[[1]]$prompt, "Clinical presentation of Copd")
})

test_that("generateNewDefinitions splits contexts across definitions", {
  contexts <- tibble(phenotypeId = 1:2,
                             phenotypeName = c("Asthma", "Copd"),
                             conceptId = 2,
                             conceptName = "Concept 2",
                             pertains = "Disease of interest",
                             category = "Clinical presentation",
                             context = c("Clinical presentation of Asthma", "Clinical presentation of Copd"))
  client <- createMockLlmClient(function(prompt, systemPrompt, type) {
    tibble(label = c("Acute", "Chronic"),
                   definition = c("Acute def", "Chronic def"),
                   coveredContexts = list("Clinical presentation of Asthma", "Clinical presentation of Copd"))
  })
  result <- Pegasus:::generateNewDefinitions(contexts, llmClient = client, costTracker = Pegasus::createCostTracker())
  expect_equal(result$conceptSetName[result$phenotypeName == "Asthma"], "Acute")
  expect_equal(result$conceptSetName[result$phenotypeName == "Copd"], "Chronic")
})

test_that("existing definitions are re-used when the LLM selects them, and only the rest is generated", {
  contexts <- tibble(phenotypeId = 1:2,
                             phenotypeName = c("Asthma", "Copd"),
                             conceptId = 2,
                             conceptName = "Concept 2",
                             pertains = "Disease of interest",
                             category = "Clinical presentation",
                             context = c("Clinical presentation of Asthma", "Clinical presentation of Copd"))
  existing <- list(
    draftTargetToConceptSet = tibble(conceptId = 2, conceptSetId = 7),
    conceptSets = tibble(conceptSetId = 7, conceptId = 2, conceptSetName = "Acute", clinicalDefinition = "Acute def")
  )
  client <- createMockLlmClient(function(prompt, systemPrompt, type) {
    if (grepl("Acute def", prompt)) {
      tibble(label = "Acute", coveredContexts = list("Clinical presentation of Asthma"))
    } else {
      tibble(label = "Chronic", definition = "Chronic def", coveredContexts = list("all"))
    }
  })
  result <- Pegasus:::fetchOrGenerateDefinitionsSingleTarget(contexts,
                                                             existingDefinitionData = existing,
                                                             llmClient = client,
                                                             costTracker = Pegasus::createCostTracker())
  expect_equal(result$conceptSetId[result$phenotypeName == "Asthma"], 7)
  expect_equal(result$conceptSetName[result$phenotypeName == "Asthma"], "Acute")
  expect_true(is.na(result$conceptSetId[result$phenotypeName == "Copd"]))
  expect_equal(result$conceptSetName[result$phenotypeName == "Copd"], "Chronic")
  expect_length(client$calls, 2)
})

test_that("generateOrReuseConceptSetTargetDefinitions assigns a source type to every kind of target", {
  phenotypes <- data.frame(phenotypeId = 1,
                           phenotypeName = "Asthma",
                           clinicalDefinition = "def a")
  client <- createMockLlmClient(function(prompt, systemPrompt, type) {
    tibble(label = "Wheezing", definition = "Wheezing of any kind", coveredContexts = list("all"))
  })
  result <- suppressMessages(Pegasus::generateOrReuseConceptSetTargetDefinitions(
    conceptSetTargets = createTargetsForDefinitions(),
    phenotypes = phenotypes,
    minPhenotypeFraction = 0,
    llmClient = client
  ))
  sourceTypeOf <- function(category, conceptId = NA) {
    rows <- result |> dplyr::filter(.data$category == !!category)
    if (!is.na(conceptId)) rows <- rows |> dplyr::filter(.data$conceptId == !!conceptId)
    rows$sourceType
  }
  expect_equal(sourceTypeOf("Disease of interest"), "From human definition")
  expect_equal(sourceTypeOf("Hypernym"), "Concept")
  expect_true(all(sourceTypeOf("Clinical presentation") == "From AI definition"))
  expect_true(all(result$conceptSetName[result$sourceType == "From AI definition"] == "Wheezing"))
})

test_that("rarely used targets become simple concept + descendants sets without calling the LLM", {
  phenotypes <- data.frame(phenotypeId = 1:2,
                           phenotypeName = c("Asthma", "Copd"),
                           clinicalDefinition = c("def a", "def c"))
  targets <- createTargetsForDefinitions()
  targets <- dplyr::bind_rows(targets, targets |> dplyr::filter(.data$conceptId == 2) |> dplyr::mutate(phenotypeId = 2))
  client <- createMockLlmClient(function(prompt, systemPrompt, type) {
    tibble(label = "Wheezing", definition = "Wheezing of any kind", coveredContexts = list("all"))
  })
  result <- suppressMessages(Pegasus::generateOrReuseConceptSetTargetDefinitions(
    conceptSetTargets = targets,
    phenotypes = phenotypes,
    minPhenotypeFraction = 1,
    llmClient = client
  ))
  # Concept 2 is used by both phenotypes and needs a definition; concept 3 is used by one and stays simple
  expect_equal(unique(result$sourceType[result$conceptId %in% 3 & result$category == "Clinical presentation"]),
               "Concept + descendants")
  expect_equal(unique(result$sourceType[result$conceptId %in% 2 & result$category == "Clinical presentation"]),
               "From AI definition")
})

createDefinitionsForConceptSets <- function() {
  data.frame(
    phenotypeId = 1,
    pertains = "Disease of interest",
    category = c("Hypernym", "Drug", "Disease of interest", "Disease of interest"),
    conceptId = c(1, 2, NA, 3),
    conceptName = c("Concept 1", "Concept 2", NA, "Concept 3"),
    conceptSetId = NA_real_,
    conceptSetName = c("Concept 1", "Concept 2", "Asthma", "Wheezing"),
    clinicalDefinition = c(NA, NA, "def a", "Wheezing of any kind"),
    sourceType = c("Concept", "Concept + descendants", "From human definition", "From AI definition")
  )
}

test_that("generateConceptSets builds simple concept sets from the CDM vocabulary", {
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  definitions <- createDefinitionsForConceptSets() |>
    dplyr::filter(.data$sourceType %in% c("Concept", "Concept + descendants"))
  client <- createMockLlmClient(function(prompt, systemPrompt, type) stop("the LLM was called"))
  conceptSets <- suppressMessages(Pegasus::generateConceptSets(definitions,
                                                               llmClient = client,
                                                               databaseReference = databaseReference,
                                                               maxCores = 1))
  expect_equal(nrow(conceptSets), 2)
  expect_equal(conceptSets$hasDescendants[conceptSets$conceptSetName == "Concept 1"], 0)
  expect_equal(conceptSets$hasDescendants[conceptSets$conceptSetName == "Concept 2"], 1)
  expect_match(conceptSets$caprConceptSetExpression[conceptSets$conceptSetName == "Concept 2"], "descendants\\(2\\)")

  # The generated expressions identify the right concepts
  expression <- jsonlite::fromJSON(conceptSets$circeConceptSetExpression[conceptSets$conceptSetName == "Concept 1"])
  expect_equal(expression$items$concept$CONCEPT_ID, 1)
  expect_false(expression$items$includeDescendants)
})

test_that("generateConceptSets leaves targets that already have a concept set untouched", {
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  definitions <- createDefinitionsForConceptSets() |>
    dplyr::filter(.data$sourceType == "Concept") |>
    dplyr::mutate(conceptSetId = 5)
  conceptSets <- suppressMessages(Pegasus::generateConceptSets(definitions,
                                                               llmClient = createMockLlmClient(),
                                                               databaseReference = databaseReference,
                                                               maxCores = 1))
  expect_equal(conceptSets$conceptSetId, 5)
  expect_false("circeConceptSetExpression" %in% colnames(conceptSets))
})

test_that("full concept sets come from Phenelope, with a Capr translation of the result", {
  local_mocked_bindings(
    createConceptSet = function(name, clinicalDefinition, ...) {
      createCirceConceptSet(list(1, FALSE, TRUE), list(3, TRUE, FALSE))
    },
    .package = "Phenelope"
  )
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  definitions <- createDefinitionsForConceptSets() |>
    dplyr::filter(.data$conceptSetName == "Asthma")
  conceptSets <- suppressMessages(Pegasus::generateConceptSets(definitions,
                                                               llmClient = createMockLlmClient(),
                                                               databaseReference = databaseReference,
                                                               maxCores = 1))
  expect_equal(nrow(conceptSets), 1)
  expect_equal(conceptSets$hasDescendants, 1)
  expect_match(conceptSets$caprConceptSetExpression, "descendants\\(1\\)")
  expect_match(conceptSets$caprConceptSetExpression, "Asthma")
})

test_that("a full concept set without approved concepts is left empty with a warning", {
  local_mocked_bindings(
    createConceptSet = function(...) stop("No approved concepts: createDrugConceptSet failed"),
    .package = "Phenelope"
  )
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  definitions <- createDefinitionsForConceptSets() |>
    dplyr::filter(.data$conceptSetName == "Asthma")
  expect_warning(
    conceptSets <- suppressMessages(Pegasus::generateConceptSets(definitions,
                                                                 llmClient = createMockLlmClient(),
                                                                 databaseReference = databaseReference,
                                                                 maxCores = 1)),
    "leaving empty"
  )
  expect_equal(conceptSets$circeConceptSetExpression, "")
  expect_equal(conceptSets$caprConceptSetExpression, "")
})

test_that("unexpected Phenelope errors are not swallowed", {
  local_mocked_bindings(createConceptSet = function(...) stop("kaboom"), .package = "Phenelope")
  databaseReference <- createTestDatabaseReference(createTestCdmConnectionDetails())
  definitions <- createDefinitionsForConceptSets() |>
    dplyr::filter(.data$conceptSetName == "Asthma")
  expect_error(
    suppressMessages(Pegasus::generateConceptSets(definitions,
                                                  llmClient = createMockLlmClient(),
                                                  databaseReference = databaseReference,
                                                  maxCores = 1)),
    "kaboom"
  )
})
