test_that("queryLlm returns the chat response and sets the system prompt", {
  client <- createMockLlmClient(function(prompt, systemPrompt, type) paste("echo:", prompt))
  expect_equal(Pegasus:::queryLlm("hello", systemPrompt = "be brief", llmClient = client), "echo: hello")
  expect_equal(client$systemPrompt, "be brief")
  expect_null(client$calls[[1]]$type)
})

test_that("queryLlm requests structured output when an output type is given", {
  client <- createMockLlmClient(function(prompt, systemPrompt, type) list(answer = 42))
  type <- ellmer::type_object(answer = ellmer::type_integer())
  expect_equal(Pegasus:::queryLlm("q", llmClient = client, outputType = type)$answer, 42)
  expect_false(is.null(client$calls[[1]]$type))
})

test_that("queryLlm adds the cost of the call to the cost tracker", {
  client <- createMockLlmClient()
  tracker <- Pegasus::createCostTracker()
  Pegasus:::queryLlm("q", llmClient = client, costTracker = tracker)
  expect_gt(tracker$amount, 0)
})

test_that("queryLlm retries after a transient failure", {
  attempts <- 0
  client <- createMockLlmClient(function(prompt, systemPrompt, type) {
    attempts <<- attempts + 1
    if (attempts < 2) stop("temporary glitch")
    "recovered"
  })
  expect_equal(suppressMessages(Pegasus:::queryLlm("q", llmClient = client)), "recovered")
  expect_equal(attempts, 2)
})

test_that("queryLlm gives up after repeated failures", {
  client <- createMockLlmClient(function(prompt, systemPrompt, type) stop("always broken"))
  expect_error(suppressMessages(Pegasus:::queryLlm("q", llmClient = client)), "always broken")
  expect_length(client$calls, 3)
})

test_that("queryLlm reports content filter failures with a dedicated condition class", {
  client <- createMockLlmClient(function(prompt, systemPrompt, type) stop("blocked by content filter"))
  expect_error(suppressMessages(Pegasus:::queryLlm("q", llmClient = client)), class = "ContentFilterError")
})

test_that("queryLlm stops immediately when the run is aborted", {
  client <- createMockLlmClient(function(prompt, systemPrompt, type) stop("Abort requested"))
  expect_error(suppressMessages(capture.output(Pegasus:::queryLlm("q", llmClient = client))), "stopped")
  expect_length(client$calls, 1)
})

test_that("generatePhenotypeMetaData converts the LLM output to one tidy row per phenotype", {
  response <- list(
    condition_name = "Asthma",
    clinical_rationale = "because",
    is_pharmacovigilance_outcome = "NO",
    clinical_course = "TRANSIENT_RECURRENT",
    expected_care_setting = "OUTPATIENT_EXPECTED",
    sex_constraint = "BOTH_SEXES",
    min_age_years = 0L,
    max_age_years = NULL,
    minimum_interepisode_gap_days = 30L,
    recommended_cohort_exit = "END_OF_CONTINUOUS_OBSERVATION",
    recommended_exit_days = NULL,
    has_discrete_recorded_symptoms = TRUE,
    requires_diagnostic_test_or_procedure = FALSE,
    requires_active_treatment_within_30d = TRUE,
    expects_condition_specific_followup_or_sequelae_1yr = FALSE,
    confidence = "HIGH"
  )
  client <- createMockLlmClient(function(prompt, systemPrompt, type) response)
  tracker <- Pegasus::createCostTracker()
  phenotypes <- data.frame(phenotypeId = 1:2,
                           phenotypeName = c("Asthma", "Copd"),
                           clinicalDefinition = c("def a", "def c"))
  metaData <- suppressMessages(Pegasus::generatePhenotypeMetaData(phenotypes, llmClient = client, costTracker = tracker))

  expect_equal(nrow(metaData), 2)
  expect_equal(metaData$phenotypeName, c("Asthma", "Copd"))
  expect_equal(metaData$phenotypeId, 1:2)
  expect_equal(metaData$hasDiscreteRecordedSymptoms, c(1, 1))
  expect_equal(metaData$requiresDiagnosticTestOrProcedure, c(0, 0))
  expect_true(all(is.na(metaData$maxAgeYears)))
  expect_match(client$calls[[2]]$prompt, "Copd")
  expect_match(client$calls[[2]]$prompt, "def c")
  expect_gt(tracker$amount, 0)

  # The result can be uploaded as is
  connection <- createTestPegasusConnection()
  Pegasus::uploadPhenotypes(phenotypes, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  Pegasus::uploadPhenotypeMetaData(metaData,
                                   pegasusConnection = connection,
                                   pegasusDatabaseSchema = testSchema)
  expect_equal(nrow(Pegasus::downloadPhenotypeMetaData(pegasusConnection = connection,
                                                       pegasusDatabaseSchema = testSchema)), 2)
})

test_that("generatePhenotypeMetaData skips phenotypes that already have meta data", {
  connection <- createTestPegasusConnection()
  phenotypes <- data.frame(phenotypeId = 1:2,
                           phenotypeName = c("Asthma", "Copd"),
                           clinicalDefinition = c("def a", "def c"))
  Pegasus::uploadPhenotypes(phenotypes, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  DatabaseConnector::executeSql(connection, "INSERT INTO main.phenotype_meta_data (phenotype_id) VALUES (1);")
  client <- createMockLlmClient(function(prompt, systemPrompt, type) stop("the LLM was called"))
  metaData <- suppressMessages(Pegasus::generatePhenotypeMetaData(phenotypes[1, ],
                                                                  llmClient = client,
                                                                  pegasusConnection = connection,
                                                                  pegasusDatabaseSchema = testSchema))
  expect_equal(nrow(metaData), 0)
  expect_length(client$calls, 0)
})

test_that("registerFileWriteTool registers a tool that writes the definition to file", {
  client <- createMockLlmClient()
  Pegasus:::registerFileWriteTool(client)
  expect_length(client$tools, 1)
  file <- withr::local_tempfile(fileext = ".txt")
  result <- client$tools[[1]](definition = "The final definition", fileName = file)
  expect_match(result, "Successfully saved")
  expect_equal(readLines(file), "The final definition")
})

test_that("refineDefinition starts the conversation with the phenotype name and optional initial definition", {
  local_mocked_bindings(live_console = function(...) invisible(NULL), .package = "ellmer")
  client <- createMockLlmClient()
  Pegasus::refineDefinition("Asthma", llmClient = client)
  expect_equal(client$calls[[1]]$prompt, "Asthma")
  expect_false(grepl("^---", client$systemPrompt))
  expect_gt(nchar(client$systemPrompt), 0)
  expect_length(client$tools, 1)

  client <- createMockLlmClient()
  Pegasus::refineDefinition("Asthma", clinicalDefinition = "Wheezing", llmClient = client)
  expect_match(client$calls[[1]]$prompt, "Asthma")
  expect_match(client$calls[[1]]$prompt, "Wheezing")
})
