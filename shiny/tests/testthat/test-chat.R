test_that("loadSystemPrompt strips front matter and adds the tool override", {
  prompt <- loadSystemPrompt("../../skills/ClinicalDefinitionRefiner.md")
  expect_false(grepl("^---", prompt))
  expect_false(grepl("name: condition-definition-refiner", prompt, fixed = TRUE))
  expect_true(grepl("submitDefinition", prompt, fixed = TRUE))
})

test_that("buildInitialPrompt mirrors refineDefinition", {
  expect_equal(buildInitialPrompt("Asthma"), "Asthma")
  expect_equal(buildInitialPrompt("Asthma", "  "), "Asthma")
  expect_equal(buildInitialPrompt("Asthma", "Airway disease"), "Name: Asthma\n\nInitial definition: Airway disease\n")
})

test_that("extractFinalDefinition parses the final heading", {
  text <- "Summary of constraints...\n\n### Final Condition Definition\n\nAsthma is a chronic disease.\n\nI have saved it."
  expect_equal(extractFinalDefinition(text), "Asthma is a chronic disease.")
  expect_null(extractFinalDefinition("Which option do you prefer?"))
  expect_null(extractFinalDefinition(NULL))
})
