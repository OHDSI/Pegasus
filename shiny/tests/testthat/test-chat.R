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

test_that("extractFinalDefinition tolerates variations of the final heading", {
  expect_equal(extractFinalDefinition("Done.\n\n## Final Condition Definition\n\nAsthma is chronic."), "Asthma is chronic.")
  expect_equal(extractFinalDefinition("Done.\n\n**Final Condition Definition**\n\nAsthma is chronic."), "Asthma is chronic.")
  expect_equal(extractFinalDefinition("Done.\n\n### Final Definition:\n\nAsthma is chronic.\n\nBye"), "Asthma is chronic.")
  expect_equal(extractFinalDefinition("### Final Condition Definition\n\nOld.\n\n### Final Condition Definition\n\nNew."), "New.")
  expect_null(extractFinalDefinition("The final definition is still open."))
})

test_that("llmModelLabel names the provider and model", {
  expect_equal(llmModelLabel(list(provider = "JNJ BEDROCK", model = "claude-sonnet")), "JNJ BEDROCK / claude-sonnet")
  expect_equal(llmModelLabel(list(provider = NULL, model = "o3")), "Azure OpenAI / o3")
  expect_equal(llmModelLabel(list(provider = "", model = "o3")), "Azure OpenAI / o3")
  expect_equal(llmModelLabel(list(model = "")), "Unknown model")
  expect_equal(llmModelLabel(NULL), "Unknown model")
})
