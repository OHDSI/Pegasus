# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Pegasus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# source("R/HelperFunctions.R")
# library(dplyr)

#' Generate meta data for phenotypes
#'
#' @template Phenotypes
#' @template LlmClient
#' @template CostTracker
#'
#' @returns
#' A tibble with one row.
#'
#' @export
generatePhenotypeMetaData <- function(phenotypes,
                                      llmClient,
                                      costTracker = createCostTracker()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  startCost <- costTracker$amount

  # Generate targets for the phenotype itself
  promptFile <- system.file("prompts", "GeneratePhenotypeMetaData.txt", package = "Pegasus")
  systemPrompt <- paste(readLines(promptFile), collapse = "\n")

  nullableInteger <- ellmer::type_from_schema(text = "{\"type\": [\"integer\", \"null\"]}")

  outputType <- ellmer::type_object(
    condition_name = ellmer::type_string(),
    clinical_rationale = ellmer::type_string(),
    is_pharmacovigilance_outcome = ellmer::type_enum(values = c("YES", "NO", "CONDITIONAL")),
    clinical_course = ellmer::type_enum(values = c("PERSISTENT_STABLE", "PERSISTENT_RELAPSING", "TRANSIENT_RECURRENT", "TRANSIENT_SINGLE")),
    expected_care_setting = ellmer::type_enum(values = c("ACUTE_CARE_EXPECTED", "ACUTE_CARE_COMMON", "OUTPATIENT_EXPECTED")),
    sex_constraint = ellmer::type_enum(values = c("BOTH_SEXES", "FEMALE_ONLY", "MALE_ONLY")),
    min_age_years = ellmer::type_integer(),
    max_age_years = nullableInteger,
    minimum_interepisode_gap_days = ellmer::type_integer(),
    recommended_cohort_exit = ellmer::type_enum(values = c("END_OF_CONTINUOUS_OBSERVATION", "FIXED_WINDOW_FROM_INDEX", "END_OF_ERA_WITH_PERSISTENCE_GAP")),
    recommended_exit_days = nullableInteger,
    has_discrete_recorded_symptoms = ellmer::type_boolean(),
    requires_diagnostic_test_or_procedure = ellmer::type_boolean(),
    requires_active_treatment_within_30d = ellmer::type_boolean(),
    expects_condition_specific_followup_or_sequelae_1yr = ellmer::type_boolean(),
    confidence = ellmer::type_enum(values = c("HIGH", "MODERATE", "LOW"))
  )
  metaData <- list()
  for (i in seq_len(nrow(phenotypes))) {
    message("Creating meta data for ", phenotypes$phenotypeName[i])
    prompt <- sprintf("Phenotype: %s\nClinical definition: %s\n",
                      phenotypes$phenotypeName[i],
                      phenotypes$clinicalDefinition[i])
    response <- queryLlm(
      prompt = prompt,
      systemPrompt = systemPrompt,
      llmClient = llmClient,
      costTracker = costTracker,
      outputType = outputType
    )
    metaData[[i]] <- as_tibble(lapply(response, function(x) if (is.null(x)) NA_integer_ else x)) |>
      SqlRender::snakeCaseToCamelCaseNames() |>
      mutate(phenotypeName = phenotypes$phenotypeName[i],
             hasDiscreteRecordedSymptoms = as.integer(.data$hasDiscreteRecordedSymptoms),
             requiresDiagnosticTestOrProcedure = as.integer(.data$requiresDiagnosticTestOrProcedure),
             requiresActiveTreatmentWithin30d = as.integer(.data$requiresActiveTreatmentWithin30d),
             expectsConditionSpecificFollowupOrSequelae1yr = as.integer(.data$expectsConditionSpecificFollowupOrSequelae1yr))
  }
  metaData <- bind_rows(metaData)

  delta <- Sys.time() - startTime
  message(sprintf("Generating phenotype meta data took %0.2f %s and cost $%0.2f.",
                  delta,
                  attr(delta, "units"),
                  costTracker$amount - startCost))
  return(metaData)
}
