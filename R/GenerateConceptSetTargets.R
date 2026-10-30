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

targetOutputSchema <- ellmer::type_object(
  concepts = ellmer::type_array(
    ellmer::type_object(
      category = ellmer::type_enum(
        values = c(
          "Hypernym",
          "Risk factor",
          "Clinical presentation",
          "Diagnostic procedure",
          "Measurement",
          "Drug",
          "Treatment procedure",
          "Etiology",
          "Alternative diagnosis",
          "Progression complication"
        ),
        description = "Clinical category assigned to the concepts."
      ),
      terms = ellmer::type_array(
        ellmer::type_string(
          description = "Standardized clinical term from OHDSI vocabularies."
        ),
        description = "List of terms belonging to the category."
      )
    ),
    description = "Collection of phenotype-related concepts grouped by category."
  )
)

categoryRestrictions <- list(
  `Hypernym` = list(
    domains = c(
      "Condition"
    ),
    conceptClasses = c(
      "Clinical Finding",
      "Disorder"
    )
  ),

  `Risk factor` = list(
    domains = c(
      "Condition",
      "Observation",
      "Drug",
      "Procedure"),
    conceptClasses = c(
      "Clinical Finding",
      "Disorder",
      "Clinical Observation",
      "Ingredient",
      "Procedure"
    )
  ),

  `Clinical presentation` = list(
    domains = c(
      "Condition",
      "Observation"
    ),
    conceptClasses = c(
      "Clinical Finding",
      "Disorder"
    )
  ),

  `Diagnostic procedure` = list(
    domains = c(
      "Procedure",
      "Measurement"
    ),
    conceptClasses = c(
      "Procedure"
    )
  ),

  `Measurement` = list(
    domains = c(
      "Measurement"
    ),
    conceptClasses = c(
      "Clinical Observation",
      "Procedure",
      "Lab Test"
    )
  ),

  `Drug` = list(
    domains = c(
      "Drug"
    ),
    conceptClasses = c(
      "Ingredient"
    )
  ),

  `Treatment procedure` = list(
    domains = c(
      "Procedure"
    ),
    conceptClasses = c(
      "Procedure"
    )
  ),

  `Etiology` = list(
    domains = c(
      "Condition",
      "Observation",
      "Measurement"
    ),
    conceptClasses = c(
      "Disorder",
      "Clinical Finding",
      "Lab Test"
    )
  ),

  `Alternative diagnosis` = list(
    domains = c(
      "Condition",
      "Observation"),
    conceptClasses = c(
      "Clinical Finding",
      "Disorder"
    )
  ),

  `Progression complication` = list(
    domains = c(
      "Condition",
      "Observation"
    ),
    conceptClasses = c(
      "Clinical Finding",
      "Disorder"
    )
  )
)

mapToConcepts <- function(termsPerCategory,
                          connection,
                          vocabDatabaseSchema,
                          llmClient,
                          costTracker) {
  targets <- list()
  for (i in seq_len(nrow(termsPerCategory$concepts))) {
    category <- as.character(termsPerCategory$concepts$category[[i]])
    message("- Mapping terms to concepts for category ", category)

    terms <- termsPerCategory$concepts$terms[[i]]
    restrictions <- categoryRestrictions[[category]]

    # Look for verbatim matches
    sql <- "
      SELECT concept_id,
        concept_name,
        domain_id,
        concept_class_id,
        vocabulary_id
      FROM @vocab_database_schema.concept
      WHERE domain_id IN (@domain_ids)
        AND concept_class_id IN (@concept_classes)
        AND LOWER(concept_name) IN (@terms)
        AND standard_concept = 'S';
    "
    verbatimMatches <- DatabaseConnector::renderTranslateQuerySql(
      connection = connection,
      sql = sql,
      vocab_database_schema = vocabDatabaseSchema,
      domain_ids = paste0("'", paste0(restrictions$domains, collapse = "', '"), "'"),
      concept_classes = paste0("'", paste0(restrictions$conceptClasses, collapse = "', '"), "'"),
      terms = paste0("'", paste0(tolower(gsub("'", "", terms)), collapse = "', '"), "'"),
      snakeCaseToCamelCase = TRUE
    )
    verbatimMatches <- verbatimMatches |>
      distinct() |>
      mutate(lowercaseName = tolower(.data$conceptName)) |>
      inner_join(
        tibble(term = terms,
               lowercaseName = tolower(terms)),
        by = join_by("lowercaseName")
      ) |>
      select(-"lowercaseName") |>

      group_by(.data$term) |>
      slice_head(n = 1) |>
      ungroup() |>
      mutate(match = "Verbatim")


    # Do vector matching for remaining terms:
    remainingTerms <- terms[!terms %in% verbatimMatches$term]
    if (length(remainingTerms) > 0) {
      vectorMatches <- list()
      for (j in seq_along(remainingTerms)) {
        matches <- vectorSearch(
          term = remainingTerms[j],
          domains = restrictions$domains,
          conceptClasses = restrictions$conceptClasses,
          limit = 10
        )
        if (nrow(matches) > 0) {
          vectorMatches[[j]] <- matches |>
            filter(.data$recordCount > 0) |>
            mutate(term = remainingTerms[j]) |>
            select("term", "recordCount", "conceptId", "conceptName", "domainId", "conceptClassId", "vocabularyId")
        }
      }
      vectorMatches <- bind_rows(vectorMatches)

      # Prompt LLM for best matches:
      promptFile <- system.file("prompts", "MatchPrompt.txt", package = "Pegasus")
      systemPrompt <- paste(readLines(promptFile), collapse = "\n")

      prompt <- vectorMatches |>
        group_by(.data$term) |>
        summarise(
          candidates = list(
            purrr::pmap(
              list(
                conceptId = .data$conceptId,
                conceptName = .data$conceptName,
                recordCount = .data$recordCount
              ),
              list
            )
          ),
          .groups = "drop"
        ) |>
        jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

      bestVectorMatches <- queryLlm(
        prompt = prompt,
        systemPrompt = systemPrompt,
        llmClient = llmClient,
        costTracker = costTracker,
        outputType = ellmer::type_array(
          ellmer::type_object(term = ellmer::type_string(), conceptId = ellmer::type_integer())
        )
      )
      # There may be mismatches in Vocab versions, so get concept properties from provided Vocab:
      sql <- "
        SELECT concept_id,
          concept_name,
          domain_id,
          concept_class_id,
          vocabulary_id
        FROM @vocab_database_schema.concept
        WHERE concept_id IN (@concept_ids);
      "
      vectorMatchConcepts <- DatabaseConnector::renderTranslateQuerySql(
        connection = connection,
        sql = sql,
        vocab_database_schema = vocabDatabaseSchema,
        concept_ids = bestVectorMatches$conceptId,
        snakeCaseToCamelCase = TRUE
      )
      bestVectorMatches <- bestVectorMatches |>
        inner_join(vectorMatchConcepts, by = join_by("conceptId")) |>
        select("term", "conceptId", "conceptName", "domainId", "conceptClassId", "vocabularyId") |>
        mutate(match = "Vector")

      unmatchedTerms <- tibble(
        term = remainingTerms[!remainingTerms %in% bestVectorMatches$term],
        match = "None"
      )
      matches <- bind_rows(verbatimMatches, bestVectorMatches, unmatchedTerms)
    } else {
      matches <- verbatimMatches
    }
    targets[[i]] <- matches |>
      mutate(category = !!category) |>
      relocate(category, "term")
  }
  targets <- bind_rows(targets)
  return(targets)
}

#' Generate concept set targets for a phenotype
#'
#' @template PhenotypeName
#' @template ClinicalDefinition
#' @template Connection
#' @template VocabDatabaseSchema
#' @template LlmClient
#' @template CostTracker
#'
#' @description
#' Enumerates targets for concept sets for a phenotype. The targets themselves are mapped to concepts in the OHDSI
#' Vocabulary to increase the probability that two phenotypes share the same target.
#'
#' @returns
#' A tibble with concept set targets.
#'
#' @export
generateConceptSetTargets <- function(phenotypeName,
                                      clinicalDefinition,
                                      connection = NULL,
                                      connectionDetails = NULL,
                                      vocabDatabaseSchema,
                                      llmClient,
                                      costTracker = createCostTracker()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(phenotypeName, len = 1, add = errorMessages)
  checkmate::assertCharacter(clinicalDefinition, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(connection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(vocabDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(connectionDetails) && is.null(connection)) {
    stop("Must provide either connectionDetails or a connection.")
  }
  message("Creating targets for ", phenotypeName)

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # Generate targets for the phenotype itself
  promptFile <- system.file("prompts", "CreateConceptSetTargetsPrompt.txt", package = "Pegasus")
  systemPrompt <- paste(readLines(promptFile), collapse = "\n")
  prompt <- sprintf("Phenotype: %s\nClinical definition: %s\n",
                    phenotypeName,
                    clinicalDefinition)
  termsPerCategory <- queryLlm(
    prompt = prompt,
    systemPrompt = systemPrompt,
    llmClient = llmClient,
    costTracker = costTracker,
    outputType = targetOutputSchema
  )
  targets <- mapToConcepts(
    termsPerCategory = termsPerCategory,
    connection = connection,
    vocabDatabaseSchema = vocabDatabaseSchema,
    llmClient = llmClient,
    costTracker = costTracker)

  alternativeDiagnoses <- termsPerCategory$concepts |>
    filter(.data$category == "Alternative diagnosis") |>
    pull(.data$terms) |>
    unlist()

  # Generate targets for the alternative diagnoses
  promptFile <- system.file("prompts", "AlternativeDiagnosesTargetsPrompt.txt", package = "Pegasus")
  systemPrompt <- paste(readLines(promptFile), collapse = "\n")
  prompt <- paste(c("Phenotypes:", sprintf("- %s", alternativeDiagnoses)), collapse = "\n")
  termsPerCategoryAd <- queryLlm(
    prompt = prompt,
    systemPrompt = systemPrompt,
    llmClient = llmClient,
    outputType = targetOutputSchema
  )
  targetsAd <- mapToConcepts(
    termsPerCategory = termsPerCategoryAd,
    connection = connection,
    vocabDatabaseSchema = vocabDatabaseSchema,
    llmClient = llmClient,
    costTracker = costTracker)

  # Combine and return
  result <- bind_rows(
    targets |>
      mutate(phenotypeName = !!phenotypeName,
             pertains = "Disease of interest"),
    targetsAd |>
      mutate(phenotypeName = !!phenotypeName,
             pertains = "Alternative diagnoses")
  ) |>
    relocate("phenotypeName", "pertains")

  return(result)
}
