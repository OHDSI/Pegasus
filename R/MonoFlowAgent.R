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


#' Create cohort definitions using the MonoFlow agent
#'
#' @description
#' Uses an AI agent to generate cohort definitions for phenotypes. The agent is simply provided a set of tool, and runs
#' in an otherwise unrestricted single flow.
#'
#' This requires that the concept sets have already been generated and uploaded to the Pegasus database, and that the
#'  `createKeeperReferenceCohorts()` function has been executed with `uploadKeeperProfiles = TRUE`.
#'
#' @template Phenotypes
#' @template LlmClient
#' @template CostTracker
#' @template PegasusConnectionDetails
#' @param databaseReferences A list of 1 or more objects of type `DatabaseReference` as created by the
#'                           `createDatabaseReference()` funnction. These should have the  `workDatabaseSchema`,
#'                           `workCohortTable`, and `workConceptSetTable` properties defined.
#' @param workFolder A folder where data objects are stored.
#'
#' @returns
#' A tibble with one row per phenotype.
#'
#' @export
generateCohortDefinitionsUsingMonoFlowAgent <- function(phenotypes,
                                                        llmClient,
                                                        costTracker = createCostTracker(),
                                                        pegasusConnectionDetails,
                                                        pegasusDatabaseSchema,
                                                        databaseReferences,
                                                        workFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(databaseReferences, min.len = 1, add = errorMessages)
  for (databaseReference in databaseReferences) {
    checkmate::assertClass(databaseReference, "DatabaseReference", add = errorMessages)
    checkmate::assertNames(names(databaseReference), must.include = c(
      "workDatabaseSchema",
      "workCohortTable",
      "workConceptSetTable"
    ), add = errorMessages)
  }
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  startCost <- costTracker$amount

  dir.create(workFolder)
  toolContext <- PegasusAgentContext$new(pegasusConnectionDetails = pegasusConnectionDetails,
                                         pegasusDatabaseSchema = pegasusDatabaseSchema,
                                         databaseReferences = databaseReferences)
  on.exit(toolContext$close())
  systemPromptFile <- system.file("prompts", "MonoFlowSystemPrompt.txt", package = "Pegasus")
  systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")

  monoFlowAgent <- llmClient$clone()
  monoFlowAgent$register_tools(createAgentTools(toolContext))
  monoFlowAgent$set_system_prompt(systemPrompt)

  cohortDefinitions <- list()
  for (i in seq_len(nrow(phenotypes))) {
    phenotype <- phenotypes[i, ]
    outputFolder <- file.path(workFolder, gsub("[^[:alnum:]]+", "_", phenotype$phenotypeName))
    bestJsonFile <- file.path(outputFolder, "best_cohort.json")
    if (!file.exists(bestJsonFile)) {
      message("Creating cohort defintion for ", phenotype$phenotypeName)
      dir.create(outputFolder)

      context <- constructContext(phenotype)

      # Run chat
      monoFlowAgent$set_turns(list())
      response <- monoFlowAgent$chat(context)

      # Save full chat
      turns <- monoFlowAgent$get_turns()
      texts <- list()
      for (t in 2:length(turns)) {
        texts[[t-1]] <- format(turns[[t]])
      }
      texts <- paste(texts, collapse = "\n\n")
      writeLines(texts, file.path(outputFolder, "chat.txt"))

      # Get best cohort definition
      prompt <- "
        Please return your best definition capr code.
        Output using the following structure. Do not include text outside the JSON:
        { \"capr\": \"cohort(...)\"}
      "
      bestCohortDefinition <- monoFlowAgent$chat_structured(prompt, type = type_object(capr = type_string()))
      json <- convertCaprToJson(toolContext, bestCohortDefinition$capr)
      writeLines(json, bestJsonFile)

      # Save cost
      cost <- monoFlowAgent$get_cost("all")
      writeLines(sprintf("Total cost: $%0.2f", cost), file.path(outputFolder, "cost.txt"))
      costTracker$amount <- costTracker$amount + cost
    } else {
      json <- paste(readLines(bestJsonFile), collapse = "\n")
    }
    cohortDefinitions[[i]] <- tibble(
      phenotypeName = phenotype$phenotypeName,
      cohortDefinitionName = sprintf("[MonoFlow] %s (AI-generated)", phenotype$phenotypeName),
      circeCohortExpression = json,
      caprCohortExpression = circeToCaprCohortDefinition(json),
      cdSourceType = "MonoFlow"
    )
  }
  cohortDefinitions <- bind_rows(cohortDefinitions)
  delta <- Sys.time() - startTime
  message(sprintf("Generating concept sets took %0.2f %s and cost $%0.2f.",
                  delta,
                  attr(delta, "units"),
                  costTracker$amount - startCost))
  return(cohortDefinitions)
}

constructContext <- function(phenotype) {
  skillFile <- system.file("skills", "CohortDeveloper.md", package = "Pegasus")
  skill <- readLines(skillFile)
  endOfHeader <- grep("---", skill)[2]
  skill <- paste(skill[(endOfHeader + 1) : length(skill)], collapse = "\n")

  caprReferenceFile <- system.file("skills", "CaprReference.md", package = "Pegasus")
  caprReference <- paste(readLines(caprReferenceFile), collapse = "\n")
  context <- paste(
    skill,
    "",
    "--- Start of CAPR_REFERENCE.md ---",
    caprReference,
    "--- End of CAPR_REFERENCE.md ---",
    "",
    sprintf("Phenotype: %s", phenotype$phenotypeName),
    "",
    "--- Start of clinical definition ---",
    phenotype$clinicalDefinition,
    "--- End of clinical definition ---",
    sep = "\n"
  )
  return(context)
}

#' Get standard concept sets
#'
#' @description
#' Gets a list of standard concepts from the Pegasus package. These are used by the MonoFlow agent.
#'
#' @returns
#' A tibble similar to `generateConceptSets()`.
#'
#' @export
getStandardConceptSets <- function() {
  fileName <- system.file("StandardConceptSets.csv", package = "Pegasus")
  conceptSets <- readr::read_csv(fileName, show_col_types = FALSE)
  conceptSets <- conceptSets |>
    mutate(phenotypeName = NA_character_,
           pertains = NA_character_,
           category = NA_character_,
           conceptId = NA_integer_,
           conceptName = NA_character_,
           conceptSetId = NA_integer_,
           hasDescendants = 1,
           clinicalDefinition = NA_character_)
  return(conceptSets)
}

ensureStandardConceptSets <- function(connection, databaseReference, pegasusConnection, pegasusDatabaseSchema) {
  conceptSets <- getStandardConceptSets()
  sql <- "
    SELECT concept_set_id,
      concept_set_name
    FROM @database_schema.concept_set;
  "
  existingConceptSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = pegasusConnection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  missingConceptSets <- conceptSets[!conceptSets$conceptSetName %in% existingConceptSets$conceptSetName, ]
  if (nrow(missingConceptSets) > 0) {
    message(sprintf("Missing standard concept sets '%s'. Adding (including person counts).",
                    paste(missingConceptSets$conceptSetName, collapse = "', '")))
    lastId <- max(existingConceptSets$conceptSetId)
    missingConceptSets <- missingConceptSets |>
      mutate(conceptSetId = row_number() + lastId)

    counts <- createConceptSetCounts(
      completeConceptSets = missingConceptSets,
      databaseReferences = list(databaseReference)
    )

    dataToUpload <- missingConceptSets |>
      select("conceptSetId",
             "conceptSetName",
             "circeConceptSetExpression",
             "caprConceptSetExpression",
             "hasDescendants",
             "clinicalDefinition",
             "conceptId",
             csSourceType = "sourceType") |>
      mutate(valid = 1)
    DatabaseConnector::insertTable(
      connection = pegasusConnection,
      databaseSchema = pegasusDatabaseSchema,
      tableName = "concept_set",
      data = dataToUpload,
      dropTableIfExists = FALSE,
      createTable = FALSE,
      tempTable = FALSE,
      camelCaseToSnakeCase = TRUE
    )
    DatabaseConnector::insertTable(
      connection = pegasusConnection,
      databaseSchema = pegasusDatabaseSchema,
      tableName = "concept_set_person_count",
      data = counts,
      dropTableIfExists = FALSE,
      createTable = FALSE,
      tempTable = FALSE,
      camelCaseToSnakeCase = TRUE
    )
  }
}
