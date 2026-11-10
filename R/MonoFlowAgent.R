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
#' @template DatabaseReferences
#' @param workFolder A folder where data objects are stored.
#'
#' @returns
#' A tibble with one row per phenotype.
#'
#' @export
generateCohortDefinitionsUsingMonoFlowAgent <- function(phenotypes,
                                                        llmClient,
                                                        costTracker = createCostTracker(),
                                                        pegasusConnection = NULL,
                                                        pegasusConnectionDetails = NULL,
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
  checkmate::assertClass(pegasusConnection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
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
  if (is.null(pegasusConnection) && is.null(pegasusConnectionDetails)) {
    stop("Must provide either pegasusConnection or pegasusConnectionDetails")
  }
  if (is.null(pegasusConnection)) {
    pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(pegasusConnection))
  }

  checkRequirementsMet(phenotypes,
                       llmClient,
                       costTracker,
                       pegasusConnection,
                       pegasusDatabaseSchema,
                       databaseReferences)

  startTime <- Sys.time()
  startCost <- costTracker$amount

  dir.create(workFolder)
  toolContext <- PegasusAgentContext$new(pegasusConnection = pegasusConnection,
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
      message("Creating cohort definition for ", phenotype$phenotypeName)
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

checkRequirementsMet <- function(phenotypes,
                                 llmClient,
                                 costTracker,
                                 pegasusConnection,
                                 pegasusDatabaseSchema,
                                 databaseReferences) {

  message("- Checking if phenotypes have concepts sets")
  phenotypeToConceptSets <- downloadPhenotypeToConceptSets(phenotypes = phenotypes,
                                                           pegasusConnection = pegasusConnection,
                                                           pegasusDatabaseSchema = pegasusDatabaseSchema)
  phenotypeNames <- unique(phenotypeToConceptSets$phenotypeName)
  if (length(phenotypeNames) != nrow(phenotypes)) {
    stop(sprintf("No concept sets found for phenotype(s) '%s'",
                 paste(phenotypes$phenotypeName[!phenotypes$phenotypeName %in% phenotypeNames], collapse = "', '")))
  }

  message(" - Ensuring standard concept sets exist in Pegasus schema")
  standardConceptSets <- getStandardConceptSets()
  sql <- "
    SELECT concept_set_id,
      concept_set_name,
      circe_concept_set_expression
    FROM @database_schema.concept_set;
  "
  existingConceptSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = pegasusConnection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE
  )
  existingStandardConceptSets <- existingConceptSets |>
    filter(.data$conceptSetName %in% standardConceptSets$conceptSetName)
  missingStandardConceptSets <- standardConceptSets |>
    filter(!.data$conceptSetName %in% existingStandardConceptSets$conceptSetName)
  if (nrow(missingStandardConceptSets) > 0) {
    message(sprintf("Missing standard concept sets '%s'. Adding.",
                    paste(missingStandardConceptSets$conceptSetName, collapse = "', '")))
    lastId <- max(existingConceptSets$conceptSetId)
    missingStandardConceptSets <- missingStandardConceptSets |>
      mutate(conceptSetId = row_number() + lastId)

    dataToUpload <- missingStandardConceptSets |>
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
    standardConceptSets <- bind_rows(
      existingStandardConceptSets,
      missingStandardConceptSets
    )
  } else {
    standardConceptSets <- existingStandardConceptSets
  }

  message(" - Ensuring standard concept sets have counts")
  conceptSetPersonCounts <- createConceptSetCounts(completeConceptSets = standardConceptSets,
                                                   pegasusConnection = pegasusConnection,
                                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                                   databaseReferences = databaseReferences,
                                                   maxCores = 1)
  if  (nrow(conceptSetPersonCounts) > 0) {
    uploadConceptSetPersonCounts(
      conceptSetPersonCounts = conceptSetPersonCounts,
      pegasusConnection = pegasusConnection,
      pegasusDatabaseSchema = pegasusDatabaseSchema
    )
  }
  for (databaseReference in databaseReferences) {
    checkRequirementsMetDatabase(phenotypes = phenotypes,
                                 databaseReference = databaseReference)
  }
}

checkRequirementsMetDatabase <- function(phenotypes,
                                         databaseReference) {
  connection <- DatabaseConnector::connect(databaseReference$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  referenceTables <- Keeper::createReferenceCohortTableNames(databaseReference$referenceCohortTable)

  message("- Checking if Keeper reference cohort exists for ", databaseReference$databaseName)
  if (!DatabaseConnector::existsTable(connection = connection,
                                      databaseSchema = databaseReference$referenceCohortDatabaseSchema,
                                      tableName = referenceTables$referenceCohortMetadataTable)) {

    stop(sprintf("No reference cohort table with database schema '%s' and table name '%s' found.",
                 databaseReference$referenceCohortDatabaseSchema,
                 referenceTables$referenceCohortMetadataTable))
  }
  sql <- "
      SELECT phenotype,
        cohort_definition_id
      FROM @database_schema.@table;
    "
  existingPhenotypes <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseReference$referenceCohortDatabaseSchema,
    table = referenceTables$referenceCohortMetadataTable,
    snakeCaseToCamelCase = TRUE
  )
  missingPhenotypes <- phenotypes |>
    filter(!.data$phenotypeName %in% existingPhenotypes$phenotype)
  if (nrow(missingPhenotypes) > 0) {
    stop(sprintf("Missing reference cohort for phenotype(s): '%s'",
                 paste(missingPhenotypes$phenotype, collapse = "', '")))
  }
  existingPhenotypes <- existingPhenotypes|>
    filter(.data$phenotype %in% phenotypes$phenotypeName)

  message("- Checking if Keeper patient profiles exists for ", databaseReference$databaseName)
  if (!DatabaseConnector::existsTable(connection = connection,
                                      databaseSchema = databaseReference$referenceCohortDatabaseSchema,
                                      tableName = paste0(databaseReference$referenceCohortTable, "_profile"))) {
    stop(sprintf("No reference cohort profile table with database schema '%s' and table name '%s' found.",
                 databaseReference$referenceCohortDatabaseSchema,
                 paste0(databaseReference$referenceCohortTable, "_profile")))
  }
  sql <- "
      SELECT DISTINCT(cohort_definition_id)
      FROM @database_schema.@table
      WHERE cohort_definition_id IN (@cohort_ids);
    "
  existingProfileCohortIds <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseReference$referenceCohortDatabaseSchema,
    table = paste0(databaseReference$referenceCohortTable, "_profile"),
    cohort_ids = existingPhenotypes$cohortDefinitionId
  )[, 1]
  missingPhenotypes <- existingPhenotypes |>
    filter(!.data$cohortDefinitionId %in% existingProfileCohortIds)
  if (nrow(missingPhenotypes) > 0) {
    stop(sprintf("Missing reference cohort for phenotype(s): '%s'",
                 paste(missingPhenotypes$phenotype, collapse = "', '")))
  }
}
