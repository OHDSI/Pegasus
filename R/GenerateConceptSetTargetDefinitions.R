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

#' Generate or re-use concept set target definitions
#'
#' @description
#' For one or more phenotypes and the generated concept set targets, create concept set target definitions to be used to
#' create the concept sets. If a concept set with the correct definition already exists its ID will be included in the
#' output.
#'
#'
#' @param conceptSetTargets A tibble of concept set targets as created by `generateConceptSetTargets()`.
#' @param phenotypes        A tibble with two columns: `phenotype`, and `clinicalDefinition`.
#' @param minPhenotypeFraction If a concept set target appears for less than this fraction of the provided phenotypes,
#'                             it will not get a definition but will later be instantiated as a placeholder (concept +
#'                             descendants).
#' @template LlmClient
#' @template CostTracker
#' @template PegasusConnectionDetails
#' @template TempEmulationSchema
#' @param cacheFolder A folder where data objects can be cached so a restart can continue where it left off.
#'
#' @returns
#' A tibble with concept set IDs, definitions, and source types.
#'
#' @export
generateOrReuseConceptSetTargetDefinitions <- function(
    conceptSetTargets,
    phenotypes,
    minPhenotypeFraction = 0.02,
    llmClient,
    costTracker = createCostTracker(),
    pegasusConnectionDetails = NULL,
    pegasusDatabaseSchema = NULL,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    cacheFolder = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(conceptSetTargets, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(conceptSetTargets), must.include = c(
    "phenotype",
    "pertains",
    "category",
    "term",
    "conceptId",
    "conceptName",
    "domainId",
    "conceptClassId",
    "vocabularyId",
    "match"
  ), add = errorMessages)
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotype",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertNumber(minPhenotypeFraction, lower = 0, upper = 1, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(cacheFolder, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (!is.null(pegasusConnectionDetails) && is.null(pegasusDatabaseSchema)) {
    stop("Must provide pegasusDatabaseSchema when providing pegasusConnectionDetails.")
  }
  if (is.null(pegasusConnectionDetails)) {
    connection <- NULL
  } else {
    connection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  dedupedTargets <- conceptSetTargets |>
    filter(match != "None") |>
    slice_head(n = 1, by = c("phenotype",
                             "pertains",
                             "category",
                             "conceptId",
                             "conceptName"))

  # Hypernyms
  hypernymTargets <- dedupedTargets |>
    filter(.data$category == "Hypernym")
  existingHypernymConceptSets <- downloadExistingConceptSetData(
    connection = connection,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    newTargets = hypernymTargets |>
      distinct(.data$conceptId, .data$conceptName),
    sourceType = CONCEPT
  )
  hypernymTargets <- hypernymTargets |>
    left_join(existingHypernymConceptSets, by = join_by("conceptId")) |>
    mutate(sourceType = CONCEPT)

  # Drugs
  drugTargets <- dedupedTargets |>
    filter(.data$domainId =="Drug")
  existingDrugConceptSets <- downloadExistingConceptSetData(
    connection = connection,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    newTargets = drugTargets |>
      distinct(.data$conceptId, .data$conceptName),
    sourceType = CONCEPT_PLUS_DESCENDANTS
  )
  drugTargets <- drugTargets |>
    left_join(existingDrugConceptSets, by = join_by("conceptId")) |>
    mutate(sourceType = CONCEPT_PLUS_DESCENDANTS)

  # Remaining targets
  targets <- dedupedTargets |>
    filter(.data$category != "Hypernym", .data$domainId !="Drug")
  uniqueTargets <- targets |>
    group_by(.data$conceptId, .data$conceptName) |>
    summarise(nPhenotypes = length(unique(.data$phenotype)), .groups = "drop")
  existingDefinitionData <- downloadExistingDefinitionData(
    connection = connection,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    tempEmulationSchema = tempEmulationSchema,
    uniqueTargets = uniqueTargets
  )

  # Definitions created by humans? For phenotypes overlapping with concept set targets
  definitionsFromPhenotypes <- uniqueTargets |>
    filter(!.data$conceptId %in% existingDefinitionData$definitionsFromPhenotypes$conceptId) |>
    mutate(nornName = normalizeNames(.data$conceptName)) |>
    inner_join(phenotypes |>
                 rename(phenotypeName = "phenotype") |>
                 mutate(nornName = normalizeNames(.data$phenotypeName)),
               by = join_by("nornName")) |>
    slice_head(n = 1, by = "conceptId") |>
    mutate(conceptSetId = NA_real_,
           conceptSetName = NA_character_) |>
    select(-"nornName")
  definitionsFromPhenotypes <- definitionsFromPhenotypes |>
    bind_rows(existingDefinitionData$definitionsFromPhenotypes) |>
    slice_head(n = 1, by = "conceptId")
  humanDefinitionTargets <- targets |>
    inner_join(definitionsFromPhenotypes |>
                 select(-"conceptName"),
               by = join_by("conceptId")) |>
    mutate(sourceType = FROM_HUMAN_DEFINITION)

  # Low frequency targets
  nTotalPhenotypes <- targets |>
    distinct(.data$phenotype) |>
    count() |>
    pull()
  lowFrequencyTargets <- uniqueTargets |>
    filter(.data$nPhenotypes < minPhenotypeFraction * nTotalPhenotypes)
  lowFrequencyTargets <- lowFrequencyTargets |>
    filter(!.data$conceptId %in% definitionsFromPhenotypes$conceptId,
           !.data$conceptId %in% existingDefinitionData$conceptSets$conceptId)
  lowFrequencyTargets <- lowFrequencyTargets |>
    left_join(targets, by = join_by("conceptId")) |>
    mutate(sourceType = CONCEPT_PLUS_DESCENDANTS)

  # Use AI to generate definitions (or use existing one if exists)
  uniqueTargets <- uniqueTargets |>
    filter(!.data$conceptId %in% definitionsFromPhenotypes$conceptId,
           !.data$conceptId %in% lowFrequencyTargets$conceptId)
  targets <- targets |>
    filter(.data$conceptId %in% uniqueTargets$conceptId)

  aiDefinitionTargets <- fetchOrGenerateDefinitions(targets = targets,
                                                    uniqueTargets = uniqueTargets,
                                                    existingDefinitionData = existingDefinitionData,
                                                    llmClient = llmClient,
                                                    costTracker = costTracker,
                                                    cacheFolder = cacheFolder)
  aiDefinitionTargets <- aiDefinitionTargets |>
    mutate(sourceType = FROM_AI_DEFINITION)

  result <- bind_rows(
    hypernymTargets,
    drugTargets,
    humanDefinitionTargets,
    lowFrequencyTargets,
    aiDefinitionTargets
  ) |>
    select("phenotype",
           "pertains",
           "category",
           "conceptId",
           "conceptName",
           "conceptSetId",
           "conceptSetName",
           "clinicalDefinition",
           "sourceType",
           "targetPhenotypeId",
           "targetPhenotypeName")
  return(result)
}

fetchOrGenerateDefinitions <- function(targets,
                                       uniqueTargets,
                                       existingDefinitionData,
                                       llmClient,
                                       costTracker,
                                       cacheFolder) {
  startTime <- Sys.time()
  if (!is.null(cacheFolder)) {
    dir.create(cacheFolder, recursive = TRUE, showWarnings = FALSE)
  }

  message(sprintf("Retrieving or generating %d definitions", nrow(uniqueTargets)))
  definitions <- list()
  nContextTarget <- 0
  nMatched <- 0

  # i = 1
  for (i in seq_len(nrow(uniqueTargets))) {
    row <- uniqueTargets[i, ]
    phenotypeTargetCategories <- targets |>
      filter(.data$conceptId == row$conceptId) |>
      distinct(.data$phenotype, .data$conceptId, .data$conceptName, .data$pertains, .data$category) |>
      mutate(context = sprintf("%s of %s%s",
                               .data$category,
                               if_else(.data$pertains == "Alternative diagnoses",
                                       "alternative diagnoses of ",
                                       ""),
                               .data$phenotype))
    nContextTarget <- nContextTarget + nrow(phenotypeTargetCategories)

    cacheFileName <- paste0(digest::digest(phenotypeTargetCategories), ".rds")
    definitionsForTarget <- withCache({
      fetchOrGenerateDefinitionsSingleTarget(phenotypeTargetCategories = phenotypeTargetCategories,
                                             existingDefinitionData = existingDefinitionData,
                                             llmClient = llmClient,
                                             costTracker = costTracker)
    },
    cacheFolder = cacheFolder,
    fileName = cacheFileName
    )

    nMatched <- nMatched + sum(!is.na(definitionsForTarget$conceptSetId))
    uniqueConceptSetNames <- definitionsForTarget |>
      distinct(.data$conceptSetName) |>
      pull()
    if (length(uniqueConceptSetNames) > 1) {
      message(sprintf("- Concept set target '%s' was splint into '%s'",
                      row$conceptName,
                      paste(uniqueConceptSetNames, collapse = "', '")))
    }
    definitions[[i]] <- definitionsForTarget
    if (i %% 25 == 0) {
      message(sprintf(paste("- Processed %d of %d concept set targets, representing %d context-target combinations.",
                            "Re-used %d existing definitions and generated %d new ones. Costs so far: $%0.2f"),
                      i,
                      nrow(uniqueTargets),
                      nContextTarget,
                      nMatched,
                      nContextTarget - nMatched,
                      costTracker$amount))
    }
  }
  definitions <- bind_rows(definitions) |>
    select(-"context")

  delta <- Sys.time() - startTime
  message(sprintf("Processing %d context-target combinations took %0.2f %s and cost $%0.2f. Re-used %d existing definitions and generated %d new ones.",
                  nContextTarget,
                  delta,
                  attr(delta, "units"),
                  costTracker$amount,
                  nMatched,
                  nContextTarget - nMatched))
  return(definitions)
}

fetchOrGenerateDefinitionsSingleTarget <- function(phenotypeTargetCategories,
                                                   existingDefinitionData,
                                                   llmClient,
                                                   costTracker) {
  existingDefinitions <- existingDefinitionData$draftTargetToConceptSet |>
    filter(.data$conceptId == phenotypeTargetCategories$conceptId[1]) |>
    distinct(.data$conceptSetId) |>
    inner_join(existingDefinitionData$conceptSets, by = join_by("conceptSetId"))
  if (nrow(existingDefinitions) > 0) {
    matchedDefinitions <- selectExistingDefinitions(
      existingDefinitions = existingDefinitions,
      phenotypeTargetCategories = phenotypeTargetCategories,
      llmClient = llmClient,
      costTracker = costTracker
    )
    phenotypeTargetCategories <- phenotypeTargetCategories |>
      anti_join(matchedDefinitions, by = join_by("phenotype", "pertains", "category", "context"))
  } else {
    matchedDefinitions <- NULL
  }
  if (nrow(phenotypeTargetCategories) > 0) {
    generatedDefinitions <- generateNewDefinitions(
      phenotypeTargetCategories = phenotypeTargetCategories,
      llmClient = llmClient,
      costTracker = costTracker
    )
  } else {
    generatedDefinitions <- NULL
  }
  definitions <- bind_rows(matchedDefinitions, generatedDefinitions)
  return(definitions)
}

selectExistingDefinitions <- function(existingDefinitions,
                                      phenotypeTargetCategories,
                                      llmClient,
                                      costTracker) {
  # templateFile <- file.path("inst", "prompts", "SelectExistingConceptSetTargetDefinitionsPrompt.txt")
  templateFile <- system.file("prompts", "SelectExistingConceptSetTargetDefinitionsPrompt.txt", package = "Pegasus")
  promptTemplate <- paste(readLines(templateFile), collapse = "\n")

  prompt <- gsub("%term%", row$conceptName, promptTemplate)
  prompt <- gsub("%contexts%", paste(sprintf("- %s", phenotypeTargetCategories$context), collapse = "\n"), prompt)
  definitionsJson <- existingDefinitions |>
    select(label = "conceptSetName", definition = "clinicalDefinition") |>
    jsonlite::toJSON(pretty = TRUE)
  prompt <- gsub("%existingDefinitions%", definitionsJson, prompt)
  # writeLines(prompt)
  outputType <- ellmer::type_array(
    ellmer::type_object(
      label = ellmer::type_enum(values = c("none", existingDefinitions$conceptSetName)),
      coveredContexts = ellmer::type_array(
        ellmer::type_enum(values = c("all", phenotypeTargetCategories$context))
      )
    )
  )
  response <- queryLlm(
    prompt = prompt,
    llmClient = llmClient,
    costTracker = costTracker,
    outputType = outputType
  )
  selectedDefinitions <- lapply(seq_len(nrow(response)),
                                function(i) tibble(conceptSetName = as.character(response$label[i]),
                                                   context = as.character(response$coveredContexts[i][[1]])))
  selectedDefinitions <- bind_rows(selectedDefinitions) |>
    inner_join(existingDefinitions,
               by = join_by("conceptSetName"))
  if (nrow(selectedDefinitions) == 1 && selectedDefinitions$context == "all") {
    matchedDefinitions <- phenotypeTargetCategories |>
      mutate(conceptSetId = selectedDefinitions$conceptSetId,
             conceptSetName  = selectedDefinitions$conceptSetName,
             clinicalDefinition = selectedDefinitions$clinicalDefinition)
  } else {
    matchedDefinitions <- phenotypeTargetCategories |>
      inner_join(selectedDefinitions, by = join_by("context"))
  }
  return(matchedDefinitions)
}

generateNewDefinitions <- function(phenotypeTargetCategories,
                                   llmClient,
                                   costTracker) {
  # templateFile <- file.path("inst", "prompts", "CreateConceptSetTargetDefinitionsPrompt.txt")
  templateFile <- system.file("prompts", "CreateConceptSetTargetDefinitionsPrompt.txt", package = "Pegasus")
  promptTemplate <- paste(readLines(templateFile), collapse = "\n")
  prompt <- gsub("%term%", row$conceptName, promptTemplate)
  prompt <- gsub("%contexts%", paste(sprintf("- %s", phenotypeTargetCategories$context), collapse = "\n"), prompt)
  # writeLines(prompt)
  outputType <- ellmer::type_array(
    ellmer::type_object(
      label = ellmer::type_string(),
      definition = ellmer::type_string(),
      coveredContexts = ellmer::type_array(
        ellmer::type_enum(values = c("all", phenotypeTargetCategories$context))
      )
    )
  )
  response <- queryLlm(
    prompt = prompt,
    llmClient = llmClient,
    costTracker = costTracker,
    outputType = outputType
  )
  generatedDefinitions <- lapply(seq_len(nrow(response)),
                                 function(i) tibble(conceptSetName = as.character(response$label[i]),
                                                    clinicalDefinition = response$definition[i],
                                                    context = as.character(response$coveredContexts[i][[1]])))
  generatedDefinitions <- bind_rows(generatedDefinitions)
  if (nrow(generatedDefinitions) == 1 && generatedDefinitions$context == "all") {
    generatedDefinitions <- phenotypeTargetCategories |>
      mutate(conceptSetName  = generatedDefinitions$conceptSetName,
             clinicalDefinition = generatedDefinitions$clinicalDefinition,
             conceptSetId = NA_real_)
  } else {
    generatedDefinitions <- phenotypeTargetCategories |>
      inner_join(generatedDefinitions, by = join_by("context")) |>
      mutate(conceptSetId = NA_real_)
  }
  return(generatedDefinitions)
}

downloadExistingDefinitionData <- function(connection,
                                           pegasusDatabaseSchema,
                                           tempEmulationSchema,
                                           uniqueTargets) {
  if (is.null(connection)) {
    return(list(draftTargetToConceptSet = tibble(conceptId = 1, conceptSetId = 1) |>
                  filter(.data$conceptId != 1),
                conceptSets = tibble(conceptSetId = 1) |>
                  filter(.data$conceptSetId != 1),
                definitionsFromPhenotypes = tibble()))
  }

  DatabaseConnector::insertTable(
    connection = connection,
    data = uniqueTargets |>
      select("conceptId"),
    tableName = "#target_concept_id",
    createTable = TRUE,
    dropTableIfExists = TRUE,
    tempTable = TRUE,
    camelCaseToSnakeCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )
  sql <- "
    SELECT *
    FROM @database_schema.draft_target_to_concept_set
    WHERE concept_id IN (
      SELECT concept_id
      FROM #target_concept_id
    );
  "
  draftTargetToConceptSet <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )

  sql <- "
    SELECT concept_set_id,
      concept_set_name,
      clinical_definition
    FROM @database_schema.concept_set
    WHERE valid = 1
      AND has_descendants = 1
      AND concept_set_id IN (
      SELECT DISTINCT concept_set_id
        FROM @database_schema.draft_target_to_concept_set
        WHERE concept_id IN (
          SELECT concept_id
          FROM #target_concept_id
        )
    );
  "
  conceptSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE #target_concept_id;",
    tempEmulationSchema = tempEmulationSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  # Also get definitions of phenotypes with names similar to concept set targets
  sql <- "
    SELECT phenotype_id AS target_phenotype_id,
      phenotype_name AS target_phenotype_name,
      phenotype.clinical_definition,
      concept_set_id,
      concept_set_name
    FROM @database_schema.phenotype
    LEFT JOIN @database_schema.concept_set
      ON phenotype.clinical_definition = concept_set.clinical_definition;
  "
  allPhenotypes <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    mutate(nornName = normalizeNames(.data$targetPhenotypeName))
  definitionsFromPhenotypes <- uniqueTargets |>
    mutate(nornName = normalizeNames(.data$conceptName)) |>
    inner_join(allPhenotypes, by = join_by("nornName")) |>
    slice_head(n = 1, by = "conceptId")

  return(list(draftTargetToConceptSet = draftTargetToConceptSet,
              conceptSets = conceptSets,
              definitionsFromPhenotypes = definitionsFromPhenotypes))
}

downloadExistingConceptSetData <- function(connection,
                                           pegasusDatabaseSchema,
                                           tempEmulationSchema,
                                           newTargets,
                                           sourceType) {
  if (is.null(connection)) {
    return(tibble(conceptId = 1, conceptSetId = 1) |>
             filter(.data$conceptId != 1))
  }
  DatabaseConnector::insertTable(
    connection = connection,
    data = newTargets |>
      select("conceptId"),
    tableName = "#target_concept_id",
    createTable = TRUE,
    dropTableIfExists = TRUE,
    tempTable = TRUE,
    camelCaseToSnakeCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )
  sql <- "
    SELECT concept_id,
      concept_set_id,
      concept_set_name
    FROM @database_schema.concept_set
    WHERE concept_id IN (
      SELECT concept_id
      FROM #target_concept_id
    )
      AND valid = 1
      AND cs_source_type = '@source_type';
  "
  conceptSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    source_type = sourceType,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  )
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "DROP TABLE #target_concept_id;",
    tempEmulationSchema = tempEmulationSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  return(conceptSets)
}
