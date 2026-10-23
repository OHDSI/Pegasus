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

createConceptSetTargetDefinitions <- function(
    conceptSetTargets,
    phenotypes,
    minPhenotypeFraction = 0.02,
    llmClient,
    costTracker = NULL,
    pegasusConnectionDetails = NULL,
    pegasusDatabaseSchema = NULL,
    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
    cacheFolder = NULL) {

  startTime <- Sys.time()

  targets <- conceptSetTargets |>
    filter(.data$category != "Hypernym", .data$domainId !="Drug")
  uniqueTargets <- targets |>
    group_by(.data$conceptId, .data$conceptName) |>
    summarise(nPhenotypes = length(unique(.data$phenotype)), .groups = "drop")
  nTotalPhenotypes <- targets |>
    distinct(.data$phenotype) |>
    count() |>
    pull()
  lowFrequencyTargets <- uniqueTargets |>
    filter(.data$nPhenotypes < minPhenotypeFraction * nTotalPhenotypes) |>
    mutate(sourceType = "Concept + descendants")

  if (!is.null(pegasusConnectionDetails)) {
    existingDefinitionData <- downloadExistingDefinitionData(
      pegasusConnectionDetails = pegasusConnectionDetails,
      pegasusDatabaseSchema = pegasusDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      uniqueTargets = uniqueTargets
    )
  }
  definitionsFromPhenotypes <- uniqueTargets |>
    mutate(nornName = normalizeNames(.data$conceptName)) |>
    inner_join(phenotypes |>
                 rename(phenotypeName = "phenotype") |>
                 mutate(nornName = normalizeNames(.data$phenotypeName)),
               by = join_by("nornName")) |>
    slice_head(n = 1, by = "conceptId")
  definitionsFromPhenotypes <- definitionsFromPhenotypes |>
    bind_rows(existingDefinitionData$definitionsFromPhenotypes) |>
    slice_head(n = 1, by = "conceptId") |>
    mutate(type = "From human definition")
  lowFrequencyTargets <- lowFrequencyTargets |>
    filter(!.data$conceptId %in% definitionsFromPhenotypes$conceptId)
  uniqueTargets <- uniqueTargets |>
    filter(!.data$conceptId %in% definitionsFromPhenotypes$conceptId,
           !.data$conceptId %in% lowFrequencyTargets$conceptId) |>
    mutate(type = "From AI definition")
  targets <- targets |>
    filter(.data$conceptId %in% uniqueTargets$conceptId)

}

generateDefinitions <- function(targets,
                                uniqueTargets,
                                existingDefinitionData,
                                llmClient,
                                costTracker,
                                cacheFolder) {
  dir.create(cacheFolder, recursive = TRUE, showWarnings = FALSE)


  # templateFile <- file.path("inst", "prompts", "CreateConceptSetTargetDefinitionsPrompt.txt")
  templateFile <- system.file("prompts", "CreateConceptSetTargetDefinitionsPrompt.txt", package = "Pegasus")
  generatePromptTemplate <- paste(readLines(templateFile), collapse = "\n")

  message(sprintf("Generating %d definitions", nrow(uniqueTargets)))
  definitions <- list()
  phenotypeToConceptSetName <- list()

  # i = 1
  for (i in seq_len(nrow(uniqueTargets))) {
    row <- uniqueTargets[i, ]
    fileName <- file.path(cacheFolder, sprintf("%s.json", gsub("[^[:alnum:]]+", "_", row$conceptName)))

    existingDefinitions <- existingDefinitionData$draftTargetToConceptSet |>
      filter(.data$conceptId == row$conceptId) |>
      distinct(.data$conceptSetId) |>
      inner_join(existingDefinitionData$conceptSets, by = join_by("conceptSetId"))
    phenotypeTargetCategories <- targets |>
      filter(.data$conceptId == row$conceptId) |>
      distinct(.data$phenotype, .data$pertains, .data$category) |>
      mutate(context = sprintf("%s of %s%s",
                               .data$category,
                               if_else(.data$pertains == "Alternative diagnoses",
                                       "alternative diagnoses of ",
                                       ""),
                               .data$phenotype))
    if (file.exists(fileName)) {
      definition <- jsonlite::read_json(fileName)
    } else {
      if (nrow(existingDefinitions) > 0) {
        selectExistingDefinitions(
          existingDefinitions = existingDefinitions,
          phenotypeTargetCategories = phenotypeTargetCategories,
          llmClient = llmClient,
          costTracker = costTracker
        )

      }


      prompt <- gsub("%term%", row$conceptName, promptTemplate)
      prompt <- gsub("%contexts%", paste(sprintf("- %s", phenotypeTargetCategories$context), collapse = "\n"), prompt)
      # writeLines(prompt)
      maxRetries <- 5
      for (attempt in seq_len(maxRetries)) {
        llmClient$set_turns(list())
        llmClient$set_system_prompt("")
        definition <- llmClient$chat_structured(
          prompt,
          echo = "none",
          type = ellmer::type_array(
            ellmer::type_object(
              label = ellmer::type_string(),
              definition = ellmer::type_string(),
              coveredContexts = ellmer::type_array(
                ellmer::type_enum(values = c("all", phenotypeTargetCategories$context))
              )
            )
          ),
          convert = FALSE
        )
        cost <- cost + llmClient$get_cost()
        if (length(definition) == 1) {
          break
        } else {
          returnContexts <- lapply(definition, function(x) x$coveredContexts)
          returnContexts <- unlist(do.call(c, returnContexts))
          if (length(returnContexts) == nrow(phenotypeTargetCategories) &&
              all(sort(returnContexts) == sort(phenotypeTargetCategories$context))) {
            break
          }
          if (attempt == maxRetries) {
            stop("LLM unable to return all provided contexts after ", maxRetries, " attempts. Giving up")
          } else {
            message("LLM did not return all provided contexts. Retrying")
          }
        }
      }
      jsonlite::write_json(definition, fileName, pretty = TRUE, auto_unbox = TRUE)
    }

    if (length(definition) == 1) {
      definitionRows <- tibble(conceptSetName = row$conceptName,
                               conceptSetConceptId = row$conceptId,
                               seedConceptId = row$conceptId,
                               definition = definition[[1]]$definition)
      phenotypeToConceptSetNameRows <- phenotypeTargetCategories |>
        select(-"context") |>
        mutate(conceptSetName = row$conceptName)
    } else {
      definitionRows <- lapply(definition, function(x) tibble(conceptSetName = x$label,
                                                              conceptSetConceptId = NA_real_,
                                                              seedConceptId = row$conceptId,
                                                              definition = x$definition))
      definitionRows <- bind_rows(definitionRows)
      phenotypeToConceptSetNameRows <- lapply(definition, function(x) tibble(context = unlist(x$coveredContexts),
                                                                             conceptSetName = x$label))
      phenotypeToConceptSetNameRows <- bind_rows(phenotypeToConceptSetNameRows)
      phenotypeToConceptSetNameRows <- phenotypeTargetCategories |>
        inner_join(phenotypeToConceptSetNameRows, by = join_by("context")) |>
        select(-"context")
      message("- Splitting '", row$conceptName, "' into '", paste(definitionRows$conceptSetName, collapse = "', '"), "'.")
    }
    definitions[[i]] <- definitionRows
    phenotypeToConceptSetName[[i]] <- phenotypeToConceptSetNameRows
    if (i %% 25 == 0) {
      message(sprintf("- Processed %d of %d concept sets. Costs so far: $%0.2f",
                      i,
                      nrow(uniqueTargets),
                      cost))
    }
  }

  definitions <- bind_rows(definitions)
  phenotypeToConceptSetName <- bind_rows(phenotypeToConceptSetName)

  delta <- Sys.time() - startTime
  message(paste0(
    "Reviewing cases took ",
    round(delta, 1),
    " ",
    attr(delta, "units"),
    " and cost $",
    round(cost, 2)
  ))
  readr::write_csv(definitions, "ConceptSetTargetDefinitions.csv")
  readr::write_csv(phenotypeToConceptSetName, "PhenotypeToConceptSetTargetNames.csv")
}

selectExistingDefinitions <- function(
    conceptName,
    existingDefinitions,
    phenotypeTargetCategories,
    llmClient,
    costTracker
) {
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
    phenotypeTargetCategories <- phenotypeTargetCategories |>
      mutate(conceptSetId = selectedDefinitions$conceptSetId,
             clinicalDefinition = selectedDefinitions$clinicalDefinition)
  }
  phenotypeTargetCategories |>
    inner_join(selectedDefinitions, by = join_by(context))
}

downloadExistingDefinitionData <- function(
    pegasusConnectionDetails,
    pegasusDatabaseSchema,
    uniqueTargets,
    tempEmulationSchema
) {
  connection <- DatabaseConnector::connect(pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

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
    SELECT phenotype_id,
      phenotype_name,
      clinical_definition
    FROM @database_schema.phenotype;
  "
  allPhenotypes <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = pegasusDatabaseSchema,
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema
  ) |>
    mutate(nornName = normalizeNames(.data$phenotypeName))
  definitionsFromPhenotypes <- uniqueTargets |>
    mutate(nornName = normalizeNames(.data$conceptName)) |>
    inner_join(allPhenotypes, by = join_by("nornName")) |>
    slice_head(n = 1, by = "conceptId")

  return(list(draftTargetToConceptSet = draftTargetToConceptSet,
              conceptSets = conceptSets,
              definitionsFromPhenotypes = definitionsFromPhenotypes))
}

