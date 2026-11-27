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
#' @param skipIfExists Skip creation of cohort definition if a MonoFlow cohort definition already exists for this
#'                     phenotype?
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
                                                        skipIfExists = TRUE,
                                                        workFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(phenotypes, min.rows = 1, add = errorMessages)
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeId",
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
  checkmate::assertLogical(skipIfExists, len = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(pegasusConnection) && is.null(pegasusConnectionDetails)) {
    stop("Must provide either pegasusConnection or pegasusConnectionDetails")
  }
  if (is.null(pegasusConnection)) {
    pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)
    on.exit(DatabaseConnector::disconnect(pegasusConnection))
  }
  if (skipIfExists) {
    sql <- "
    SELECT DISTINCT phenotype_id
    FROM @database_schema.cohort_definition
    WHERE cd_source_type = 'MonoFlow'
      AND phenotype_id IN (@phenotype_ids);
  "
    existingPhenotypeIds <- DatabaseConnector::renderTranslateQuerySql(
      connection = pegasusConnection,
      sql = sql,
      database_schema = pegasusDatabaseSchema,
      phenotype_ids = phenotypes$phenotypeId
    )
    if (length(existingPhenotypeIds) > 0) {
      message(sprintf("- Skipping %d of %d phenotypes that already have MonoFlow cohort definitions.",
                      length(existingPhenotypeIds),
                      nrow(phenotypes)))
      phenotypes <- phenotypes |>
        filter(!.data$phenotypeId %in% existingPhenotypeIds)
      if (nrow(phenotypes) == 0) {
        return(tibble())
      }
    }
  }

  startTime <- Sys.time()
  startCost <- costTracker$amount

  dir.create(workFolder, recursive = TRUE, showWarnings = FALSE)

  jsonFiles <- file.path(workFolder, gsub("[^[:alnum:]]+", "_", phenotypes$phenotypeName), "best_cohort.json")
  incompletePhenotypes <- phenotypes[!file.exists(jsonFiles), ]

  if (nrow(incompletePhenotypes) > 0) {
    checkRequirementsMet(phenotypes = incompletePhenotypes,
                         llmClient = llmClient,
                         costTracker = costTracker,
                         pegasusConnection = pegasusConnection,
                         pegasusDatabaseSchema = pegasusDatabaseSchema,
                         databaseReferences = databaseReferences)
  }

  cohortDefinitions <- list()
  for (i in seq_len(nrow(phenotypes))) {
    phenotype <- phenotypes[i, ]
    outputFolder <- file.path(workFolder, gsub("[^[:alnum:]]+", "_", phenotype$phenotypeName))
    bestJsonFile <- file.path(outputFolder, "best_cohort.json")
    if (!file.exists(bestJsonFile)) {
      message("Creating cohort definition for ", phenotype$phenotypeName)
      dir.create(outputFolder, showWarnings = FALSE)

      context <- PegasusAgentContext$new(phenotype = phenotype,
                                         pegasusConnection = pegasusConnection,
                                         pegasusDatabaseSchema = pegasusDatabaseSchema,
                                         databaseReferences = databaseReferences)
      tryCatch({
        systemPromptFile <- system.file("prompts", "MonoFlowSystemPrompt.txt", package = "Pegasus")
        systemPrompt <- paste(readLines(systemPromptFile), collapse = "\n")

        monoFlowAgent <- llmClient$clone()
        monoFlowAgent$set_system_prompt(systemPrompt)
        monoFlowAgent$register_tools(createAgentTools(context))

        prompt <- constructPrompt(phenotype, context)

        # Run chat
        monoFlowAgent$set_turns(list())
        response <- monoFlowAgent$chat(prompt)

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
        json <- convertCaprToJson(context, bestCohortDefinition$capr)
        writeLines(json, bestJsonFile)

        # Save cost
        cost <- monoFlowAgent$get_cost("all")
        writeLines(sprintf("Total cost: $%0.2f", cost), file.path(outputFolder, "cost.txt"))
        costTracker$amount <- costTracker$amount + cost
      },
      finally = context$close()
      )
    } else {
      json <- paste(readLines(bestJsonFile), collapse = "\n")
      if (file.exists(file.path(outputFolder, "cost.txt"))) {
        costString <- readLines(file.path(outputFolder, "cost.txt"))
        costTracker$amount <- costTracker$amount + as.numeric(gsub("Total cost: \\$", "", costString))
      }
    }
    cohortDefinitions[[i]] <- tibble(
      phenotypeId = phenotype$phenotypeId,
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

constructPrompt <- function(phenotype, context) {
  databases <- getDatabaseDescription(context)
  availableConceptSets <- listConceptSets(context)

  context <- paste(
    "<databases>",
    databases,
    "</databases>",
    "",
    "<phenotype_name>",
    phenotype$phenotypeName,
    "</phenotype_name>",
    "",
    "<clinical_definition>",
    phenotype$clinicalDefinition,
    "</clinical_definition>",
    "",
    "<available_concept_sets>",
    availableConceptSets,
    "</available_concept_sets>",
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
  phenotypeToConceptSets <- downloadPhenotypeToConceptSets(phenotypeIds = phenotypes$phenotypeId,
                                                           pegasusConnection = pegasusConnection,
                                                           pegasusDatabaseSchema = pegasusDatabaseSchema)
  phenotypeIds <- unique(phenotypeToConceptSets$phenotypeId)
  if (length(phenotypeIds) != nrow(phenotypes)) {
    stop(sprintf("No concept sets found for phenotype(s) '%s'",
                 paste(phenotypes$phenotypeName[!phenotypes$phenotypeId %in% phenotypeIds], collapse = "', '")))
  }

  message("- Ensuring standard concept sets exist in Pegasus schema")
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

  message("- Ensuring standard concept sets have counts")
  conceptSetPersonCounts <- createConceptSetCounts(conceptSets = standardConceptSets,
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
    message("- Checking if all concept sets have counts in ", databaseReference$databaseName)
    sql <- "
      SELECT TOP 10 concept_set_name
      FROM @database_schema.phenotype
      INNER JOIN @database_schema.phenotype_to_concept_set
        ON phenotype.phenotype_id = phenotype_to_concept_set.phenotype_id
      INNER JOIN @database_schema.concept_set
        ON phenotype_to_concept_set.concept_set_id = concept_set.concept_set_id
      LEFT JOIN @database_schema.concept_set_person_count
        ON phenotype_to_concept_set.concept_set_id = concept_set_person_count.concept_set_id
          AND concept_set_person_count.database_id = @database_id
      WHERE phenotype.phenotype_id IN (@phenotype_ids)
        AND concept_set_person_count.concept_set_id IS NULL;
    "
    missingCounts <- DatabaseConnector::renderTranslateQuerySql(
      connection = pegasusConnection,
      sql = sql,
      database_schema = pegasusDatabaseSchema,
      database_id = databaseReference$databaseId,
      phenotype_ids = phenotypes$phenotypeId,
      snakeCaseToCamelCase = TRUE
    )
    if (nrow(missingCounts) > 0) {
      stop("Missing concept set counts for (amongst others) concept sets '%s'",
           paste(missingCounts$conceptSetName, collapse = "', '"))
    }

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
      SELECT phenotype AS phenotype_name,
        cohort_definition_id AS phenotype_id
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
    filter(!.data$phenotypeId %in% existingPhenotypes$phenotypeId)
  if (nrow(missingPhenotypes) > 0) {
    stop(sprintf("Missing reference cohort for phenotype(s): '%s'",
                 paste(missingPhenotypes$phenotypeName, collapse = "', '")))
  }
  existingPhenotypes <- existingPhenotypes|>
    filter(.data$phenotypeId %in% phenotypes$phenotypeId)

  message("- Checking if Keeper patient profiles exists for ", databaseReference$databaseName)
  if (!DatabaseConnector::existsTable(connection = connection,
                                      databaseSchema = databaseReference$referenceCohortDatabaseSchema,
                                      tableName = paste0(databaseReference$referenceCohortTable, "_profile"))) {
    stop(sprintf("No reference cohort profile table with database schema '%s' and table name '%s' found.",
                 databaseReference$referenceCohortDatabaseSchema,
                 paste0(databaseReference$referenceCohortTable, "_profile")))
  }
  sql <- "
      SELECT DISTINCT(cohort_definition_id) AS phenotype_id
      FROM @database_schema.@table
      WHERE cohort_definition_id IN (@cohort_ids);
    "
  existingProfilePhenotypeIds <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    database_schema = databaseReference$referenceCohortDatabaseSchema,
    table = paste0(databaseReference$referenceCohortTable, "_profile"),
    cohort_ids = existingPhenotypes$phenotypeId
  )[, 1]
  missingPhenotypes <- existingPhenotypes |>
    filter(!.data$phenotypeId %in% existingProfilePhenotypeIds)
  if (nrow(missingPhenotypes) > 0) {
    stop(sprintf("Missing reference cohort for phenotype(s): '%s'",
                 paste(missingPhenotypes$phenotypeName, collapse = "', '")))
  }
}

listConceptSets <- function(context) {
  sql <- "
    SELECT concept_set_name,
      has_descendants,
      database_meta_data.database_id,
      cdm_source_abbreviation,
      overall_persons
    FROM @database_schema.phenotype_to_concept_set
    INNER JOIN @database_schema.concept_set
      ON phenotype_to_concept_set.concept_set_id = concept_set.concept_set_id
    INNER JOIN @database_schema.concept_set_person_count
      ON concept_set.concept_set_id = concept_set_person_count.concept_set_id
    INNER JOIN @database_schema.database_meta_data
      ON concept_set_person_count.database_id = database_meta_data.database_id
    WHERE phenotype_id = @phenotype_id
      AND capr_concept_set_expression IS NOT NULL;
  "
  conceptSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = context$pegasusConnection,
    sql = sql,
    database_schema = context$pegasusDatabaseSchema,
    phenotype_id = context$phenotype$phenotypeId,
    snakeCaseToCamelCase = TRUE
  )

  sql <- "
    SELECT concept_set_name,
      has_descendants,
      database_meta_data.database_id,
      cdm_source_abbreviation,
      overall_persons
    FROM @database_schema.concept_set
    INNER JOIN @database_schema.concept_set_person_count
      ON concept_set.concept_set_id = concept_set_person_count.concept_set_id
    INNER JOIN @database_schema.database_meta_data
      ON concept_set_person_count.database_id = database_meta_data.database_id
    WHERE concept_set_name IN (@concept_set_names);
  "
  standardConceptSets <- DatabaseConnector::renderTranslateQuerySql(
    connection = context$pegasusConnection,
    sql = sql,
    database_schema = context$pegasusDatabaseSchema,
    concept_set_names = paste(sapply(getStandardConceptSets()$conceptSetName, quoteSqlString), collapse = ", "),
    snakeCaseToCamelCase = TRUE
  )

  subset <- bind_rows(conceptSets, standardConceptSets) |>
    filter(.data$databaseId %in% context$getDatabaseIds()) |>
    select("conceptSetName",
           "hasDescendants",
           "cdmSourceAbbreviation",
           "overallPersons") |>
    slice_head(n =1, by = c("conceptSetName",
                            "hasDescendants",
                            "cdmSourceAbbreviation")) |>
    tidyr::pivot_wider(
      id_cols = c("conceptSetName", "hasDescendants"),
      names_from = "cdmSourceAbbreviation",
      values_from = "overallPersons",
      names_prefix = "persons in ",
      values_fill = 0
    ) |>
    arrange(.data$conceptSetName)
  table <- formatPipeTable(subset)
  return(table)
}

getDatabaseDescription <- function(context, database = NULL) {
  databaseReferences <- context$selectDatabaseReferences(database)
  descriptions <- list()
  for (i in seq_along(databaseReferences)) {
    descriptions[[i]] <- getDescriptionForDatabase(connection = context$getConnection(databaseReferences[[i]]$databaseName),
                                                   databaseReference = databaseReferences[[i]])
  }
  descriptions <- bind_rows(descriptions)
  json <- jsonlite::toJSON(descriptions)
  return(json)
}
