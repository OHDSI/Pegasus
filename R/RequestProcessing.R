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

#' Create a Pegasus config object
#'
#' @param pegasusConnectionDetails An R object of type `connectionDetails` created using the
#'                                 [DatabaseConnector::createConnectionDetails()] function for connecting to the
#'                                 database holding the Pegasus data model.
#' @param pegasusDatabaseSchema    The name of the database schema holding the Pegasus data model
#' @template DatabaseReferences
#' @template LlmClient
#' @param llmClientFrontier An `ellmer` chat object to connect to an **advanced reasoning** LLM, as created for example
#'                          using `ellmer::chat_ollama()`.
#' @param baseUrl The base URL of the OHDSI Web API.
#' @param rootWorkFolder A folder on the local file system where results can be written.
#' @template MaxCores
#'
#' @returns
#' Returns an object of type `PegasusConfig`, to be used in `processRequest()`.
#'
#' @export
createPegasusConfig <- function(pegasusConnectionDetails,
                                pegasusDatabaseSchema,
                                databaseReferences,
                                llmClient,
                                llmClientFrontier = llmClient,
                                baseUrl = NULL,
                                rootWorkFolder,
                                maxCores = 4) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(databaseReferences, types = c("DatabaseReference"), min.len = 1, add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertR6(llmClientFrontier, "Chat", add = errorMessages)
  checkmate::assertCharacter(baseUrl, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(rootWorkFolder, len = 1, add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  pegasusConfig <- list()
  for (name in names(formals(createPegasusConfig))) {
    pegasusConfig[[name]] <- get(name)
  }
  class(pegasusConfig) <- "PegasusConfig"
  return(pegasusConfig)
}

#' Ensures all databases in the config exist in the Pegasus results model
#'
#' @param pegasusConfig A Pegasus config object as created by `createPegasusConfig()`
#'
#' @returns
#' Does not return anything. Is executed for populated the Pegasus data model.
#'
#' @export
ensureDatabases <- function(pegasusConfig) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(pegasusConfig, "PegasusConfig", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  pegasusConnection <- DatabaseConnector::connect(pegasusConfig$pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(pegasusConnection))

  newDatabases <- getDatabaseMetaData(databaseReferences = pegasusConfig$databaseReferences,
                                      pegasusConnection = pegasusConnection,
                                      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
  if (nrow(newDatabases) > 0) {
    uploadDatabaseMetaData(
      databaseMetaData = newDatabases,
      pegasusConnection = pegasusConnection,
      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema
    )
  }
  invisible(NULL)
}

TEMPLATES <- "templates"
MONO_FLOW <- "monoflow"

#' Process a request made in the Shiny app
#'
#' @description
#' This function will make sure the artifacts implied in the request exist; For the phenotype, cohorts will be created
#' using the specified approaches, and all existing cohort definitions will be evaluated. Any artifacts that already
#' exist will not be created again.
#'
#' The phenotype ID must already exist in the Pegasus results database. The database IDs must exist in the provided
#' config, and will be added to the results database if needed.
#'
#' If no cohort creation process is specified, concept sets will still be created.
#'
#' @param phenotypeId The phenotype ID.
#' @param databaseIds A vector of database IDs.
#' @param cohortCreationApproaches A vector of strings with possible values 'templates' and 'monoflow'.
#' @param pegasusConfig A Pegasus config object as created by `createPegasusConfig()`
#'
#' @returns
#' Does not return anything. Is executed for populated the Pegasus data model.
#'
#' @export
processRequest <- function(phenotypeId,
                           databaseIds,
                           cohortCreationApproaches,
                           pegasusConfig) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(phenotypeId, add = errorMessages)
  checkmate::assertIntegerish(databaseIds, min.len = 1, unique = TRUE, add = errorMessages)
  checkmate::assertCharacter(cohortCreationApproaches, add = errorMessages)
  checkmate::assertSubset(cohortCreationApproaches, choices = c(TEMPLATES, MONO_FLOW), add = errorMessages)
  checkmate::assertClass(pegasusConfig, "PegasusConfig", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  costTracker <- createCostTracker()

  ParallelLogger::addDefaultFileLogger(file.path(pegasusConfig$rootWorkFolder, "log.txt"), name = "PROCESS_REQUEST_LOG")
  on.exit(ParallelLogger::unregisterLogger("PROCESS_REQUEST_LOG"))

  pegasusConnection <- DatabaseConnector::connect(pegasusConfig$pegasusConnectionDetails)
  on.exit(DatabaseConnector::disconnect(pegasusConnection))

  dir.create(pegasusConfig$rootWorkFolder, recursive = TRUE, showWarnings = FALSE)

  # Filter database references -----------------------------------------------------------------------------------------
  databaseReferences <- pegasusConfig$databaseReferences[sapply(pegasusConfig$databaseReferences,
                                                                function(x) x$databaseId %in% databaseIds)]
  if (length(databaseReferences) < length(databaseIds)) {
    foundDatabaseIds <- sapply(databaseReferences, function(x) x$databaseId)
    missingDatabaseIds <- databaseIds[!databaseIds %in% foundDatabaseIds]
    stop("Could not find databases with IDs ", paste(missingDatabaseIds, collapse = ", "))
  }

  # Get phenotype ------------------------------------------------------------------------------------------------------
  phenotypes <- downloadPhenotypes(phenotypeIds = phenotypeId,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
  if (nrow(phenotypes) == 0) {
    stop(sprintf("No phenotype with ID %s found", phenotypeId))
  }

  # Create phenotype meta data -----------------------------------------------------------------------------------------
  phenotypeMetaData <- generatePhenotypeMetaData(phenotypes = phenotypes,
                                                 llmClient = pegasusConfig$llmClient,
                                                 costTracker = costTracker,
                                                 pegasusConnection = pegasusConnection,
                                                 pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
  if (nrow(phenotypeMetaData) > 0) {
    uploadPhenotypeMetaData(phenotypeMetaData = phenotypeMetaData,
                            pegasusConnection = pegasusConnection,
                            pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
  }

  # Create concept sets ------------------------------------------------------------------------------------------------
  conceptSetTargets <- generateConceptSetTargets(
    phenotypes = phenotypes,
    databaseReference = databaseReferences[[1]],
    llmClient = pegasusConfig$llmClient,
    costTracker = costTracker,
    cacheFolder =  file.path(pegasusConfig$rootWorkFolder, "cacheTargets"),
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema
  )

  if (nrow(conceptSetTargets) > 0) {
    # Create concept set target definitions ------------------------------------------------------------------------------
    conceptSetTargetDefinitions <- generateOrReuseConceptSetTargetDefinitions(
      conceptSetTargets = conceptSetTargets,
      phenotypes = phenotypes,
      llmClient = pegasusConfig$llmClient,
      costTracker = costTracker,
      pegasusConnection = pegasusConnection,
      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema,
      cacheFolder = file.path(pegasusConfig$rootWorkFolder, "cacheTargetDefinitions")
    )

    # Create concept sets ------------------------------------------------------------------------------------------------
    conceptSets <- generateConceptSets(
      conceptSetTargetDefinitions = conceptSetTargetDefinitions,
      llmClient = pegasusConfig$llmClient,
      costTracker = costTracker,
      databaseReference = databaseReferences[[1]],
      maxCores = pegasusConfig$maxCores,
      cacheFolder = file.path(pegasusConfig$rootWorkFolder, "cacheConceptSets")
    )

    uploadDraftConceptSetTargets(draftConceptSetTargets = conceptSetTargets,
                                 pegasusConnection = pegasusConnection,
                                 pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
    uploadConceptSets(conceptSets = conceptSets,
                      pegasusConnection = pegasusConnection,
                      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
  }

  # Count persons with concept sets ------------------------------------------------------------------------------------
  conceptSetPersonCounts <- createConceptSetCounts(
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema,
    databaseReferences = databaseReferences,
    maxCores = pegasusConfig$maxCores,
    cacheFolder = file.path(pegasusConfig$rootWorkFolder, "cacheConceptSetPersonCounts")
  )
  if (nrow(conceptSetPersonCounts) > 0) {
    uploadConceptSetPersonCounts(conceptSetPersonCounts = conceptSetPersonCounts,
                                 pegasusConnection = pegasusConnection,
                                 pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema)
  }

  # Create Keeper reference cohorts ------------------------------------------------------------------------------------
  status <- createKeeperReferenceCohorts(
    phenotypes = phenotypes,
    llmClient = pegasusConfig$llmClient,
    costTracker = costTracker,
    databaseReferences = databaseReferences,
    maxCores = pegasusConfig$maxCores,
    uploadKeeperProfiles = TRUE,
    workFolder = file.path(pegasusConfig$rootWorkFolder, "cacheKeeperReference")
  )

  # Create cohort definitions using MonoFlow ---------------------------------------------------------------------------
  if (MONO_FLOW %in% cohortCreationApproaches) {
    # Note: for now keeping it simple and only use 1 database reference for agent:
    cohortDefinitions <- generateCohortDefinitionsUsingMonoFlowAgent(
      phenotypes = phenotypes,
      llmClient = pegasusConfig$llmClientFrontier,
      costTracker = costTracker,
      pegasusConnection = pegasusConnection,
      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema,
      databaseReferences = databaseReferences[1],
      workFolder = file.path(pegasusConfig$rootWorkFolder, "cacheMonoFlow")
    )

    if (nrow(cohortDefinitions) != 0) {
      if (!is.null(pegasusConfig$baseUrl)) {
        cohortDefinitionsWithAtlasIds <- insertCohortDefinitionsInAtlas(
          cohortDefinitions = cohortDefinitions,
          baseUrl = pegasusConfig$baseUrl
        )
      }

      uploadCohortDefinitions(
        cohortDefinitions = cohortDefinitionsWithAtlasIds,
        pegasusConnection = pegasusConnection,
        pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema
      )
    }
  }

  # Create cohort definitions using templates --------------------------------------------------------------------------
  if (TEMPLATES %in% cohortCreationApproaches) {
    cohortDefinitions <- generateCohortDefinitionssUsingTemplates(
      phenotypes = phenotypes,
      pegasusConnection = pegasusConnection,
      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema,
      databaseReference = databaseReferences[[1]],
      maxCores = pegasusConfig$maxCores,
      workFolder = file.path(pegasusConfig$rootWorkFolder, "cacheTemplates")
    )

    uploadCohortDefinitions(
      cohortDefinitions = cohortDefinitions,
      pegasusConnection = pegasusConnection,
      pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema
    )
  }

  # Evaluate cohort definitions ----------------------------------------------------------------------------------------
  cohortDefinitionsEvaluations <-evaluateUsingKeeperReferenceCohorts(
    phenotypes = phenotypes,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema,
    databaseReferences = databaseReferences
  )

  # Flip draft and valid flags -----------------------------------------------------------------------------------------
  updatePhenotypes(
    phenotypes = phenotypes,
    draft = 0,
    valid = 1,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = pegasusConfig$pegasusDatabaseSchema,
  )

  delta <- Sys.time() - startTime
  message(sprintf("Processing request took %0.2f %s and cost $%0.2f.",
                  delta,
                  attr(delta, "units"),
                  costTracker$amount))
  invisible(NULL)
}
