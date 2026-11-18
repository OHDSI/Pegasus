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
                                baseUrl,
                                rootWorkFolder,
                                maxCores = 4) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(databaseReferences, types = c("DatabaseReference"), min.len = 1, add = errorMessages)
  checkmate::assertR6(llmClientFrontier, "Chat", add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(baseUrl, len = 1, add = errorMessages)
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

  #TODO: implement
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
  checkmate::assertIntegerish(databaseIds, min.len = 1, add = errorMessages)
  checkmate::assertCharacter(cohortCreationApproaches, add = errorMessages)
  checkmate::assertSubset(cohortCreationApproaches, choices = c(TEMPLATES, MONO_FLOW), add = errorMessages)
  checkmate::assertClass(pegasusConfig, "PegasusConfig", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  costTracker <- createCostTracker()

  ParallelLogger::addDefaultFileLogger(file.path(rootWorkFolder, "log.txt"), name = "PROCESS_REQUEST_LOG")
  on.exit(ParallelLogger::unregisterLogger("PROCESS_REQUEST_LOG"))

  dir.create(rootWorkFolder, recursive = TRUE, showWarnings = FALSE)

  #TODO: implement

  # # Phenotype meta data ------------------------------------------------------------------------------------------------
  # phenotypeMetaData <- generatePhenotypeMetaData(phenotypes = phenotypes,
  #                                                llmClient = llmClient,
  #                                                costTracker = costTracker)
  #
  # uploadPhenotypeMetaData(phenotypeMetaData = phenotypeMetaData,
  #                         pegasusConnectionDetails = pegasusConnectionDetails,
  #                         pegasusDatabaseSchema = pegasusDatabaseSchema)
  #
  #
  # # Create concept sets ------------------------------------------------------------------------------------------------
  # conceptSetTargets <- generateConceptSetTargets(
  #   phenotypes = phenotypes,
  #   databaseReference = databaseReferences[[1]],
  #   llmClient = llmClient,
  #   costTracker = costTracker,
  #   cacheFolder =  file.path(rootWorkFolder, "cacheTargets")
  # )
  # readr::write_csv(conceptSetTargets, file.path(rootWorkFolder, "conceptSetTargets.csv"))
  #
  # uploadDraftConceptSetTargets(draftConceptSetTargets = conceptSetTargets,
  #                              pegasusConnectionDetails = pegasusConnectionDetails,
  #                              pegasusDatabaseSchema = pegasusDatabaseSchema)
  #
  #
  # # Create concept set target definitions ------------------------------------------------------------------------------
  # conceptSetTargetDefinitions <- generateOrReuseConceptSetTargetDefinitions(
  #   conceptSetTargets = conceptSetTargets,
  #   phenotypes = phenotypes,
  #   llmClient = llmClient,
  #   costTracker = costTracker,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema,
  #   cacheFolder = file.path(rootWorkFolder, "cacheTargetDefinitions")
  # )
  # readr::write_csv(conceptSetTargetDefinitions, file.path(rootWorkFolder, "conceptSetTargetDefinitions.csv"))
  #
  #
  # # Create concept sets ------------------------------------------------------------------------------------------------
  # conceptSets <- generateConceptSets(
  #   conceptSetTargetDefinitions = conceptSetTargetDefinitions,
  #   llmClient = llmClient,
  #   costTracker = costTracker,
  #   databaseReference = databaseReferences[[1]],
  #   cacheFolder = file.path(rootWorkFolder, "cacheConceptSets")
  # )
  # saveRDS(conceptSets, file.path(rootWorkFolder, "conceptSets.rds"))
  #
  # uploadConceptSets(conceptSets = conceptSets,
  #                   pegasusConnectionDetails = pegasusConnectionDetails,
  #                   pegasusDatabaseSchema = pegasusDatabaseSchema)
  #
  #
  # # Count persons with concept sets ------------------------------------------------------------------------------------
  # conceptSetPersonCounts <- createConceptSetCounts(
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema,
  #   databaseReferences = databaseReferences,
  #   maxCores = 4,
  #   cacheFolder = file.path(rootWorkFolder, "cacheConceptSetPersonCounts")
  # )
  # uploadConceptSetPersonCounts(conceptSetPersonCounts = conceptSetPersonCounts,
  #                              pegasusConnectionDetails = pegasusConnectionDetails,
  #                              pegasusDatabaseSchema = pegasusDatabaseSchema)
  #
  #
  # # Create Keeper reference cohorts ------------------------------------------------------------------------------------
  # status <- createKeeperReferenceCohorts(
  #   phenotypes = phenotypes,
  #   llmClient = llmClient,
  #   costTracker = costTracker,
  #   databaseReferences = databaseReferences,
  #   workFolder = file.path(rootWorkFolder, "cacheKeeperReference")
  # )
  #
  #
  # # Create cohort definitions using MonoFlow ---------------------------------------------------------------------------
  # cohortDefinitions <- generateCohortDefinitionsUsingMonoFlowAgent(
  #   phenotypes = phenotypes,
  #   llmClient = llmClientFrontier,
  #   costTracker = costTracker,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema,
  #   databaseReferences = databaseReferences,
  #   workFolder = file.path(rootWorkFolder, "cacheMonoFlow")
  # )
  #
  # cohortDefinitionsWithAtlasIds <- insertCohortDefinitionsInAtlas(
  #   cohortDefinitions = cohortDefinitions,
  #   baseUrl = baseUrl
  # )
  #
  # uploadCohortDefinitions(
  #   cohortDefinitions = cohortDefinitionsWithAtlasIds,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema
  # )
  #
  #
  # # Create cohort definitions using templates --------------------------------------------------------------------------
  # cohortDefinitions <- generateCohortDefinitionssUsingTemplates(
  #   phenotypes = phenotypes,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema,
  #   databaseReference = databaseReferences[[1]],
  #   workFolder = file.path(rootWorkFolder, "cacheTemplates")
  # )
  #
  # uploadCohortDefinitions(
  #   cohortDefinitions = cohortDefinitions,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema
  # )
  #
  #
  # # Evaluate cohort definitions ----------------------------------------------------------------------------------------
  # cohortDefinitionsEvaluations <-evaluateUsingKeeperReferenceCohorts(
  #   phenotypes = phenotypes,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema,
  #   databaseReferences = databaseReferences
  # )
  #
  #
  # # Flip draft and valid flags -----------------------------------------------------------------------------------------
  # updatePhenotypes(
  #   phenotypes = phenotypes,
  #   draft = 0,
  #   valid = 1,
  #   pegasusConnectionDetails = pegasusConnectionDetails,
  #   pegasusDatabaseSchema = pegasusDatabaseSchema,
  # )
  #
  #
  # delta <- Sys.time() - startTime
  # message(sprintf("Creating cohort defintions and evaluations took %0.2f %s and cost $%0.2f.",
  #                 delta,
  #                 attr(delta, "units"),
  #                 costTracker$amount - startCost))
  invisible(NULL)
}
