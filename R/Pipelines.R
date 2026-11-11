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

#' Create cohort definitions and evaluate them
#'
#' @template Phenotypes
#' @template LlmClient
#' @param llmClientFrontier
#' @template CostTracker
#' @param pegasusConnectionDetails An R object of type `connectionDetails` created using the
#'                                 [DatabaseConnector::createConnectionDetails()] function for connecting to the
#'                                 database holding the Pegasus data model.
#' @param pegasusDatabaseSchema    The name of the database schema holding the Pegasus data model
#' @template DatabaseReferences
#' @template MaxCores
#' @param baseUrl The base URL of the OHDSI Web API.
#' @param rootWorkFolder A folder on the local file system where results can be written.
#'
#' @returns
#' Does not return anything. Is executed for populated the Pegasus data model.
#'
#' @export
createCohortDefinitionsAndEvaluate <- function(phenotypes,
                                               llmClient,
                                               llmClientFrontier = llmClient,
                                               costTracker = createCostTracker(),
                                               pegasusConnectionDetails,
                                               pegasusDatabaseSchema,
                                               databaseReferences,
                                               maxCores = 4,
                                               baseUrl,
                                               rootWorkFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNames(colnames(phenotypes), must.include = c(
    "phenotypeName",
    "clinicalDefinition"
  ), add = errorMessages)
  checkmate::assertR6(llmClient, "Chat", add = errorMessages)
  checkmate::assertR6(llmClientFrontier, "Chat", add = errorMessages)
  checkmate::assertClass(costTracker, "CostTracker", add = errorMessages)
  checkmate::assertClass(pegasusConnectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(pegasusDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(databaseReferences, types = c("DatabaseReference"), min.len = 1, add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::assertCharacter(baseUrl, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  startTime <- Sys.time()
  startCost <- costTracker$amount

  ParallelLogger::addDefaultFileLogger(file.path(rootWorkFolder, "log.txt"), name = "COHORT_DEF_AND_EVAL_LOGGER")
  on.exit(ParallelLogger::unregisterLogger("COHORT_DEF_AND_EVAL_LOGGER"))

  dir.create(rootWorkFolder, recursive = TRUE, showWarnings = FALSE)

  # Phenotype meta data ------------------------------------------------------------------------------------------------
  phenotypeMetaData <- generatePhenotypeMetaData(phenotypes = phenotypes,
                                                 llmClient = llmClient,
                                                 costTracker = costTracker)

  uploadPhenotypeMetaData(phenotypeMetaData = phenotypeMetaData,
                          pegasusConnectionDetails = pegasusConnectionDetails,
                          pegasusDatabaseSchema = pegasusDatabaseSchema)


  # Create concept sets ------------------------------------------------------------------------------------------------
  conceptSetTargets <- generateConceptSetTargets(
    phenotypes = phenotypes,
    databaseReference = databaseReferences[[1]],
    llmClient = llmClient,
    costTracker = costTracker,
    cacheFolder =  file.path(rootWorkFolder, "cacheTargets")
  )
  readr::write_csv(conceptSetTargets, file.path(rootWorkFolder, "conceptSetTargets.csv"))

  uploadDraftConceptSetTargets(draftConceptSetTargets = conceptSetTargets,
                               pegasusConnectionDetails = pegasusConnectionDetails,
                               pegasusDatabaseSchema = pegasusDatabaseSchema)


  # Create concept set target definitions ------------------------------------------------------------------------------
  conceptSetTargetDefinitions <- generateOrReuseConceptSetTargetDefinitions(
    conceptSetTargets = conceptSetTargets,
    phenotypes = phenotypes,
    llmClient = llmClient,
    costTracker = costTracker,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    cacheFolder = file.path(rootWorkFolder, "cacheTargetDefinitions")
  )
  readr::write_csv(conceptSetTargetDefinitions, file.path(rootWorkFolder, "conceptSetTargetDefinitions.csv"))


  # Create concept sets ------------------------------------------------------------------------------------------------
  conceptSets <- generateConceptSets(
    conceptSetTargetDefinitions = conceptSetTargetDefinitions,
    llmClient = llmClient,
    costTracker = costTracker,
    databaseReference = databaseReferences[[1]],
    cacheFolder = file.path(rootWorkFolder, "cacheConceptSets")
  )
  saveRDS(conceptSets, file.path(rootWorkFolder, "conceptSets.rds"))

  uploadConceptSets(conceptSets = conceptSets,
                    pegasusConnectionDetails = pegasusConnectionDetails,
                    pegasusDatabaseSchema = pegasusDatabaseSchema)


  # Count persons with concept sets ------------------------------------------------------------------------------------
  conceptSetPersonCounts <- createConceptSetCounts(
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    databaseReferences = databaseReferences,
    maxCores = 4,
    cacheFolder = file.path(rootWorkFolder, "cacheConceptSetPersonCounts")
  )
  uploadConceptSetPersonCounts(conceptSetPersonCounts = conceptSetPersonCounts,
                               pegasusConnectionDetails = pegasusConnectionDetails,
                               pegasusDatabaseSchema = pegasusDatabaseSchema)


  # Create Keeper reference cohorts ------------------------------------------------------------------------------------
  status <- createKeeperReferenceCohorts(
    phenotypes = phenotypes,
    llmClient = llmClient,
    costTracker = costTracker,
    databaseReferences = databaseReferences,
    workFolder = file.path(rootWorkFolder, "cacheKeeperReference")
  )


  # Create cohort definitions using MonoFlow ---------------------------------------------------------------------------
  cohortDefinitions <- generateCohortDefinitionsUsingMonoFlowAgent(
    phenotypes = phenotypes,
    llmClient = llmClientFrontier,
    costTracker = costTracker,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    databaseReferences = ldatabaseReferences,
    workFolder = file.path(rootWorkFolder, "cacheMonoFlow")
  )

  cohortDefinitionsWithAtlasIds <- insertCohortDefinitionsInAtlas(
    cohortDefinitions = cohortDefinitions,
    baseUrl = baseUrl
  )

  uploadCohortDefinitions(
    cohortDefinitions = cohortDefinitionsWithAtlasIds,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )


  # Create cohort definitions using templates --------------------------------------------------------------------------
  cohortDefinitions <- generateCohortDefinitionssUsingTemplates(
    phenotypes = phenotypes,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    databaseReference = databaseReferences[[1]],
    workFolder = file.path(rootWorkFolder, "cacheTemplates")
  )

  uploadCohortDefinitions(
    cohortDefinitions = cohortDefinitions,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema
  )


  # Evaluate cohort definitions ----------------------------------------------------------------------------------------
  cohortDefinitionsEvaluations <-evaluateUsingKeeperReferenceCohorts(
    phenotypes = phenotypes,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    databaseReferences = databaseReferences
  )


  # Flip draft and valid flags -----------------------------------------------------------------------------------------
  updatePhenotypes(
    phenotypes = phenotypes,
    draft = 0,
    valid = 1,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
  )


  delta <- Sys.time() - startTime
  message(sprintf("Creating cohort defintions and evaluations took %0.2f %s and cost $%0.2f.",
                  delta,
                  attr(delta, "units"),
                  costTracker$amount - startCost))
  invisible(NULL)
}
