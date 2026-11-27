library(Pegasus)
library(dplyr)

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

pegasusConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("PEGASUS_DBMS"),
  server = Sys.getenv("PEGASUS_SERVER"),
  connectionString = Sys.getenv("PEGASUS_CONNECTION_STRING"),
  user = Sys.getenv("PEGASUS_DB_USER"),
  password = Sys.getenv("PEGASUS_DB_PASSWORD")
)

pegasusDatabaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
# connection <- DatabaseConnector::connect(connectionDetails)
databaseReference <- createDatabaseReference(
  databaseName = "Optum Clinformatics",
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4020",
  referenceCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4020",
  referenceCohortTable  = "reference_cohort",
  sensitiveCohortDatabaseSchema = "",
  sensitiveCohortTable = "",
  candidateCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4020",
  candidateCohortTable = "pegasus_cohort",
  workDatabaseSchema = "scratch.scratch_mschuemi",
  workCohortTable = "pegasus_agent_cohort",
  workConceptSetTable = "pegasus_agent_concept_set"
)

llmClientFrontier <- Helios::chat_jnj_bedrock(
  model = "global.anthropic.claude-opus-4-8",
  api_key = keyring::key_get("genai_bedrock_key")
)

folder <- "e:/temp/MonoFlow02"

definitions <- openxlsx::readWorkbook("../largescalephentest/ClinicalDefinitionLibrary564.xlsx")
phenotypeNames <- definitions |>
  pull(Phenotype)
cohortSizes <- readr::read_csv("../largescalephentest/Keeper/referenceCohortSizes.csv", show_col_types = FALSE)
toSkip  <- cohortSizes |>
  filter(countOptumClinformatics == 0) |>
  pull(phenotype)
toSkip <- c(toSkip, "Death")

phenotypes <- downloadPhenotypes(
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema
)
phenotypes  <- phenotypes |>
  filter(draft == 0,
         phenotypeName %in% phenotypeNames,
         !phenotypeName %in% toSkip)

# phenotype = phenotypes |> filter(phenotypeId == 12)
runMonoFlow <- function(phenotype,
                        llmClientFrontier,
                        pegasusConnectionDetails,
                        pegasusDatabaseSchema,
                        databaseReference,
                        folder) {
  message(sprintf("*** Creating cohort definition for %s ***", phenotype$phenotypeName))

  # Create separate work tables per thread to avoid collisions:
  threadNr <- ParallelLogger::getThreadNumber()
  threadDatabaseReference <- databaseReference
  threadDatabaseReference$workCohortTable <- paste(threadDatabaseReference$workCohortTable, threadNr, sep = "_")
  threadDatabaseReference$workConceptSetTable <- paste(threadDatabaseReference$workConceptSetTable, threadNr, sep = "_")

  cohortDefinition <- Pegasus::generateCohortDefinitionsUsingMonoFlowAgent(
    phenotypes = phenotype,
    llmClient = llmClientFrontier,
    pegasusConnectionDetails = pegasusConnectionDetails,
    pegasusDatabaseSchema = pegasusDatabaseSchema,
    databaseReferences = list(threadDatabaseReference),
    skipIfExists = FALSE,
    workFolder = folder
  )
  return(cohortDefinition)
}


ParallelLogger::addDefaultFileLogger(file.path(folder, "log.txt"), name = "MONOFLOW_LOGGER")
cluster <- ParallelLogger::makeCluster(3)
cohortDefinitions <- ParallelLogger::clusterApply(
  cluster = cluster,
  x = split(phenotypes, phenotypes$phenotypeId),
  fun = runMonoFlow,
  llmClientFrontier = llmClientFrontier,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  databaseReference = databaseReference,
  folder = folder
)
cohortDefinitions <- bind_rows(cohortDefinitions)
ParallelLogger::stopCluster(cluster)
ParallelLogger::unregisterLogger("MONOFLOW_LOGGER")

# If there were intermittent errors, run one last time with all:
cohortDefinitions <- Pegasus::generateCohortDefinitionsUsingMonoFlowAgent(
  phenotypes = phenotypes,
  llmClient = llmClientFrontier,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  databaseReferences = list(databaseReference),
  skipIfExists = FALSE,
  workFolder = folder
)

# We already have MonoFlow definitions for these phenotypes: make the names unique:
cohortDefinitions <- cohortDefinitions |>
  mutate(cohortDefinitionName = gsub("MonoFlow", "MonoFlow 0.2", cohortDefinitionName))

ROhdsiWebApi::authorizeWebApi(baseUrl = keyring::key_get("baseUrl"), authMethod = "windows")
cohortDefinitions <- insertCohortDefinitionsInAtlas(cohortDefinitions, baseUrl = keyring::key_get("baseUrl"))

uploadCohortDefinitions(
  cohortDefinitions = cohortDefinitions,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
)

evaluations <- evaluateUsingKeeperReferenceCohorts(
  phenotypes = phenotypes,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  databaseReferences = list(databaseReference)
)
uploadCohortDefinitionEvaluations(
  cohortDefinitionsEvaluations = evaluations,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema
)
