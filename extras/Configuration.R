# Set the database connections and schemas in our environment
library(dplyr)
library(Pegasus)

# Connection to the pegasus database and schema ------------------------------------------------------------------------
# This is where the Pegasus data model with all the results live
pegasusConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("PEGASUS_DBMS"),
  server = Sys.getenv("PEGASUS_SERVER"),
  connectionString = Sys.getenv("PEGASUS_CONNECTION_STRING"),
  user = Sys.getenv("PEGASUS_DB_USER"),
  password = Sys.getenv("PEGASUS_DB_PASSWORD")
)

# pegasusDatabaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")
pegasusDatabaseSchema <- "pegasus_stage"

options(sqlRenderTempEmulationSchema = NULL) # For the Pegasus connection

# Connection to the CDM database(s) ------------------------------------------------------------------------------------
# These are used to generate the results
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)

cdmDatabaseSchemas <- c("optum_extended_dod.cdm_optum_extended_dod_v4020",
                        "optum_extended_dod.cdm_optum_extended_dod_v4155",
                        "optum_ehr.cdm_optum_ehr_v4154",
                        "jmdc.cdm_jmdc_v4152",
                        "merative_mdcr.cdm_merative_mdcr_v4151")
databaseTails <- gsub("^.*cdm_", "", cdmDatabaseSchemas)
databaseNames <- c("OPTUM Extended DOD",
                   "OPTUM Extended DOD",
                   "Optum EHR",
                   "JMDC",
                   "Merative MDCR")

# Some code to automatically get the database names. Was used to create the statement above:
# connection <- DatabaseConnector::connect(connectionDetails)
# getDatabaseName <- function(cdmDatabaseSchema) {
#   sql <- "
#     SELECT TOP 1 cdm_source_abbreviation
#     FROM @cdm.cdm_source;
#   "
#   databaseName <- DatabaseConnector::renderTranslateQuerySql(connection, sql, cdm = cdmDatabaseSchema)[1, 1]
#   return(databaseName)
# }
# databaseNames <- sapply(cdmDatabaseSchemas, getDatabaseName)
# DatabaseConnector::disconnect(connection)
# writeLines(sprintf("databaseNames <- c(\"%s\")", paste(databaseNames, collapse = "\",\n\"")))


databaseReferences <- list()
for (i in seq_along(cdmDatabaseSchemas)) {
  # Note: work tables are not intended to be shared. This is where agents write their work in progress
  databaseReferences[[i]] <- createDatabaseReference(
    databaseName = databaseNames[i],
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchemas[i],
    tempEmulationSchema = "pegasus.temp_emulation",
    referenceCohortDatabaseSchema = paste0("pegasus.pegasus_", databaseTails[i]),
    referenceCohortTable  = "reference_cohort",
    sensitiveCohortDatabaseSchema = paste0("pegasus.pegasus_", databaseTails[i]),
    sensitiveCohortTable = "pegasus_sensitive_cohort_optum_extended_dod_v4155",
    candidateCohortDatabaseSchema = paste0("pegasus.pegasus_", databaseTails[i]),
    candidateCohortTable = "pegasus_cohort_optum_extended_dod_v4155",
    workDatabaseSchema = paste0("pegasus.pegasus_", databaseTails[i]),
    workCohortTable = "pegasus_agent_cohort",
    workConceptSetTable = "pegasus_agent_concept_set_"
  )
}

# Connections to LLMS --------------------------------------------------------------------------------------------------
# Used for various processing steps
llmClient <- ellmer::chat_azure_openai(
  endpoint = keyring::key_get("genai_openai_endpoint"),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

llmClientFrontier <- Helios::chat_jnj_bedrock(
  model = "global.anthropic.claude-opus-4-8",
  api_key = keyring::key_get("genai_bedrock_key")
)

# Connection to a WebApi -----------------------------------------------------------------------------------------------
# Used to upload cohort definitions
baseUrl <- keyring::key_get("baseUrl")

# Local work folder ----------------------------------------------------------------------------------------------------
# Where various caching takes place
rootWorkFolder <- "e:/Pegasus"

# Maximum number of cores ----------------------------------------------------------------------------------------------
maxCores <- 4

pegasusConfig <- createPegasusConfig (
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  databaseReferences = databaseReferences,
  llmClient = llmClient,
  llmClientFrontier = llmClientFrontier,
  baseUrl = baseUrl,
  rootWorkFolder = rootWorkFolder,
  maxCores = maxCores
)

