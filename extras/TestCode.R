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

# pegasusDatabaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")
pegasusDatabaseSchema <- "pegasus_backend_test"

cdmConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "optum_extended_dod.cdm_optum_extended_dod_v4020"

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

# llmClient <- ellmer::chat_lmstudio(
#   model = "qwen3.8-27b-splash"
# )

llmClient <- ellmer::chat_azure_openai(
  endpoint = keyring::key_get("genai_openai_endpoint"),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

# Test local definition refiner ---------------------------------------------------------------------
refineDefinition("Acute liver injury", llmClient = llmClient)

# Create data model --------------------------------------------------------
createDataModel(pegasusConnectionDetails = pegasusConnectionDetails,
                pegasusDatabaseSchema = pegasusDatabaseSchema)

# Create concept set targets ---------------------------------
phenotypes <- tibble(
  phenotypeName = "Acute liver failure",
  clinicalDefinition = "Acute liver failure is a rare but life-threatening syndrome characterized by the rapid deterioration of hepatocellular function, manifesting as significant coagulopathy and hepatic encephalopathy of any grade, developing within 28 days of the onset of jaundice or initial hepatic symptoms in an individual without evidence of pre-existing chronic liver disease or cirrhosis. The syndrome arises from a direct, primary insult to hepatocytes â€” including viral, toxic, drug-induced, autoimmune, metabolic, or indeterminate causes â€” and is conceptually distinct from liver dysfunction occurring as a secondary consequence of hemodynamic compromise (e.g., ischemic hepatitis, shock liver), systemic sepsis, or passive hepatic congestion from right-sided heart failure, all of which are explicitly excluded."
)

targets <- generateConceptSetTargets(
  phenotypeName = phenotypes$phenotypeName[1],
  clinicalDefinition = phenotypes$clinicalDefinition[1],
  connectionDetails = connectionDetails,
  vocabDatabaseSchema = cdmDatabaseSchema,
  llmClient = llmClient)
saveRDS(targets, "e:/temp/PegasusTargets.rds")

# Create concept set target definitions --------------------------------
conceptSetTargets <- readRDS("e:/temp/PegasusTargets.rds")

conceptSetTargetDefinitions <- generateOrReuseConceptSetTargetDefinitions(
  conceptSetTargets = conceptSetTargets,
  phenotypes = phenotypes,
  llmClient = llmClient,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  cacheFolder = "e:/temp/pegasusConceptSetTargetCache")
saveRDS(conceptSetTargetDefinitions, "e:/temp/PegasusTargetDefinitions.rds")

# Create concept sets -------------------------------------------------
conceptSetTargetDefinitions <- readRDS("e:/temp/PegasusTargetDefinitions.rds")

conceptSets <- generateConceptSets(
  conceptSetTargetDefinitions = conceptSetTargetDefinitions,
  llmClient = llmClient,
  connectionDetails = cdmConnectionDetails,
  vocabDatabaseSchema = cdmDatabaseSchema,
  cacheFolder = "e:/temp/pegasusConceptSetCache")

saveRDS(conceptSets, "e:/temp/PegasusConceptSets.rds")

# Upload concept set data -------------------------------------------
conceptSets <- readRDS("e:/temp/PegasusConceptSets.rds")

uploadPhenotypes(phenotypes = phenotypes,
                 pegasusConnectionDetails = pegasusConnectionDetails,
                 pegasusDatabaseSchema = pegasusDatabaseSchema)

# downloadPhenotypes(pegasusConnectionDetails = pegasusConnectionDetails,
#                    pegasusDatabaseSchema = pegasusDatabaseSchema)

uploadDraftConceptSetTargets(draftConceptSetTargets = conceptSetTargets,
                             pegasusConnectionDetails = pegasusConnectionDetails,
                             pegasusDatabaseSchema = pegasusDatabaseSchema)



