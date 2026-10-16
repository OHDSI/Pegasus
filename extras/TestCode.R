library(Pegasus)

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

pegasusConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)

pegasusDatabaseSchema <- "scratch.scratch_mschuemi"

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
createDataModel(connectionDetails = connectionDetails,
                databaseSchema = databaseSchema)

# Create concept set targets ---------------------------------
costTracker <- createCostTracker()
targets <- generateConceptSetTargets(
  phenotype = "Acute liver failure",
  clinicalDefinition = "Acute liver failure is a rare but life-threatening syndrome characterized by the rapid deterioration of hepatocellular function, manifesting as significant coagulopathy and hepatic encephalopathy of any grade, developing within 28 days of the onset of jaundice or initial hepatic symptoms in an individual without evidence of pre-existing chronic liver disease or cirrhosis. The syndrome arises from a direct, primary insult to hepatocytes â€” including viral, toxic, drug-induced, autoimmune, metabolic, or indeterminate causes â€” and is conceptually distinct from liver dysfunction occurring as a secondary consequence of hemodynamic compromise (e.g., ischemic hepatitis, shock liver), systemic sepsis, or passive hepatic congestion from right-sided heart failure, all of which are explicitly excluded.",
  connectionDetails = connectionDetails,
  vocabDatabaseSchema = cdmDatabaseSchema,
  llmClient = llmClient,
  costTracker = costTracker)
saveRDS(targets, "e:/temp/PegasusTargets.rds")
