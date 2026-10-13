library(Pegasus)

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)

databaseSchema <- "scratch.scratch_mschuemi"

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
