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

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

databaseReference <- createDatabaseReference(
  databaseName = "Optum Clinformatics",
  connectionDetails = DatabaseConnector::createConnectionDetails(
    dbms = "spark",
    connectionString = keyring::key_get("databricksConnectionString"),
    user = "token",
    password = keyring::key_get("databricksToken")
  ),
  cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4020",
  referenceCohortDatabaseSchema = "scratch.scratch_mschuemi",
  referenceCohortTable  = "pegasus_reference_cohort",
  sensitiveCohortDatabaseSchema = "scratch.scratch_mschuemi",
  sensitiveCohortTable = "pegasus_sensitive_cohort",
  candidateCohortDatabaseSchema = "scratch.scratch_all",
  candidateCohortTable = "pegasus_cohort"
)


# llmClient <- ellmer::chat_lmstudio(
#   model = "qwen3.8-27b-splash"
# )

llmClient <- ellmer::chat_azure_openai(
  endpoint = keyring::key_get("genai_openai_endpoint"),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)

phenotypes <- tibble(
  phenotypeName = "Acute myocardial infarction",
  clinicalDefinition = "Acute myocardial infarction (AMI) is an acute ischemic event characterized by myocardial necrosis resulting from an abrupt and sustained reduction in coronary blood supply or a critical imbalance between myocardial oxygen supply and demand, leading to detectable cardiomyocyte death as evidenced by a rise and/or fall in cardiac biomarkers (particularly high-sensitivity troponin) with at least one value exceeding the 99th percentile upper reference limit, accompanied by at least one of the following: ischemic symptoms, new or presumed new ischemic ECG changes, development of pathological Q waves, or imaging evidence of new loss of viable myocardium or new regional wall motion abnormality consistent with an ischemic etiology. The phenotype encompasses all pathophysiological subtypes, including spontaneous atherothrombotic plaque rupture or erosion (Type 1), ischemia due to supply-demand mismatch from a non-coronary cause (Type 2), and procedure-related infarctions (Types 3â€“5), and captures both ST-elevation MI (STEMI) and non-ST-elevation MI (NSTEMI) presentations. Explicitly excluded are myocarditis and Takotsubo (stress) cardiomyopathy, which may produce troponin elevation and ischemic-like presentations through non-obstructive, non-ischemic mechanisms fundamentally distinct from coronary-driven myocardial necrosis."
)

# Test local definition refiner ----------------------------------------------------------------------------------------
refineDefinition("Acute liver injury", llmClient = llmClient)

# Create data model ----------------------------------------------------------------------------------------------------
createDataModel(pegasusConnectionDetails = pegasusConnectionDetails,
                pegasusDatabaseSchema = pegasusDatabaseSchema)

databaseMetaData <- getDatabaseMetaData(databaseReference)
uploadDatabaseMetaData(databaseMetaData = databaseMetaData,
                       pegasusConnectionDetails = pegasusConnectionDetails,
                       pegasusDatabaseSchema = pegasusDatabaseSchema)

# Phenotypes -----------------------------------------------------------------------------------------------------------
uploadPhenotypes(phenotypes = phenotypes,
                 pegasusConnectionDetails = pegasusConnectionDetails,
                 pegasusDatabaseSchema = pegasusDatabaseSchema)

phenotypeMetaData <- generatePhenotypeMetaData(phenotypes = phenotypes,
                                               llmClient = llmClient)

uploadPhenotypeMetaData(phenotypeMetaData = phenotypeMetaData,
                        pegasusConnectionDetails = pegasusConnectionDetails,
                        pegasusDatabaseSchema = pegasusDatabaseSchema)

# Create concept set targets -------------------------------------------------------------------------------------------
conceptSetTargets <- generateConceptSetTargets(
  phenotypes = phenotypes,
  databaseReference = databaseReference,
  llmClient = llmClient,
  cacheFolder = "e:/temp/PegasusTargetsCache"
)
saveRDS(conceptSetTargets, "e:/temp/PegasusTargets.rds")

uploadDraftConceptSetTargets(draftConceptSetTargets = conceptSetTargets,
                             pegasusConnectionDetails = pegasusConnectionDetails,
                             pegasusDatabaseSchema = pegasusDatabaseSchema)



# Create concept set target definitions --------------------------------------------------------------------------------
conceptSetTargets <- readRDS("e:/temp/PegasusTargets.rds")

conceptSetTargetDefinitions <- generateOrReuseConceptSetTargetDefinitions(
  conceptSetTargets = conceptSetTargets,
  phenotypes = phenotypes,
  llmClient = llmClient,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  cacheFolder = "e:/temp/pegasusConceptSetTargetDefinitionCache"
)
saveRDS(conceptSetTargetDefinitions, "e:/temp/PegasusTargetDefinitions.rds")

# Create concept sets --------------------------------------------------------------------------------------------------
conceptSetTargetDefinitions <- readRDS("e:/temp/PegasusTargetDefinitions.rds")

conceptSets <- generateConceptSets(
  conceptSetTargetDefinitions = conceptSetTargetDefinitions,
  llmClient = llmClient,
  databaseReference = databaseReference,
  cacheFolder = "e:/temp/pegasusConceptSetCache"
)
saveRDS(conceptSets, "e:/temp/PegasusConceptSets.rds")

conceptSetsWithIds <- uploadConceptSets(conceptSets = conceptSets,
                                        pegasusConnectionDetails = pegasusConnectionDetails,
                                        pegasusDatabaseSchema = pegasusDatabaseSchema)
saveRDS(conceptSetsWithIds, "e:/temp/PegasusConceptSetsWithIds.rds")

# Count persons with concept sets --------------------------------------------------------------------------------------
conceptSetsWithIds <- readRDS("e:/temp/PegasusConceptSetsWithIds.rds")
conceptSetPersonCounts <- createConceptSetCounts(conceptSets = conceptSetsWithIds,
                                                 databaseReference = databaseReference,
                                                 maxCores = 4,
                                                 cacheFolder = "e:/temp/pegasusConceptSetCountCache")
uploadConceptSetPersonCounts(conceptSetPersonCounts = conceptSetPersonCounts,
                             pegasusConnectionDetails = pegasusConnectionDetails,
                             pegasusDatabaseSchema = pegasusDatabaseSchema)


# Create Keeper reference cohorts --------------------------------------------------------------------------------------
status <- createKeeperReferenceCohorts(
  phenotypes = phenotypes,
  llmClient = llmClient,
  databaseReference = databaseReference,
  workFolder = "e:/temp/pegasusKeeperReferenceCache"
)



