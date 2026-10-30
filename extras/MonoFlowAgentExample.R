# Example of setting up the Pegasus agent tools on an ellmer chat object, using Optum databases on Databricks.
library(Pegasus)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken"),
  pathToDriver = "c:/Users/admin_mschuemi/jdbcDrivers" # .renviron is not read when running in VSCode
)

databases <- tibble(
  databaseName = c("Optum Clinformatics",
                   "Optum EHR"),
  databaseDescription = c("Medical claims, pharmacy claims, lab test results, inpatient, and provider data. It includes electronic health data for over 126 million patients across the USA.",
                          "Longitudinal inpatient/outpatient data from dozens of US healthcare provider organizations (OMOP CDM). Contains standard domains (demographics, dx, px, meds, labs, vitals, allergies) plus NLP-extracted facts from clinical notes (symptoms, family history, disease scores, genetics, and prescribing rationale)."),
  cdmDatabaseSchema = c("optum_extended_dod.cdm_optum_extended_dod_v4020",
                        "optum_ehr.cdm_optum_ehr_v4154"),
  referenceCohortDatabaseSchema = c("scratch.scratch_all",
                                    "scratch.scratch_all"),
  referenceCohortTable = c("reference_cohort_optum_extended_dod_v4020",
                           "reference_cohort_optum_ehr_v4154"),
  referenceCohortProfilesTable = c("reference_cohort_profiles_optum_extended_dod_v4020",
                                   "reference_cohort_profiles_optum_ehr_v4154"),
  cohortDatabaseSchema = c("scratch.scratch_mschuemi",
                           "scratch.scratch_mschuemi"),
  cohortTable = c("agent_test_cohort_optum_clinformatics",
                  "agent_test_cohort_optum_ehr"),
  conceptSetTable = c("agent_test_concept_set_optum_clinformatics",
                      "agent_test_concept_set_optum_ehr"),
  conceptSetDefinitionTable = c("agent_test_concept_set_definition_optum_clinformatics",
                                "agent_test_concept_set_definition_optum_ehr")
)

context <- PegasusAgentContext$new(
  connectionDetails = connectionDetails,
  databases = databases,
  conceptSetDatabaseSchema = "scratch.scratch_all",
  tempEmulationSchema = "scratch.scratch_mschuemi"
)

chat <- ellmer::chat_azure_openai(
  endpoint = keyring::key_get("genai_openai_endpoint"),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
chat$set_tools(context$tools())

chat$chat("Create a cohort definition for asthma and report its size in each database.")

context$close()
