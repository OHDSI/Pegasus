library(Pegasus)
library(dplyr)
source("R/HelperFunctions.R")
source("R/KeeperReferenceCohorts.R")

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

pegasusConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("PEGASUS_DBMS"),
  server = Sys.getenv("PEGASUS_SERVER"),
  connectionString = Sys.getenv("PEGASUS_CONNECTION_STRING"),
  user = Sys.getenv("PEGASUS_DB_USER"),
  password = Sys.getenv("PEGASUS_DB_PASSWORD")
)

# pegasusDatabaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")
pegasusDatabaseSchema <- "pegasus_stage"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)

databaseReference <- createDatabaseReference(
  databaseName = "Optum Clinformatics",
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4155",
  referenceCohortDatabaseSchema = "scratch.scratch_mschuemi",
  referenceCohortTable  = "pegasus_reference_cohort_optum_extended_dod_v4155",
  sensitiveCohortDatabaseSchema = "scratch.scratch_mschuemi",
  sensitiveCohortTable = "pegasus_sensitive_cohort_optum_extended_dod_v4155",
  candidateCohortDatabaseSchema = "scratch.scratch_all",
  candidateCohortTable = "pegasus_cohort_optum_extended_dod_v4155",
  workDatabaseSchema = "scratch.scratch_mschuemi",
  workCohortTable = "pegasus_agent_cohort_optum_extended_dod_v4155",
  workConceptSetTable = "pegasus_agent_concept_set_optum_extended_dod_v4155"
)

# phenotype <- "Acute myocardial infarction"
phenotype = "Radiation Induced Lung Toxicity"

pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)

source("R/MonoFlowAgentTools.R")
context <- PegasusAgentContext$new(pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
listConceptSets(context, phenotype)
getConceptSetsCapr(context, phenotype, conceptSetNames = c("Acute heart failure", "Acute pulmonary embolism"))
getConceptSetsCapr(context, phenotype, conceptSetNames = c("Radiation Induced Lung Toxicity"), detail = "full_reference")

getConceptSetsCapr(context, phenotype, conceptSetNames = c("Acute heart failure", "Acute pulmonary embolism"), detail = "code")
getConceptSetsCapr(context, phenotype, conceptSetNames = c("Acute heart failure", "Acute pulmonary embolism"), detail = "full_reference")
caprCohort <- "cohort(entry = entry(conditionOccurrence(cs(descendants(764873, 442310, exclude(36716182)), name = \"Acute heart failure\")), primaryCriteriaLimit = \"First\"), exit  = exit(endStrategy = observationExit()))"
caprConceptSet <- "cs(descendants(764873, 442310, exclude(36716182)), name = \"Acute heart failure\")"
caprMeasurementConceptSet <- "cs(descendants(4208414, exclude(36315486, 36315484, 36315487, 1552093, 1552133, 1988606, 3000965, 3034254, 3035320, 3044417, 36315485, 40762274)), name = \"C-reactive protein measurement\")"

validateCapr(context, caprCohort)
convertCaprToJson(context, caprCohort)
generateCohort(context, caprCohort)
getDatabaseDescription(context)
countConceptSetPersonOverlap(context = context,
                             caprCode = caprConceptSet,
                             cohortId = 1)
describeMeasurementValues(context = context,
                          caprCode = caprMeasurementConceptSet)
evaluateCohort(context = context,
               cohortId = 1,
               phenotype = phenotype)
samplePatientProfile(context = context,
                     cohortId = 1,
                     phenotype = phenotype,
                     type = "FP")

context$close()

