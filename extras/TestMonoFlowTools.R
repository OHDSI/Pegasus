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
  referenceCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4155",
  referenceCohortTable  = "reference_cohort",
  sensitiveCohortDatabaseSchema = "",
  sensitiveCohortTable = "",
  candidateCohortDatabaseSchema = "",
  candidateCohortTable = "",
  workDatabaseSchema = "scratch.scratch_mschuemi",
  workCohortTable = "pegasus_agent_cohort_optum_extended_dod_v4155",
  workConceptSetTable = "pegasus_agent_concept_set_optum_extended_dod_v4155"
)

# phenotype <- "Acute myocardial infarction"
phenotype <- tibble(
  phenotypeId = 579,
  phenotypeName = "Radiation Induced Lung Toxicity",
  clinicalDefinition = ""
)

pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)

source("R/MonoFlowAgentTools.R")
context <- PegasusAgentContext$new(phenotype = phenotype,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
listConceptSets(context)
getConceptSetsCapr(context, conceptSetNames = c("Spirometry", "Acute pulmonary embolism"), detail = "code")
getConceptSetsCapr(context, conceptSetNames = c("Spirometry", "Acute pulmonary embolism"))
getConceptSetsCapr(context, conceptSetNames = c("Radiation Induced Lung Toxicity"), detail = "full_reference")
caprCohort <- "cohort(entry = entry(conditionOccurrence(cs(descendants(764873, 442310, exclude(36716182)), name = \"Acute heart failure\")), primaryCriteriaLimit = \"First\"), exit  = exit(endStrategy = observationExit()))"
caprConceptSet <- "cs(descendants(764873, 442310, exclude(36716182)), name = \"Acute heart failure\")"
caprMeasurementConceptSet <- "cs(4133840, descendants(759713, 2106190, 2314005, 2314007, 43533300, 43533319, 2106191, 2106212, 2314004, 2314006, 2314008, 2314009, 40756916), name = \"Spirometry\")"

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
               cohortId = 579)
samplePatientProfile(context = context,
                     cohortId = 1,
                     type = "FN")

context$close()

