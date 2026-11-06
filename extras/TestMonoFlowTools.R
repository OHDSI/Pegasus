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
  candidateCohortTable = "pegasus_cohort",
  workDatabaseSchema = "scratch.scratch_mschuemi",
  workCohortTable = "pegasus_agent_cohort",
  workConceptSetTable = "pegasus_agent_concept_set"
)

phenotype <- "Acute myocardial infarction"

source("R/MonoFlowAgentTools.R")
context <- PegasusAgentContext$new(pegasusConnectionDetails = pegasusConnectionDetails,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
listConceptSets(context, phenotype)
getConceptSetsCapr(context, phenotype, conceptSetNames = c("Acute heart failure", "Acute pulmonary embolism"))
getConceptSetsCapr(context, phenotype, conceptSetNames = c("Acute heart failure", "Acute pulmonary embolism"), detail = "code")
getConceptSetsCapr(context, phenotype, conceptSetNames = c("Acute heart failure", "Acute pulmonary embolism"), detail = "full_reference")
caprCohort <- "cohort(entry = entry(conditionOccurrence(cs(descendants(192671L), name = \"GI Bleed\")), primaryCriteriaLimit = \"First\"), exit  = exit(endStrategy = observationExit()))"
caprConceptSet <- "cs(descendants(192671L), name = \"GI Bleed\")"
caprMeasurementConceptSet <- "cs(descendants(3004501L), name = \"GI Bleed\")"

validateCapr(caprCohort)
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

