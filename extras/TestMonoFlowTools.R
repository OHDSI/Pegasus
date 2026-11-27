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

pegasusDatabaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)

# databaseReference <- createDatabaseReference(
#   databaseName = "Optum Clinformatics",
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4155",
#   referenceCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4155",
#   referenceCohortTable  = "reference_cohort",
#   sensitiveCohortDatabaseSchema = "",
#   sensitiveCohortTable = "",
#   candidateCohortDatabaseSchema = "",
#   candidateCohortTable = "",
#   workDatabaseSchema = "scratch.scratch_mschuemi",
#   workCohortTable = "pegasus_agent_cohort_optum_extended_dod_v4155",
#   workConceptSetTable = "pegasus_agent_concept_set_optum_extended_dod_v4155"
# )

databaseReference <- createDatabaseReference(
  databaseName = "Optum Clinformatics",
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4020",
  referenceCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4020",
  referenceCohortTable  = "reference_cohort",
  sensitiveCohortDatabaseSchema = "",
  sensitiveCohortTable = "",
  candidateCohortDatabaseSchema = "",
  candidateCohortTable = "",
  workDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4020",
  workCohortTable = "pegasus_agent_cohort",
  workConceptSetTable = "pegasus_agent_concept_set"
)
# conn <- DatabaseConnector::connect(databaseReference$connectionDetails)
# sql <- "DROP TABLE @schema.@table; DROP TABLE @schema.@table_2; DROP TABLE @schema.@table_2_definition;"
# DatabaseConnector::renderTranslateExecuteSql(connection = conn, sql = sql, schema = databaseReference$workDatabaseSchema, table = databaseReference$workCohortTable, table_2 = databaseReference$workConceptSetTable)
# DatabaseConnector::disconnect(conn)

# phenotype <- "Acute myocardial infarction"
phenotype <- tibble(
  phenotypeId = 421,
  phenotypeName = "Pneumonia",
  clinicalDefinition = "Pneumonia is an acute infectious inflammatory process of the lung parenchyma, characterized by the simultaneous presence of a new pulmonary infiltrate on imaging and clinical signs of infection, such as fever, productive cough, or leukocytosis. The phenotype spans all severity levels, from mild presentations amenable to outpatient management to severe cases requiring intensive care and mechanical ventilatory support. An infectious etiology must be the established clinical working diagnosis, although microbiological identification of a specific causative organism is not required. The concept encompasses pneumonia arising from any predisposing mechanism or host context, including aspiration pneumonia, post-obstructive pneumonia distal to structural lesions, and pneumonia occurring in immunocompromised individuals. Explicitly excluded are non-infectious mimics: cardiogenic pulmonary infiltrates secondary to hemodynamic causes such as congestive heart failure, and pulmonary infarction arising from pulmonary embolism, both of which may produce radiographic opacities and systemic inflammatory signs but do not constitute primary infectious parenchymal inflammation."
)

pegasusConnection <- DatabaseConnector::connect(pegasusConnectionDetails)

source("R/MonoFlowAgentTools.R")
context <- PegasusAgentContext$new(phenotype = phenotype,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
writeLines(Pegasus:::listConceptSets(context))
getConceptSetsCapr(context, conceptSetNames = c("Hemoglobin A1c measurement", "Acute pulmonary embolism"), detail = "code")
getConceptSetsCapr(context, conceptSetNames = c("Spirometry", "Acute pulmonary embolism"))
getConceptSetsCapr(context, conceptSetNames = c("Radiation Induced Lung Toxicity"), detail = "full_reference")
caprCohort <- "cohort(\n  entry = entry(\n    conditionOccurrence(cs(438350, 3663281, 438967, 4142174, 4327948, descendants(759817, 254561, 4033925, 45757664, 260026, 4049797, 759818, 2617502, 4138244, 4345215, 4345699, 38001039, 38001040, 38001041, 255848, 4270197, 443741, 37017248, exclude(37118654, 4096917, 1340380, 4014884, 4080753, 36110591, 36148515, 37161105, 37169818)), exclude(4245409, 260034), name = \"Pneumonia\")),\n    observationWindow = continuousObservation(0L, 0L),\n    primaryCriteriaLimit = \"All\"\n  ),\n  attrition = attrition(\n    \"Confirmed by imaging, acute care, or antibiotics\" = withAny(\n      atLeast(1, procedure(cs(4163872, 36713260, 2006695, 1073000, 1073002, descendants(2792810, 3033135, 3047860, 42529152, 1072997, 1072998, 724942, 724943, 725068, 725069, 1072996, 2006700, 2211365, 2211367, 4122945, 4197182, 36713182, 36713195, exclude(2211364)), name = \"Plain X-ray of chest\")), duringInterval(eventStarts(-7, 7))),\n      atLeast(1, procedure(cs(4058335, 4327032, 4145380, 36713200, 36715386, 1074449, 37395895, 37397392, descendants(4117762, 37017298, 2211378, 2211380, 2785892, 2787120, 2787121, 2787123, 2787124, 2787125, 2787126, 2787127, 2787128, 2787682, 2787683, 2787684, 2787685, 2787686, 2787689, 2787690, 2787691, 2787692, 2787693, 2787694, 2787696, 2787697, 2787698, 2787699, 2793846, 2793847, 2793848, 4120673, 36713048, 37111353, 37209019, 37396021, 40489438, 44797583, 44808380, exclude(4083211, 4234782, 44813844, 44806015, 40480138, 42535315, 1074474, 1243854, 2785837, 2785891, 2786144, 2786152, 4082686, 4168976, 4234043, 37396813, 40479300, 40480584, 40481084, 40481451, 40491503)), name = \"CT of chest\")), duringInterval(eventStarts(-7, 7))),\n      atLeast(1, visit(cs(descendants(9201, 9203, 262), name = \"Inpatient or emergency room visit\")), duringInterval(eventStarts(-Inf, 0), eventEnds(0, Inf))),\n      atLeast(1, drugExposure(cs(descendants(1734104), name = \"azithromycin\")), duringInterval(eventStarts(-3, 7))),\n      atLeast(1, drugExposure(cs(descendants(1742253), name = \"levofloxacin\")), duringInterval(eventStarts(-3, 7))),\n      atLeast(1, drugExposure(cs(descendants(1777806), name = \"ceftriaxone\")), duringInterval(eventStarts(-3, 7))),\n      atLeast(1, drugExposure(cs(descendants(1716903), name = \"moxifloxacin\")), duringInterval(eventStarts(-3, 7)))\n    ),\n    expressionLimit = \"All\"\n  ),\n  exit = exit(endStrategy = fixedExit(index = \"startDate\", offsetDays = 1L)),\n  era = era(eraDays = 30L)\n)"
# caprConceptSet <- "cs(descendants(764873, 442310, exclude(36716182)), name = \"Acute heart failure\")"
caprConceptSet <- "cs(438350, 3663281, 438967, 4142174, 4327948, descendants(759817, 254561, 4033925, 45757664, 260026, 4049797, 759818, 2617502, 4138244, 4345215, 4345699, 38001039, 38001040, 38001041, 255848, 4270197, 443741, 37017248, exclude(37118654, 4096917, 1340380, 4014884, 4080753, 36110591, 36148515, 37161105, 37169818)), exclude(4245409, 260034), name = \"Pneumonia\")"

caprMeasurementConceptSet <- "cs(descendants(2617505, 3003309, 3004410, 3005673, 3007263, 40762352, 40765129, 42869630, 4184637, exclude(36314183, 36314182, 36314184, 36362381)), name = \"Hemoglobin A1c measurement\")"

validateCapr(context, caprCohort)
convertCaprToJson(context, caprCohort)
generateCohort(context, caprCohort)
getCohortCount(context, cohortId = 2)

getDatabaseDescription(context)
writeLines(countConceptSetPersonOverlap(context = context,
                             caprCode = caprConceptSet,
                             cohortId = 2))
describeMeasurementValues(context = context,
                          caprCode = caprMeasurementConceptSet)
evaluateCohort(context = context,
               cohortId = 1)
samplePatientProfile(context = context,
                     cohortId = 1,
                     type = "FN")
samplePatientProfile(context, cohortId = 8L, type = "FP", database = structure(1L, levels = "Optum Clinformatics", class = "factor"))
context$close()

# Example cohort definitions -------------------------------------------------------------------------------------------
# To be used in the system prompt


library(Capr)

# Tools -----------------------------------------------------------------------------------------------------------------
phenotype <- tibble(
  phenotypeId = 534,
  phenotypeName = "Type 2 Diabetes mellitus",
  clinicalDefinition = ""
)
context <- PegasusAgentContext$new(phenotype = phenotype,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
writeLines(getConceptSetsCapr(context, conceptSetNames = c("Type 2 diabetes mellitus", "Type 1 diabetes mellitus", "metformin", "Hemoglobin A1c measurement"), detail = "code"))
cs_t2dm <- cs(descendants(443732, 201826, 760989), name = "Type 2 diabetes mellitus")
cs_t1dm <- cs(descendants(42689695, 435216, 201254, exclude(4322638, 4102018, 36717215, 37163452, 37165632, 37396268)), name = "Type 1 diabetes mellitus")
cs_metformin <- cs(descendants(1503297), name = "metformin")
cs_hba1c <- cs(descendants(2617505, 3003309, 3004410, 3005673, 3007263, 40762352, 40765129, 42869630, 4184637, exclude(36314183, 36314182, 36314184, 36362381)), name = "Hemoglobin A1c measurement")

cs_t2dm <- getConceptSetDetails(cs_t2dm, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
cs_t1dm <- getConceptSetDetails(cs_t1dm, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
cs_metformin <- getConceptSetDetails(cs_metformin, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
cs_hba1c <- getConceptSetDetails(cs_hba1c, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)

cohort <- cohort(
  entry = entry(
    conditionOccurrence(cs_t2dm),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "All"
  ),
  attrition = attrition(
    "Adult males" = withAll(male(), age(gte(18L))),
    "Prior HbA1c < 5.7%" = withAll(
      atLeast(1,
              measurement(cs_hba1c, valueAsNumber(lt(5.7)), measurementUnit(8554L)), # %
              duringInterval(eventStarts(-Inf, -1))
      )
    ),
    "No prior T1DM" = withAll(
      exactly(0, conditionOccurrence(cs_t1dm), duringInterval(eventStarts(-Inf, 0)))
    ),
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = drugExit(cs_metformin, persistenceWindow = 30L, surveillanceWindow = 7L),
    censor = censoringEvents(death())
  ),
  era = era(eraDays = 0L)
)
# cohort <- getConceptSetDetails(cohort, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
writeLines(toCohortJson(cohort), "e:/temp/cohort.txt")
context$close()

# Acute recurrent event (Acute MI) -------------------------------------------------------------------------------------
phenotype <- tibble(
  phenotypeId = 16,
  phenotypeName = "acute myocardial infarction",
  clinicalDefinition = ""
)
context <- PegasusAgentContext$new(phenotype = phenotype,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
writeLines(getConceptSetsCapr(context, conceptSetNames = c("Acute myocardial infarction", "Inpatient or emergency room visit"), detail = "code"))
cs_ami <- cs(606159, descendants(438172, 609068, 609083, 2617498, 2617501, 4108220, 4108678, 4108680, 4179525, 4180609, 4198141, 4253065, 4322145, 36712985, 37109910, 37109911, 37109912, 38001108, 38001109, 38001110, 38001111, 38001112, 38001113, 40761359, 4329847, 4124687, exclude(314666, 766257, 4108679, 4170094)), name = "Acute myocardial infarction")
cs_inpatient_or_er <- cs(descendants(9201, 9203, 262), name = "Inpatient or emergency room visit")


cohort <- cohort(
  entry = entry(
    conditionOccurrence(cs_ami),
    primaryCriteriaLimit = "All",
    additionalCriteria = withAll(
      atLeast(1,
              visit(cs_inpatient_or_er),
              duringInterval(startWindow = eventStarts(-Inf, 0),
                             endWindow = eventEnds(0, Inf)))
    ),
    qualifiedLimit = "All"
  ),
  attrition = attrition(
    "No AMI in prior 30 days" = withAll(
      exactly(
        0,
        conditionOccurrence(cs_ami,
                            nestedWithAll(
                              atLeast(1,
                                      visit(cs_inpatient_or_er),
                                      duringInterval(startWindow = eventStarts(-Inf, 0),
                                                     endWindow = eventEnds(0, Inf)))
                            )
        ),
        duringInterval(eventStarts(-Inf, 0))
      )
    ),
    expressionLimit = "All"
  ),
  exit = exit(fixedExit(offsetDays  = 1L))
)
cohort <- getConceptSetDetails(cohort, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
writeLines(toCohortJson(cohort), "e:/temp/cohort.txt")
context$close()

# Chronic event: Major depressive disorder -----------------------------------------------------------------------------
phenotype <- tibble(
  phenotypeId = 344,
  phenotypeName = "Major Depressive Disorder",
  clinicalDefinition = ""
)
context <- PegasusAgentContext$new(phenotype = phenotype,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
writeLines(getConceptSetsCapr(context, conceptSetNames = c("Major depressive disorder", "Inpatient or emergency room visit", "Bipolar I disorder", "Bipolar II disorder"), detail = "code"))
cs_mdd <- cs(4114950, descendants(4152280, 2101900, 2106304, 2106305, 2106310, 2106322, 2106326, 2617865, 2617866, 40756912, 44786399, 44786400, 44786417, 44786437, 4098302, exclude(4226155)), name = "Major depressive disorder")
cs_inpatient_or_er <- cs(descendants(9201, 9203, 262), name = "Inpatient or emergency room visit")
cm_bipolar_1 <- cs(descendants(432876), name = "Bipolar I disorder")
cm_bipolar_2 <- cs(descendants(4307956), name = "Bipolar II disorder")

cohort <- cohort(
  entry = entry(
    conditionOccurrence(cs_mdd),
    primaryCriteriaLimit = "All",
    additionalCriteria = withAny(
      atLeast(1,
              conditionOccurrence(cs_mdd),
              duringInterval(eventStarts(1, 90, "startDate"))
      ),
      atLeast(1,
              visit(cs_inpatient_or_er),
              duringInterval(eventStarts(-Inf, 0, "startDate"), eventEnds(0, Inf, "startDate"))
      )
    ),
    qualifiedLimit = "First"
  ),
  attrition = attrition(
    "18 or older" = withAll(age(gte(18L))),
    "No prior bipolar" = withAll(
      exactly(0,
              conditionOccurrence(cm_bipolar_1),
              duringInterval(eventStarts(-Inf, 0))
      ),
      exactly(0,
              conditionOccurrence(cm_bipolar_2),
              duringInterval(eventStarts(-Inf, 0))
      )
    ),
    expressionLimit = "First"
  ),
  exit(endStrategy = observationExit(),
       censor = censoringEvents(conditionOccurrence(cm_bipolar_1),
                                conditionOccurrence(cm_bipolar_2)))
)
cohort <- getConceptSetDetails(cohort, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
writeLines(toCohortJson(cohort), "e:/temp/cohort.txt")
context$close()

# Recurrent era event: UTI
phenotype <- tibble(
  phenotypeId = 544,
  phenotypeName = "Urinary tract infection",
  clinicalDefinition = ""
)
context <- PegasusAgentContext$new(phenotype = phenotype,
                                   pegasusConnection = pegasusConnection,
                                   pegasusDatabaseSchema = pegasusDatabaseSchema,
                                   databaseReferences = list(databaseReference))
writeLines(getConceptSetsCapr(context, conceptSetNames = c("Urinary tract infection", "trimethoprim", "nitrofurantoin", "fosfomycin", "cephalexin", "cefuroxime", "cefpodoxime", "amoxicillin", "ciprofloxacin", "levofloxacin"), detail = "code"))

cs_uti <- cs(40481093, descendants(45757420, 4306541, 4308394, 36715430, 38001427, 38001428, 81902, 195588, 4098854, exclude(201353, 441615, 4128397, 4128399, 442345, 607157, 4009638, 4126268, 4126282, 4128384, 37311936, 199869, 200657, 201633, 201792, 606935, 765742, 1076104, 1076752, 4027122, 4032454, 4042397, 4060312, 4062493, 4089498, 4089973, 4127558, 4127709, 4128060, 4128374, 4128400, 4145196, 4196799, 4215879, 4216858, 4232758, 4248714, 4277601, 4327335, 4331584, 36009686, 37017217, 37165748, 37169806, 37169811, 37311937, 40493232, 45757237)), exclude(4032445, 4324904, 201621, 4227291), name = "Urinary tract infection")
cs_trimethoprim <- cs(descendants(1705674), name = "trimethoprim")
cs_fosfomycin <- cs(descendants(956653), name = "fosfomycin")
cs_amoxicillin <- cs(descendants(1713332), name = "amoxicillin")
cs_ciprofloxacin <- cs(descendants(1797513), name = "ciprofloxacin")


cohort <- cohort(
  entry = entry(
    conditionOccurrence(cs_uti),
    primaryCriteriaLimit = "All"
  ),
  attrition = attrition(
    "Treated with antibiotics" = withAny(
      atLeast(1, drugExposure(cs_trimethoprim), duringInterval(eventStarts(-Inf, 0, "startDate"), eventEnds(0, Inf, "startDate"))),
      atLeast(1, drugExposure(cs_fosfomycin), duringInterval(eventStarts(-Inf, 0, "startDate"), eventEnds(0, Inf, "startDate"))),
      atLeast(1, drugExposure(cs_amoxicillin), duringInterval(eventStarts(-Inf, 0, "startDate"), eventEnds(0, Inf, "startDate"))),
      atLeast(1, drugExposure(cs_ciprofloxacin), duringInterval(eventStarts(-Inf, 0, "startDate"), eventEnds(0, Inf, "startDate")))
    ),
    expressionLimit = "All"
  ),
  exit(fixedExit(offsetDays = 1L)),
  era = era(eraDays = 30L)
)
cohort <- getConceptSetDetails(cohort, con = context$connections[[1]], vocabularyDatabaseSchema = context$databaseReferences[[1]]$cdmDatabaseSchema)
writeLines(toCohortJson(cohort), "e:/temp/cohort.txt")
context$close()


# Run MonoFlow -------------------------------------
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

# databaseReference <- createDatabaseReference(
#   databaseName = "Optum Clinformatics",
#   connectionDetails = connectionDetails,
#   cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4155",
#   referenceCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4155",
#   referenceCohortTable  = "reference_cohort",
#   sensitiveCohortDatabaseSchema = "",
#   sensitiveCohortTable = "",
#   candidateCohortDatabaseSchema = "",
#   candidateCohortTable = "",
#   workDatabaseSchema = "scratch.scratch_mschuemi",
#   workCohortTable = "pegasus_agent_cohort_optum_extended_dod_v4155",
#   workConceptSetTable = "pegasus_agent_concept_set_optum_extended_dod_v4155"
# )

databaseReference <- createDatabaseReference(
  databaseName = "Optum Clinformatics",
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "optum_extended_dod.cdm_optum_extended_dod_v4020",
  referenceCohortDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4020",
  referenceCohortTable  = "reference_cohort",
  sensitiveCohortDatabaseSchema = "",
  sensitiveCohortTable = "",
  candidateCohortDatabaseSchema = "",
  candidateCohortTable = "",
  workDatabaseSchema = "pegasus.pegasus_optum_extended_dod_v4020",
  workCohortTable = "pegasus_agent_cohort",
  workConceptSetTable = "pegasus_agent_concept_set"
)

llmClientFrontier <- Helios::chat_jnj_bedrock(
  model = "global.anthropic.claude-opus-4-8",
  api_key = keyring::key_get("genai_bedrock_key")
)

phenotypes <- tibble(
  phenotypeId = 24,
  phenotypeName = "Age-related macular degeneration",
  clinicalDefinition = "Age-related macular degeneration (AMD) is a chronic, progressive, idiopathic degenerative disease of the central retina arising from age-driven deterioration of the retinal pigment epithelium, Bruch's membrane, and choriocapillaris, leading to dysfunction and loss of macular photoreceptors. The phenotype encompasses both the non-neovascular (dry/atrophic) form â€” characterized by drusen accumulation, retinal pigment epithelium atrophy, and in advanced cases geographic atrophy â€” and the neovascular (wet) form, characterized by pathological choroidal neovascularization through a disrupted Bruch's membrane, resulting in subretinal or intraretinal fluid, exudation, or hemorrhage. The definition spans the full disease continuum from early AMD (small to medium drusen, no visual impairment) through intermediate AMD (large drusen and/or pigmentary abnormalities) to late AMD (geographic atrophy or active neovascular disease). This phenotype is strictly restricted to idiopathic, age-driven pathophysiology and explicitly excludes inherited macular dystrophies (including Stargardt disease, Best vitelliform dystrophy, and other genetically determined retinal degenerations), as well as macular pathology secondary to a primary systemic or ocular condition such as diabetic macular edema, macular edema from retinal vein occlusion, or uveitis-associated macular disease."
)
phenotypes <- tibble(
  phenotypeId = 16,
  phenotypeName = "acute myocardial infarction",
  clinicalDefinition = "Acute myocardial infarction (AMI) is an acute ischemic event characterized by myocardial necrosis resulting from an abrupt and sustained reduction in coronary blood supply or a critical imbalance between myocardial oxygen supply and demand, leading to detectable cardiomyocyte death as evidenced by a rise and/or fall in cardiac biomarkers (particularly high-sensitivity troponin) with at least one value exceeding the 99th percentile upper reference limit, accompanied by at least one of the following: ischemic symptoms, new or presumed new ischemic ECG changes, development of pathological Q waves, or imaging evidence of new loss of viable myocardium or new regional wall motion abnormality consistent with an ischemic etiology. The phenotype encompasses all pathophysiological subtypes, including spontaneous atherothrombotic plaque rupture or erosion (Type 1), ischemia due to supply-demand mismatch from a non-coronary cause (Type 2), and procedure-related infarctions (Types 3â€“5), and captures both ST-elevation MI (STEMI) and non-ST-elevation MI (NSTEMI) presentations. Explicitly excluded are myocarditis and Takotsubo (stress) cardiomyopathy, which may produce troponin elevation and ischemic-like presentations through non-obstructive, non-ischemic mechanisms fundamentally distinct from coronary-driven myocardial necrosis."
)
phenotypes <- tibble(
  phenotypeId = 534,
  phenotypeName = "Type 2 Diabetes mellitus",
  clinicalDefinition = "Type 2 Diabetes Mellitus is a chronic, progressive metabolic disorder characterized by peripheral insulin resistance coupled with a relative, progressive decline in pancreatic beta-cell insulin secretory capacity, resulting in sustained impairment of glucose homeostasis and overt hyperglycemia. The condition arises from a polygenic predisposition interacting with environmental and lifestyle factors, and encompasses the full clinical spectrum from newly diagnosed, diet-controlled disease to advanced states requiring pharmacological or insulin-based management. It is distinguished from all forms of diabetes driven by absolute insulin deficiency or autoimmune beta-cell destruction, including Type 1 Diabetes Mellitus, Latent Autoimmune Diabetes in Adults (LADA), and monogenic diabetes (e.g., MODY). Also excluded are all forms of secondary diabetes attributable to a discrete structural or endocrine primary cause, including pancreatogenic diabetes, endocrinopathy-associated hyperglycemia (e.g., Cushing's syndrome, acromegaly), and drug- or toxin-induced glucose dysregulation. Gestational diabetes mellitus and transient stress hyperglycemia are likewise excluded as distinct, situationally determined states not reflecting the underlying chronic pathophysiological process."
)
phenotypes <- tibble(
  phenotypeId = 72,
  phenotypeName = "Behcet's syndrome",
  clinicalDefinition = "Behcet's syndrome is a systemic, recurrent inflammatory vasculitis of unknown etiology affecting vessels of all sizes, defined by the concurrent or sequential occurrence of the hallmark triad of recurrent oral aphthous ulcers, genital ulcers, and uveitis or other ocular inflammation. The diagnosis is established clinically on the basis of this triad, and the phenotype includes all confirmed cases regardless of current disease activity or severity. Explicitly excluded are conditions that mimic this triad through distinct pathophysiological mechanisms: isolated recurrent aphthous stomatitis without systemic involvement, herpes simplex virus (HSV)-driven mucocutaneous ulceration, oral and genital ulcers attributable to the gastrointestinal inflammation of inflammatory bowel disease, and reactive arthritis (Reiter's syndrome), which shares mucocutaneous features but arises from a post-infectious immune response rather than a primary vasculitic process."
)
phenotypes <- tibble(
  phenotypeId = 421,
  phenotypeName = "Pneumonia",
  clinicalDefinition = "Pneumonia is an acute infectious inflammatory process of the lung parenchyma, characterized by the simultaneous presence of a new pulmonary infiltrate on imaging and clinical signs of infection, such as fever, productive cough, or leukocytosis. The phenotype spans all severity levels, from mild presentations amenable to outpatient management to severe cases requiring intensive care and mechanical ventilatory support. An infectious etiology must be the established clinical working diagnosis, although microbiological identification of a specific causative organism is not required. The concept encompasses pneumonia arising from any predisposing mechanism or host context, including aspiration pneumonia, post-obstructive pneumonia distal to structural lesions, and pneumonia occurring in immunocompromised individuals. Explicitly excluded are non-infectious mimics: cardiogenic pulmonary infiltrates secondary to hemodynamic causes such as congestive heart failure, and pulmonary infarction arising from pulmonary embolism, both of which may produce radiographic opacities and systemic inflammatory signs but do not constitute primary infectious parenchymal inflammation."
)

# counts <- createConceptSetCounts(
#   phenotypes = phenotypes,
#   pegasusConnectionDetails = pegasusConnectionDetails,
#   pegasusDatabaseSchema = pegasusDatabaseSchema,
#   databaseReferences = list(databaseReference),
# )
# uploadConceptSetPersonCounts(
#   conceptSetPersonCounts = counts,
#   pegasusConnectionDetails = pegasusConnectionDetails,
#   pegasusDatabaseSchema = pegasusDatabaseSchema
# )

cd <- generateCohortDefinitionsUsingMonoFlowAgent(
  phenotypes = phenotypes,
  llmClient = llmClientFrontier,
  pegasusConnectionDetails = pegasusConnectionDetails,
  pegasusDatabaseSchema = pegasusDatabaseSchema,
  databaseReferences = list(databaseReference),
  skipIfExists = FALSE,
  workFolder = "e:/temp/MonoFlow02"
)
writeLines(cd$caprCohortExpression)
