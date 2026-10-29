library(Pegasus)
library(dplyr)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("PEGASUS_DBMS"),
  server = Sys.getenv("PEGASUS_SERVER"),
  connectionString = Sys.getenv("PEGASUS_CONNECTION_STRING"),
  user = Sys.getenv("PEGASUS_DB_USER"),
  password = Sys.getenv("PEGASUS_DB_PASSWORD")
)

databaseSchema <- Sys.getenv("PEGASUS_DATABASE_SCHEMA")

cdmConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchemas <- c("optum_extended_dod.cdm_optum_extended_dod_v4020",
                        "optum_ehr.cdm_optum_ehr_v4154")

options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

# Create data model ----------------------------------------------------------------------------------------------------
connection <- DatabaseConnector::connect(connectionDetails)
# DatabaseConnector::executeSql(connection, "CREATE SCHEMA pegasus;")

createDataModel(connection = connection,
                databaseSchema = databaseSchema)

# Upload phenotype -----------------------------------------------------------------------------------------
definitions <- openxlsx::readWorkbook("../largescalephentest/ClinicalDefinitionLibrary564.xlsx")
definitions <- definitions |>
  as_tibble() |>
  transmute(phenotypeId = row_number(),
            phenotypeName = Phenotype,
            clinicalDefinition = Definition,
            creator = "pryan4",
            timeCreated = as.POSIXct("2026-09-01"),
            draft = 0,
            valid = 1)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "phenotype",
  data = definitions,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload concept_set --------------------------------------------------------------------------------------------------
convertCirceConceptSetToCapr <- function(json, target) {
  expression <- CirceR::conceptSetExpressionFromJson(json)
  concepts <- c()
  excludedConcepts <- c()
  includeDescendantConcept <- c()
  includeDescendantAndExcludedConcept <- c()
  conceptIds <- c()
  conceptNames <- c()
  for (item in expression$items) {
    conceptId <- item$concept$conceptId$toString()
    conceptName <- item$concept$conceptName
    conceptIds <- c(conceptIds, conceptId)
    conceptNames <- c(conceptNames, conceptName)
    if (item$includeDescendants) {
      if (item$isExcluded) {
        includeDescendantAndExcludedConcept <- c(includeDescendantAndExcludedConcept, conceptId)
      } else {
        includeDescendantConcept <- c(includeDescendantConcept, conceptId)
      }
    } else {
      if (item$isExcluded) {
        excludedConcepts <- c(excludedConcepts, conceptId)
      } else {
        concepts <- c(concepts, conceptId)
      }
    }
  }
  hasIncludeDescendants <- length(includeDescendantConcept) > 0 || length(includeDescendantAndExcludedConcept) > 0
  code <- paste0("cs(",
                 if (length(concepts) > 0) paste(concepts, collapse = ", ") else "",
                 if (length(concepts) > 0 && hasIncludeDescendants) ", " else "",
                 if (hasIncludeDescendants) "descendants(" else "",
                 if (length(includeDescendantConcept) > 0) paste(includeDescendantConcept, collapse = ", ") else "",
                 if (length(includeDescendantConcept) > 0 && length(includeDescendantAndExcludedConcept) > 0) ", " else "",
                 if (length(includeDescendantAndExcludedConcept) > 0) paste0("exclude(", paste(includeDescendantAndExcludedConcept, collapse = ", "), ")") else "",
                 if (hasIncludeDescendants) ")" else "",
                 if ((length(concepts) > 0 || hasIncludeDescendants) && length(excludedConcepts) > 0) ", " else "",
                 if (length(excludedConcepts) > 0) paste0("exclude(", paste(excludedConcepts, collapse = ", "), ")") else "",
                 ", name = \"",
                 target,
                 "\")")
  return(code)
}

fullConceptSets <- readRDS("../largescalephentest/FullConceptSets.rds")
simpleConceptSets <- readRDS("../largescalephentest/SimpleConceptSets.rds")
conceptSets <- bind_rows(
  fullConceptSets |>
    transmute(conceptSetName,
              conceptId = conceptSetConceptId,
              hasDescendants = 1,
              circeConceptSetExpression = expression,
              clinicalDefinition = definition,
              csSourceType = source),
  simpleConceptSets |>
    transmute(conceptSetName = conceptName,
              conceptId = conceptId,
              hasDescendants = if_else(conceptSetClass == "hypernym", 0, 1),
              circeConceptSetExpression = expression,
              clinicalDefinition = NA_character_,
              csSourceType = if_else(conceptSetClass == "hypernym", "Concept", "Concept + descendants"))
) |>
  mutate(conceptSetId = row_number(),
         valid = 1,
         atlasConceptSetId = NA_integer_)

# Checking for and removing any duplicates
conceptSets |>
  arrange(conceptId) |>
  filter(duplicated(paste(hasDescendants, conceptSetName)))
# conceptSetName                hasDescendants
# Diagnostic pericardiocentesis              1
conceptSets <- conceptSets |>
  arrange(conceptId) |>
  filter(!duplicated(paste(hasDescendants, conceptSetName)))

sql <- "
  SELECT *
  FROM @database_schema.concept_set;
"
existingConceptSets <- DatabaseConnector::renderTranslateQuerySql(
  connection = connection,
  sql = sql,
  database_schema = databaseSchema,
  snakeCaseToCamelCase = TRUE
)
conceptSets <- conceptSets |>
  anti_join(existingConceptSets |>
            select("conceptSetName", "hasDescendants"),
            by = join_by("conceptSetName", "hasDescendants"))
conceptSets <- conceptSets |>
  mutate(conceptSetId = row_number() + max(existingConceptSets$conceptSetId))
sum(conceptSets$conceptSetId %in% existingConceptSets$conceptSetId)

conceptSets$caprConceptSetExpression <- mapply(convertCirceConceptSetToCapr,
                                               conceptSets$circeConceptSetExpression,
                                               conceptSets$conceptSetName)

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "concept_set",
  data = conceptSets,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)
conceptSets <- bind_rows(existingConceptSets, conceptSets)

# Upload phenotype_to_concept_set ------------------------------------------------------
phenotypeToConceptSet <- readRDS("../largescalephentest/phenotypeToConceptSetNameTable.rds")
phenotypeToConceptSet <- phenotypeToConceptSet |>
  inner_join(definitions |>
               select(phenotype = "phenotypeName", "phenotypeId"), by = join_by(phenotype)) |>
  inner_join(conceptSets |>
               mutate(hypernym = 1 - hasDescendants) |>
               select("hypernym", "conceptSetName", "conceptSetId"),
             by = join_by("hypernym", "conceptSetName")) |>
  transmute(phenotypeId,
            pertains = about,
            category = stringr::str_to_sentence(gsub("_", " ", category)),
            conceptSetId)
sql <- "
  SELECT *
  FROM @database_schema.phenotype_to_concept_set;
"
existingPhenotypeToConceptSet <- DatabaseConnector::renderTranslateQuerySql(
  connection = connection,
  sql = sql,
  database_schema = databaseSchema,
  snakeCaseToCamelCase = TRUE
)
phenotypeToConceptSet <- phenotypeToConceptSet |>
  anti_join(existingPhenotypeToConceptSet |>
              select("phenotypeId", "conceptSetId"),
            by = join_by("phenotypeId", "conceptSetId"))
# newPhenotypes <- phenotypeToConceptSet |>
#   distinct(phenotypeId) |>
#   inner_join(definitions)

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "phenotype_to_concept_set",
  data = phenotypeToConceptSet,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload cohort_definition ------------------------------------------------------------------------------
humanAndTemplatedCohortRef <- readr::read_csv("../largescalephentest/CohortReferenceTemplatesAndHuman.csv", show_col_types = FALSE)
monoFlowCohortRef <- readr::read_csv("../largescalephentest/CohortReferenceMonoFlow.csv", show_col_types = FALSE)
phenotypeDesignerCohortRef <- readr::read_csv("../largescalephentest/CohortReferencePhenotypeDesigner.csv", show_col_types = FALSE)

cohortRef <- bind_rows(
  humanAndTemplatedCohortRef,
  monoFlowCohortRef |>
    mutate(approach = "Mono flow"),
  phenotypeDesignerCohortRef |>
    mutate(approach = "PhenotypeDesigner")
)
baseUrl <- "https://epi.jnj.com:8443/WebAPI"
ROhdsiWebApi::authorizeWebApi(baseUrl, authMethod = "windows")
fromAtlas <- lapply(cohortRef$cohortId, ROhdsiWebApi::getCohortDefinition, baseUrl = baseUrl)
cohortDefinitions <- tibble(
  atlasCohortDefinitionId = sapply(fromAtlas, function(x) x$id),
  cohortDefinitionName = sapply(fromAtlas, function(x) x$name),
  circeCohortExpression = sapply(fromAtlas, function(x) RJSONIO::toJSON(x$expression, digits = 23, pretty = TRUE))
)
cohortDefinitions <- cohortDefinitions |>
  mutate(cohortDefinitionId = atlasCohortDefinitionId) |>
  inner_join(cohortRef |>
               select(atlasCohortDefinitionId = "cohortId", "approach", "phenotype"),
             by = join_by("atlasCohortDefinitionId")) |>
  inner_join(definitions |>
               select(phenotype = "phenotypeName", "phenotypeId"), by = join_by(phenotype)) |>
  transmute(cohortDefinitionId,
            cohortDefinitionName,
            phenotypeId,
            circeCohortExpression,
            cdSourceType = case_when(approach == "Mono flow" ~ "MonoFlow",
                                     approach == "Human" ~ "User",
                                     TRUE ~ approach),
            valid = 1,
            atlasCohortDefinitionId)

circeToCaprCohortDefinition <- function(json) {
  tempFile <- tempfile(fileext = ".json")
  on.exit(unlink(tempFile))
  writeLines(cohortDefinitions$circeCohortExpression[1], tempFile)
  capr <- Capr::jsonToCapr(tempFile)
  capr <- strsplit(capr, "\n")[[1]]
  capr <- capr[-(1:5)]
  capr <- paste(capr, collapse = "\n")
  return(capr)
}

cohortDefinitions$caprCohortExpression <- sapply(cohortDefinitions$circeCohortExpression,
                                                 circeToCaprCohortDefinition)

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "cohort_definition",
  data = cohortDefinitions,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload database_meta_data --------------------------------------
cdmConnection <- DatabaseConnector::connect(cdmConnectionDetails)
databaseMetaDatas <- list()
for (i in seq_along(cdmDatabaseSchemas)) {
  cdmSource <- DatabaseConnector::renderTranslateQuerySql(
    connection = cdmConnection,
    sql = "SELECT * FROM @cdm.cdm_source;",
    cdm = cdmDatabaseSchemas[i],
    snakeCaseToCamelCase = TRUE
  )
  databaseMetaDatas[[i]] <- cdmSource |>
    select("cdmSourceName",
           "cdmSourceAbbreviation",
           "cdmReleaseDate",
           "vocabularyVersion") |>
    mutate(databaseId = i,
           cdmDatabaseSchema = cdmDatabaseSchemas[i],
           resultDatabaseSchema = gsub("cdm_", "results_", cdmDatabaseSchemas[i]),
           cdmReleaseIdentifier = gsub(".*_v", "v", cdmDatabaseSchemas[i]))
}
DatabaseConnector::disconnect(cdmConnection)
databaseMetaDatas <- bind_rows(databaseMetaDatas)

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "database_meta_data",
  data = databaseMetaDatas,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload cohort_definition_evaluation --------------------------------------------
performance <- readRDS("../largescalephentest/PhenotypeEvaluation/Performance.rds")
# Warning: note hard-coded database IDs:
cdEvaluations <- performance |>
  transmute(cohortDefinitionId = cohortId,
            databaseId = if_else(database == "Optum Clinformatics", 1, 2),
            cohortEntries = if_else(is.na(cohortEntries), 0, cohortEntries),
            cohortSubjects = if_else(is.na(cohortSubjects), 0, cohortSubjects),
            generationSeconds,
            truePositives = tp,
            falsePositives = fp,
            trueNegatives = tn,
            falseNegatives = fn,
            ppv,
            ppvLb,
            ppvUb,
            sensitivity,
            sensitivityLb,
            sensitivityUb,
            specificity = specificityOverall,
            specificityLb = specificityOverallLb,
            specificityUb = specificityOverallUb,
            npv = npvOverall,
            npvLb = npvOverallLb,
            npvUb = npvOverallUb,
            evaluationType = "KEEPER + LLM")
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "cohort_definition_evaluation",
  data = cdEvaluations,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload atlas_instance ----------------------------------------------------------
atlasInstance <- tibble(baseUrl = "https://epi.jnj.com/atlas3")

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "atlas_instance",
  data = atlasInstance,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload concept_set_person_count ------------------------------------------------------
getCounts <- function(conceptSetSql, connection, cdmDatabaseSchema) {
  sql <- "
    DROP TABLE IF EXISTS #concept_set;

    SELECT cs.concept_id,
      concept.domain_id
    INTO #concept_set
    FROM (
      @concept_set_sql
    ) cs
    INNER JOIN @cdm_database_schema.concept
      ON cs.concept_id = concept.concept_id;
  "
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    concept_set_sql = SqlRender::render(conceptSetSql, vocabulary_database_schema = cdmDatabaseSchema),
    cdm_database_schema = cdmDatabaseSchema,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  sql <- "
    SELECT DISTINCT domain_id
    FROM #concept_set ;
  "
  domains <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE
  )[, 1]
  sql <- "
    WITH domain_persons AS (
    {@has_condition} ? {
      SELECT DISTINCT
        person_id,
        'Condition' AS domain_id
      FROM @cdm_database_schema.condition_occurrence
      WHERE condition_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    {@has_procedure} ? {
      {@has_condition} ? {
      UNION ALL
      }
      SELECT DISTINCT
        person_id,
        'Procedure' AS domain_id
      FROM @cdm_database_schema.procedure_occurrence
      WHERE procedure_concept_id IN (
       SELECT concept_id
       FROM #concept_set
      )
    }
    {@has_drug} ?{
      {@has_condition | @has_procedure} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
        'Drug' AS domain_id
      FROM @cdm_database_schema.drug_exposure
      WHERE drug_concept_id IN (
       SELECT concept_id
       FROM #concept_set
      )
    }
    {@has_device} ? {
      {@has_condition | @has_procedure | @has_drug} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
        'Device' AS domain_id
      FROM @cdm_database_schema.device_exposure
      WHERE device_concept_id IN (
       SELECT concept_id
       FROM #concept_set
      )
    }
    {@has_measurement} ? {
      {@has_condition | @has_procedure | @has_drug | @has_device} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
        'Measurement' AS domain_id
      FROM @cdm_database_schema.measurement
      WHERE measurement_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    {@has_observation} ? {
      {@has_condition | @has_procedure | @has_drug | @has_device | @has_measurement} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
       'Observation' AS domain_id
      FROM @cdm_database_schema.observation
      WHERE observation_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    {@has_visit} ? {
      {@has_condition | @has_procedure | @has_drug | @has_device | @has_measurement | @has_observation} ? {
      UNION ALL
      }
      SELECT DISTINCT
       person_id,
       'Visit' AS domain_id
      FROM @cdm_database_schema.visit_occurrence
      WHERE visit_concept_id IN (
        SELECT concept_id
        FROM #concept_set
      )
    }
    )
    SELECT
    {@has_condition} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Condition' THEN person_id
      END) AS condition_persons,
    }
    {@has_procedure} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Procedure' THEN person_id
      END) AS procedure_persons,
    }
    {@has_drug} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Drug' THEN person_id
      END) AS drug_persons,
    }
    {@has_device} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Device' THEN person_id
      END) AS device_persons,
    }
    {@has_measurement} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Measurement' THEN person_id
      END) AS measurement_persons,
    }
    {@has_observation} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Observation' THEN person_id
      END) AS observation_persons,
    }
    {@has_visit} ? {
      COUNT(DISTINCT CASE
        WHEN domain_id = 'Visit' THEN person_id
      END) AS visit_persons,
    }
      COUNT(DISTINCT person_id) AS overall_persons
      FROM domain_persons;
  "
  counts <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cdm_database_schema = cdmDatabaseSchema,
    has_condition = "Condition" %in% domains,
    has_procedure = "Procedure" %in% domains,
    has_drug = "Drug" %in% domains,
    has_device = "Device" %in% domains,
    has_measurement = "Measurement" %in% domains,
    has_observation = "Observation" %in% domains,
    has_visit = "Visit" %in% domains,
    snakeCaseToCamelCase = TRUE
  )
  return(counts)
}
cdmConnection <- DatabaseConnector::connect(cdmConnectionDetails)
priorCounts <- readRDS("e:/temp/conceptSetPersonCounts.rds")
fullConceptSetIds <- priorCounts |>
  group_by(conceptSetId) |>
  summarise(n = n()) |>
  filter(n == 2) |>
  pull(conceptSetId)
remainingConceptSets <- conceptSets |>
  filter(!conceptSetId %in% fullConceptSetIds)
priorCounts <- priorCounts |>
  filter(!conceptSetId %in% fullConceptSetIds)

conceptSetPersonCounts <- list()
pb <- txtProgressBar(max = nrow(remainingConceptSets), style = 3)
for (i in seq_len(nrow(remainingConceptSets))) {
  sql <- CirceR::buildConceptSetQuery(remainingConceptSets$circeConceptSetExpression[i])
  for (j in seq_along(cdmDatabaseSchemas)) {
    counts <- getCounts(conceptSetSql = sql,
              connection = cdmConnection,
              cdmDatabaseSchema = cdmDatabaseSchemas[j])
    conceptSetPersonCounts[[length(conceptSetPersonCounts) + 1]] <- counts |>
      mutate(conceptSetId = remainingConceptSets$conceptSetId[i],
             databaseId = databaseMetaDatas$databaseId[databaseMetaDatas$cdmDatabaseSchema == cdmDatabaseSchemas[j]])
  }
  setTxtProgressBar(pb, i)
}
close(pb)

conceptSetPersonCounts <- bind_rows(conceptSetPersonCounts)
conceptSetPersonCounts <- bind_rows(conceptSetPersonCounts, priorCounts)
conceptSetPersonCounts <- conceptSetPersonCounts |>
  mutate(across(ends_with("Persons"), ~ coalesce(.x, 0)))
saveRDS(conceptSetPersonCounts, "e:/temp/conceptSetPersonCounts.rds")

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "concept_set_person_count",
  data = conceptSetPersonCounts,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload draft_concept_set_target ------------------------------------------
draftConceptSetTarget <- readr::read_csv("../largescalephentest/TargetsAsConceptsCleaned.csv")
draftConceptSetTarget <- draftConceptSetTarget |>
  inner_join(definitions |>
               select(phenotype = "phenotypeName", "phenotypeId"),
             by = join_by(phenotype)) |>
  transmute(
    phenotypeId,
    pertains = target,
    category = stringr::str_to_sentence(gsub("_", " ", category)),
    term,
    conceptId,
    conceptName,
    domainId,
    conceptClassId,
    vocabularyId,
    matchType = match
  )
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "draft_concept_set_target",
  data = draftConceptSetTarget,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload draft_target_to_concept_set -----------------------------------------
targetDefinitions <- readr::read_csv("../largescalephentest/ConceptSetTargetDefinitions.csv")
phenotypeToConceptSetName <- readr::read_csv("../largescalephentest/PhenotypeToConceptSetTargetNames.csv")

targetDefinitions <- targetDefinitions |>
  inner_join(phenotypeToConceptSetName |>
               transmute(phenotypeName = phenotype,
                         conceptSetName,
                         pertains = target,
                         category = stringr::str_to_sentence(gsub("_", " ", category))),
             by = join_by("conceptSetName")) |>
  inner_join(definitions |>
               select("phenotypeName", "phenotypeId"),
             by = join_by("phenotypeName"))

oneToOne <- targetDefinitions |>
  inner_join(conceptSets |>
               filter(!is.na(conceptId), hasDescendants == 1) |>
               select("conceptSetId", conceptSetConceptId = "conceptId"),
             by = join_by("conceptSetConceptId")) |>
  select("phenotypeId",
         "pertains",
         "category",
         conceptId = "conceptSetConceptId",
         "conceptSetId")

oneToMany <- targetDefinitions |>
  filter(is.na(conceptSetConceptId)) |>
  inner_join(conceptSets |>
               filter(is.na(conceptId), hasDescendants == 1) |>
               select("conceptSetId", "conceptSetName"),
             by = join_by("conceptSetName")) |>
  select("phenotypeId",
         "pertains",
         "category",
         conceptId = "seedConceptId",
         "conceptSetId")
draftTargetToConceptSet <- bind_rows(oneToOne, oneToMany)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "draft_target_to_concept_set",
  data = draftTargetToConceptSet,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

# Upload phenotype metadata ---------------------------------------------------------
phenotypeMetaData <- readr::read_csv("e:/temp/ClinicalDefinitionMetadata.csv") |>
  SqlRender::snakeCaseToCamelCaseNames()
# writeLines(paste0("'", paste(unique(phenotypeMetaData$isPharmacovigilanceOutcome), collapse = "', '"), "'"))

phenotypeMetaData <- phenotypeMetaData |>
  inner_join(definitions |>
               select(sourcePhenotype = "phenotypeName", "phenotypeId"),
             by = join_by("sourcePhenotype")) |>
  transmute(
    phenotypeId,
    isPharmacovigilanceOutcome,
    clinicalCourse,
    expectedCareSetting,
    sexConstraint,
    minAgeYears,
    maxAgeYears,
    minimumInterepisodeGapDays,
    hasDiscreteRecordedSymptoms = as.integer(hasDiscreteRecordedSymptoms),
    requiresDiagnosticTestOrProcedure = as.integer(requiresDiagnosticTestOrProcedure),
    requiresActiveTreatmentWithin30d = as.integer(requiresActiveTreatmentWithin30d),
    expectsConditionSpecificFollowupOrSequelae1yr = as.integer(expectsConditionSpecificFollowupOrSequelae1yr)
  )
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = databaseSchema,
  tableName = "phenotype_meta_data",
  data = phenotypeMetaData,
  dropTableIfExists = FALSE,
  createTable = FALSE,
  camelCaseToSnakeCase = TRUE
)

DatabaseConnector::disconnect(connection)

