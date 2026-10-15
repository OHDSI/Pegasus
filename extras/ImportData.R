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
cdmConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchemas <- c("optum_extended_dod.cdm_optum_extended_dod_v4020",
                        "optum_ehr.cdm_optum_ehr_v4154")
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
            specificity,
            specificityLb,
            specificityUb,
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



DatabaseConnector::disconnect(connection)
