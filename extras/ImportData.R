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

# Upload phenotype definitions -----------------------------------------------------------------------------------------
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

# Upload concept sets --------------------------------------------------------------------------------------------------
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
              conceptSetConceptId,
              hypernym = 0,
              conceptSetExpression = expression),
  simpleConceptSets |>
    transmute(conceptSetName = conceptName,
              conceptSetConceptId = conceptId,
              hypernym = if_else(conceptSetClass == "hypernym", 1, 0),
              conceptSetExpression = expression)
)


conceptSets <- conceptSets |>
  transmute(
    conceptSetId = row_number(),
    conceptSetName,
    circeConceptSetExpression = conceptSetExpresion,
    caprConceptSetExpression = sapply(conceptSetExpression, convertCirceConceptSetToCapr),
    hasDescendants = 1 - hypernym,
    clinicalDefinition = "",
    concept_id = conceptSetConceptId,
    valid = 1,
    atlas_concept_set_id = NA_integer_,
    cs_source_type = "" # 'from human definition', 'from AI definition', 'concept + descendants', 'concept', or 'user'
  )


readRDS(phenotypeToConceptSet, "phenotypeToConceptSetNameTable.rds")

DatabaseConnector::disconnect(connection)
