library(dplyr)

# Fixtures for the MonoFlow agent tools.
#
# CDM (all persons observed 2010-01-01 to 2020-12-31):
# - Concept 1 ('Disease A') is the ancestor of concepts 2 and 3. Concept 4 is unrelated. Concept 10 is a lab test.
# - Person 1 (male, 1950): concept 4 on 2014-12-15, concept 2 on 2015-01-01, lab value 5 mg/dL.
# - Person 2 (female, 1960): concept 3 on 2016-06-01, lab value 7 mg/dL.
# - Person 3 (male, 1970): concept 4 on 2017-01-01, lab value missing (mg/dL).
# - Person 4 (female, 1980): no conditions, lab value 3 without unit.
# A 'Disease A' cohort therefore contains persons 1 and 2.
agentCdmTables <- function() {
  concept <- data.frame(
    concept_id = c(1, 2, 3, 4, 10, 8840, 8507, 8532),
    concept_name = c("Disease A", "Disease A1", "Disease A2", "Disease B", "Lab test", "mg/dL", "MALE", "FEMALE"),
    domain_id = c(rep("Condition", 4), "Measurement", "Unit", "Gender", "Gender"),
    vocabulary_id = "TEST",
    concept_class_id = "Test",
    standard_concept = "S",
    concept_code = c("C1", "C2", "C3", "C4", "M10", "U1", "M", "F"),
    valid_start_date = as.Date("1970-01-01"),
    valid_end_date = as.Date("2099-12-31"),
    invalid_reason = NA_character_
  )
  conceptAncestor <- data.frame(
    ancestor_concept_id = c(concept$concept_id, 1, 1),
    descendant_concept_id = c(concept$concept_id, 2, 3),
    min_levels_of_separation = c(rep(0, nrow(concept)), 1, 1),
    max_levels_of_separation = c(rep(0, nrow(concept)), 1, 1)
  )
  conceptRelationship <- data.frame(
    concept_id_1 = integer(),
    concept_id_2 = integer(),
    relationship_id = character(),
    valid_start_date = as.Date(character()),
    valid_end_date = as.Date(character()),
    invalid_reason = character()
  )
  person <- data.frame(
    person_id = 1:4,
    gender_concept_id = c(8507, 8532, 8507, 8532),
    year_of_birth = c(1950, 1960, 1970, 1980),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observationPeriod <- data.frame(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2020-12-31"),
    period_type_concept_id = 0
  )
  conditionStartDate <- as.Date(c("2014-12-15", "2015-01-01", "2016-06-01", "2017-01-01"))
  conditionOccurrence <- data.frame(
    condition_occurrence_id = 1:4,
    person_id = c(1, 1, 2, 3),
    condition_concept_id = c(4, 2, 3, 4),
    condition_start_date = conditionStartDate,
    condition_start_datetime = as.POSIXct(conditionStartDate),
    condition_end_date = conditionStartDate,
    condition_end_datetime = as.POSIXct(conditionStartDate),
    condition_type_concept_id = 0,
    condition_status_concept_id = 0,
    stop_reason = NA_character_,
    provider_id = NA_integer_,
    visit_occurrence_id = NA_integer_,
    visit_detail_id = NA_integer_,
    condition_source_value = NA_character_,
    condition_source_concept_id = 0,
    condition_status_source_value = NA_character_
  )
  measurement <- data.frame(
    measurement_id = 1:4,
    person_id = 1:4,
    measurement_concept_id = 10,
    measurement_date = as.Date("2015-01-02"),
    measurement_type_concept_id = 0,
    value_as_number = c(5, 7, NA, 3),
    unit_concept_id = c(8840, 8840, 8840, NA)
  )
  emptyOccurrences <- function(prefix, dateColumn) {
    table <- data.frame(person_id = integer(), concept_id = integer(), date = as.Date(character()))
    colnames(table) <- c("person_id", paste0(prefix, "_concept_id"), dateColumn)
    table
  }
  list(
    concept = concept,
    concept_ancestor = conceptAncestor,
    concept_relationship = conceptRelationship,
    person = person,
    observation_period = observationPeriod,
    condition_occurrence = conditionOccurrence,
    measurement = measurement,
    procedure_occurrence = emptyOccurrences("procedure", "procedure_date"),
    drug_exposure = emptyOccurrences("drug", "drug_exposure_start_date"),
    observation = emptyOccurrences("observation", "observation_date"),
    visit_occurrence = emptyOccurrences("visit", "visit_start_date"),
    cdm_source = data.frame(cdm_source_name = "Test CDM", source_description = "A tiny test CDM")
  )
}

createAgentCdmConnectionDetails <- function(envir = parent.frame()) {
  connectionDetails <- createTestConnectionDetails(envir)
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  tables <- agentCdmTables()
  for (tableName in names(tables)) {
    DatabaseConnector::insertTable(connection = connection,
                                   databaseSchema = testSchema,
                                   tableName = tableName,
                                   data = tables[[tableName]],
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = FALSE)
  }
  connectionDetails
}

createAgentDatabaseReference <- function(connectionDetails, databaseName = "TestCdm") {
  Pegasus::createDatabaseReference(
    databaseName = databaseName,
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = testSchema,
    sensitiveCohortDatabaseSchema = testSchema,
    sensitiveCohortTable = "sensitive_cohort",
    referenceCohortDatabaseSchema = testSchema,
    referenceCohortTable = "reference_cohort",
    candidateCohortDatabaseSchema = testSchema,
    candidateCohortTable = "candidate_cohort",
    workDatabaseSchema = testSchema,
    workCohortTable = "agent_cohort",
    workConceptSetTable = "agent_concept_set",
    databaseId = 1
  )
}

# Concept sets stored in the Pegasus database. 'Unrelated' is not linked to the phenotype.
agentConceptSets <- function() {
  tibble(
    conceptSetId = 1:4,
    conceptSetName = c("Disease A", "Disease B", "Lab test", "Unrelated"),
    circeConceptSetExpression = c(createCirceConceptSet(list(1, FALSE, TRUE)),
                                  createCirceConceptSet(list(4, FALSE, FALSE)),
                                  createCirceConceptSet(list(10, FALSE, FALSE)),
                                  createCirceConceptSet(list(3, FALSE, FALSE))),
    caprConceptSetExpression = c('cs(descendants(1), name = "Disease A")',
                                 'cs(4, name = "Disease B")',
                                 'cs(10, name = "Lab test")',
                                 'cs(3, name = "Unrelated")'),
    hasDescendants = c(1, 0, 0, 0),
    valid = 1
  )
}

# Pegasus database with phenotype 1 linked to concept sets 1-3, and person counts for the test database (ID 1) and
# another database (ID 2) that is not part of the agent context.
createAgentPegasusConnection <- function(envir = parent.frame()) {
  connection <- createTestPegasusConnection(envir)
  conceptSets <- agentConceptSets()
  counts <- tidyr::expand_grid(conceptSetId = conceptSets$conceptSetId, databaseId = 1:2) |>
    mutate(conditionPersons = .data$conceptSetId * 10 + .data$databaseId,
           procedurePersons = 0,
           drugPersons = 0,
           devicePersons = 0,
           measurementPersons = 0,
           observationPersons = 0,
           visitPersons = 0,
           overallPersons = .data$conditionPersons)
  tables <- list(
    concept_set = conceptSets,
    phenotype_to_concept_set = tibble(phenotypeId = 1,
                                              pertains = "Disease of interest",
                                              category = "Clinical presentation",
                                              conceptSetId = 1:3),
    concept_set_person_count = counts,
    database_meta_data = tibble(databaseId = 1:2,
                                        cdmSourceName = c("Test CDM", "Other CDM"),
                                        cdmSourceAbbreviation = c("TestCdm", "OtherCdm"),
                                        cdmReleaseDate = as.Date("2025-01-01"),
                                        vocabularyVersion = "v1")
  )
  for (tableName in names(tables)) {
    DatabaseConnector::insertTable(connection = connection,
                                   databaseSchema = testSchema,
                                   tableName = tableName,
                                   data = tables[[tableName]],
                                   createTable = FALSE,
                                   dropTableIfExists = FALSE,
                                   camelCaseToSnakeCase = TRUE,
                                   progressBar = FALSE)
  }
  connection
}

# Agent context over the test CDM; closed when `envir` exits.
createAgentContext <- function(envir = parent.frame()) {
  pegasusConnection <- createAgentPegasusConnection(envir)
  cdmConnectionDetails <- createAgentCdmConnectionDetails(envir)
  context <- Pegasus:::PegasusAgentContext$new(
    phenotype = tibble(phenotypeId = 1, phenotypeName = "Disease A", clinicalDefinition = ""),
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = testSchema,
    databaseReferences = list(createAgentDatabaseReference(cdmConnectionDetails))
  )
  withr::defer(context$close(), envir = envir)
  context
}

diseaseACohortCapr <- paste0(
  'cohort(entry = entry(conditionOccurrence(cs(descendants(1), name = "Disease A")), primaryCriteriaLimit = "First"), ',
  'exit = exit(endStrategy = observationExit()))'
)
