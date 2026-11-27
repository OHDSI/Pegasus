uploadTestPhenotypes <- function(connection, names = c("Asthma", "Copd")) {
  Pegasus::uploadPhenotypes(
    data.frame(phenotypeName = names, clinicalDefinition = paste("definition of", names)),
    pegasusConnection = connection,
    pegasusDatabaseSchema = testSchema
  )
}

test_that("phenotype meta data round trips and rejects duplicates", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection)
  metaData <- data.frame(
    phenotypeId = 1:2,
    conditionName = c("Asthma", "Copd"),
    isPharmacovigilanceOutcome = c("YES", "NO"),
    clinicalCourse = "PERSISTENT_STABLE",
    expectedCareSetting = "OUTPATIENT_EXPECTED",
    sexConstraint = "BOTH_SEXES",
    minAgeYears = c(0, 40),
    maxAgeYears = 120,
    minimumInterepisodeGapDays = 30,
    recommendedCohortExit = "OBSERVATION_PERIOD_END",
    recommendedExitDays = 0,
    hasDiscreteRecordedSymptoms = 1,
    requiresDiagnosticTestOrProcedure = 0,
    requiresActiveTreatmentWithin30d = 0,
    expectsConditionSpecificFollowupOrSequelae1yr = 1
  )
  Pegasus::uploadPhenotypeMetaData(metaData, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)

  downloaded <- Pegasus::downloadPhenotypeMetaData(pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  expect_equal(sort(downloaded$phenotypeId), 1:2)
  expect_equal(downloaded$minAgeYears[downloaded$phenotypeId == 2], 40)

  one <- Pegasus::downloadPhenotypeMetaData(phenotypeIds = 1,
                                            pegasusConnection = connection,
                                            pegasusDatabaseSchema = testSchema)
  expect_equal(one$isPharmacovigilanceOutcome, "YES")

  expect_error(
    Pegasus::uploadPhenotypeMetaData(metaData, pegasusConnection = connection, pegasusDatabaseSchema = testSchema),
    "overlapping"
  )
})

test_that("draft concept set targets are uploaded, and a repeat upload is rejected", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection)
  targets <- data.frame(
    phenotypeName = "Asthma",
    pertains = "Disease of interest",
    category = "Disease of interest",
    term = "asthma",
    conceptId = c(1, 2),
    conceptName = c("Concept 1", "Concept 2"),
    domainId = "Condition",
    conceptClassId = "Clinical Finding",
    vocabularyId = "TEST",
    match = "Verbatim",
    phenotypeId = 1
  )
  Pegasus::uploadDraftConceptSetTargets(targets, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  stored <- DatabaseConnector::querySql(connection,
                                        "SELECT * FROM main.draft_concept_set_target;",
                                        snakeCaseToCamelCase = TRUE)
  expect_equal(sort(stored$conceptId), c(1, 2))
  expect_equal(unique(stored$phenotypeId), 1)
  expect_error(
    Pegasus::uploadDraftConceptSetTargets(targets, pegasusConnection = connection, pegasusDatabaseSchema = testSchema),
    "overlap"
  )
})

test_that("cohort definitions are linked to phenotypes by name and can be downloaded", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection)
  cohortDefinitions <- data.frame(
    phenotypeId = c(1, 2),
    cohortDefinitionName = c("Asthma cohort", "Copd cohort"),
    circeCohortExpression = "{}",
    caprCohortExpression = "cohort()",
    cdSourceType = "Templates"
  )
  Pegasus::uploadCohortDefinitions(cohortDefinitions,
                                   pegasusConnection = connection,
                                   pegasusDatabaseSchema = testSchema)
  downloaded <- Pegasus::downloadCohortDefinition(pegasusConnection = connection,
                                                  pegasusDatabaseSchema = testSchema)
  expect_equal(nrow(downloaded), 2)
  expect_equal(downloaded$phenotypeId[downloaded$cohortDefinitionName == "Copd cohort"], 2)
  expect_equal(unique(downloaded$valid), 1)

  subset <- Pegasus::downloadCohortDefinition(phenotypeIds = 1,
                                              pegasusConnection = connection,
                                              pegasusDatabaseSchema = testSchema)
  expect_equal(subset$cohortDefinitionName, "Asthma cohort")

  expect_error(
    Pegasus::uploadCohortDefinitions(cohortDefinitions,
                                     pegasusConnection = connection,
                                     pegasusDatabaseSchema = testSchema),
    "Overlapping"
  )
})

test_that("cohort definitions for unknown phenotypes are rejected", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection)
  expect_error(
    Pegasus::uploadCohortDefinitions(
      data.frame(phenotypeName = "Unknown",
                 cohortDefinitionName = "x",
                 circeCohortExpression = "{}",
                 caprCohortExpression = "cohort()",
                 cdSourceType = "User"),
      pegasusConnection = connection,
      pegasusDatabaseSchema = testSchema
    ),
    "Unknown"
  )
})

test_that("cohort definition evaluations are uploaded, and overlapping ones are rejected", {
  connection <- createTestPegasusConnection()
  evaluations <- data.frame(
    cohortDefinitionId = 1,
    databaseId = 1,
    cohortEntries = 10,
    cohortSubjects = 8,
    generationSeconds = 2,
    truePositives = 5,
    falsePositives = 1,
    trueNegatives = 90,
    falseNegatives = 4,
    ppv = 5 / 6, ppvLb = 0.4, ppvUb = 0.99,
    sensitivity = 5 / 9, sensitivityLb = 0.2, sensitivityUb = 0.8,
    specificity = 90 / 91, specificityLb = 0.9, specificityUb = 0.99,
    npv = 90 / 94, npvLb = 0.9, npvUb = 0.99,
    evaluationType = "KEEPER + llm"
  )
  Pegasus::uploadCohortDefinitionEvaluations(evaluations,
                                             pegasusConnection = connection,
                                             pegasusDatabaseSchema = testSchema)
  stored <- DatabaseConnector::querySql(connection,
                                        "SELECT * FROM main.cohort_definition_evaluation;",
                                        snakeCaseToCamelCase = TRUE)
  expect_equal(stored$truePositives, 5)
  expect_equal(stored$ppv, 5 / 6, tolerance = 1e-6)
  expect_error(
    Pegasus::uploadCohortDefinitionEvaluations(evaluations,
                                               pegasusConnection = connection,
                                               pegasusDatabaseSchema = testSchema),
    "Overlapping"
  )
})

test_that("only one ATLAS instance can be registered", {
  connection <- createTestPegasusConnection()
  Pegasus::uploadAtlasInstance("http://localhost/WebAPI",
                               pegasusConnection = connection,
                               pegasusDatabaseSchema = testSchema)
  stored <- DatabaseConnector::querySql(connection,
                                        "SELECT * FROM main.atlas_instance;",
                                        snakeCaseToCamelCase = TRUE)
  expect_equal(stored$baseUrl, "http://localhost/WebAPI")
  expect_error(
    Pegasus::uploadAtlasInstance("http://other/WebAPI",
                                 pegasusConnection = connection,
                                 pegasusDatabaseSchema = testSchema),
    "localhost"
  )
})

test_that("updatePhenotypes changes the draft and valid flags only for the named phenotypes", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection, c("Asthma", "Copd", "Gout"))
  getStatus <- function() {
    Pegasus::downloadPhenotypes(pegasusConnection = connection, pegasusDatabaseSchema = testSchema) |>
      arrange(.data$phenotypeId)
  }
  Pegasus::updatePhenotypes(data.frame(phenotypeName = "Asthma"),
                            draft = 1,
                            valid = 1,
                            pegasusConnection = connection,
                            pegasusDatabaseSchema = testSchema)
  expect_equal(getStatus()$draft, c(1, 0, 0))

  Pegasus::updatePhenotypes(data.frame(phenotypeName = "Copd"),
                            draft = 0,
                            valid = 0,
                            pegasusConnection = connection,
                            pegasusDatabaseSchema = testSchema)
  expect_equal(getStatus()$valid, c(1, 0, 1))
})

test_that("updatePhenotypes fails for phenotypes that do not exist", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection)
  expect_error(
    Pegasus::updatePhenotypes(data.frame(phenotypeName = "Unknown"),
                              pegasusConnection = connection,
                              pegasusDatabaseSchema = testSchema),
    "Unknown"
  )
})

test_that("a missing connection is reported", {
  expect_error(Pegasus::downloadPhenotypes(pegasusDatabaseSchema = testSchema), "pegasusConnection")
})

test_that("removeExistingPhenotypes drops phenotypes that already have rows in the table", {
  connection <- createTestPegasusConnection()
  uploadTestPhenotypes(connection)
  Pegasus::uploadCohortDefinitions(
    data.frame(phenotypeId = 1,
               cohortDefinitionName = "Asthma cohort",
               circeCohortExpression = "{}",
               caprCohortExpression = "cohort()",
               cdSourceType = "Templates"),
    pegasusConnection = connection,
    pegasusDatabaseSchema = testSchema
  )
  phenotypes <- data.frame(phenotypeId = 1:2, phenotypeName = c("Asthma", "Copd"))
  remaining <- Pegasus:::removeExistingPhenotypes(phenotypes,
                                                  table = "cohort_definition",
                                                  pegasusConnection = connection,
                                                  pegasusDatabaseSchema = testSchema)
  expect_equal(remaining$phenotypeName, "Copd")
  expect_equal(Pegasus:::removeExistingPhenotypes(phenotypes, table = "cohort_definition"), phenotypes)
})

test_that("removeConceptSetsWithExistingCounts only drops concept sets counted for the same database", {
  connection <- createTestPegasusConnection()
  Pegasus::uploadConceptSetPersonCounts(
    data.frame(databaseId = 1, conceptSetId = 1, overallPersons = 3, conditionPersons = 3, drugPersons = 0,
               observationPersons = 0, measurementPersons = 0, procedurePersons = 0, devicePersons = 0,
               visitPersons = 0),
    pegasusConnection = connection,
    pegasusDatabaseSchema = testSchema
  )
  conceptSets <- data.frame(conceptSetId = 1:2)
  filter <- function(databaseId) {
    Pegasus:::removeConceptSetsWithExistingCounts(conceptSets,
                                                  databaseId = databaseId,
                                                  pegasusConnection = connection,
                                                  pegasusDatabaseSchema = testSchema)$conceptSetId
  }
  expect_equal(filter(1), 2)
  expect_equal(filter(2), 1:2)
  expect_equal(Pegasus:::removeConceptSetsWithExistingCounts(conceptSets, 1, NULL, NULL)$conceptSetId, 1:2)
})

test_that("createDatabaseReference derives a stable database ID from the CDM schema", {
  create <- function(schema) {
    Pegasus::createDatabaseReference(
      databaseName = "Test",
      connectionDetails = NULL,
      tempEmulationSchema = NULL,
      cdmDatabaseSchema = schema,
      sensitiveCohortDatabaseSchema = "s", sensitiveCohortTable = "s",
      referenceCohortDatabaseSchema = "r", referenceCohortTable = "r",
      candidateCohortDatabaseSchema = "c", candidateCohortTable = "c"
    )
  }
  expect_s3_class(create("cdm_a"), "DatabaseReference")
  expect_equal(create("cdm_a")$databaseId, create("cdm_a")$databaseId)
  expect_false(create("cdm_a")$databaseId == create("cdm_b")$databaseId)
})

test_that("getDatabaseMetaData reads cdm_source and skips databases that are already uploaded", {
  cdmConnectionDetails <- createTestCdmConnectionDetails()
  connection <- DatabaseConnector::connect(cdmConnectionDetails)
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = testSchema,
    tableName = "cdm_source",
    data = data.frame(cdm_source_name = "Test CDM",
                      cdm_source_abbreviation = "TEST",
                      cdm_release_date = as.Date("2024-01-01"),
                      vocabulary_version = "v5.0"),
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = FALSE
  )
  DatabaseConnector::disconnect(connection)
  databaseReference <- Pegasus::createDatabaseReference(
    databaseName = "Test",
    connectionDetails = cdmConnectionDetails,
    tempEmulationSchema = NULL,
    cdmDatabaseSchema = testSchema,
    sensitiveCohortDatabaseSchema = testSchema, sensitiveCohortTable = "s",
    referenceCohortDatabaseSchema = testSchema, referenceCohortTable = "r",
    candidateCohortDatabaseSchema = testSchema, candidateCohortTable = "c"
  )
  metaData <- Pegasus::getDatabaseMetaData(list(databaseReference))
  expect_equal(nrow(metaData), 1)
  expect_equal(metaData$cdmSourceName, "Test CDM")
  expect_equal(metaData$databaseId, databaseReference$databaseId)

  pegasusConnection <- createTestPegasusConnection()
  Pegasus::uploadDatabaseMetaData(metaData, pegasusConnection = pegasusConnection, pegasusDatabaseSchema = testSchema)
  expect_equal(
    nrow(Pegasus::getDatabaseMetaData(list(databaseReference),
                                      pegasusConnection = pegasusConnection,
                                      pegasusDatabaseSchema = testSchema)),
    0
  )
})

test_that("createCostTracker starts at zero", {
  tracker <- Pegasus::createCostTracker()
  expect_s3_class(tracker, "CostTracker")
  expect_equal(tracker$amount, 0)
})
