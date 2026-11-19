test_that("createDataModel creates the core tables and can be re-run", {
  connection <- createTestPegasusConnection()
  Pegasus::createDataModel(pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  tables <- tolower(DatabaseConnector::getTableNames(connection, databaseSchema = testSchema))
  expect_true(all(c("phenotype", "concept_set", "phenotype_to_concept_set", "cohort_definition",
                    "database_meta_data", "concept_set_person_count") %in% tables))
})

test_that("uploaded phenotypes can be downloaded, and IDs continue from the existing maximum", {
  connection <- createTestPegasusConnection()
  Pegasus::uploadPhenotypes(
    data.frame(phenotypeName = c("Asthma", "Copd"), clinicalDefinition = c("def a", "def c")),
    creator = "tester",
    pegasusConnection = connection,
    pegasusDatabaseSchema = testSchema
  )
  Pegasus::uploadPhenotypes(
    data.frame(phenotypeName = "Gout", clinicalDefinition = "def g"),
    pegasusConnection = connection,
    pegasusDatabaseSchema = testSchema
  )
  phenotypes <- Pegasus::downloadPhenotypes(pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  expect_equal(nrow(phenotypes), 3)
  expect_equal(sort(phenotypes$phenotypeId), 1:3)
  expect_equal(phenotypes$phenotypeId[phenotypes$phenotypeName == "Gout"], 3)
  expect_equal(unique(phenotypes$creator[phenotypes$phenotypeName == "Asthma"]), "tester")

  subset <- Pegasus::downloadPhenotypes(phenotypeIds = 2,
                                        pegasusConnection = connection,
                                        pegasusDatabaseSchema = testSchema)
  expect_equal(subset$phenotypeName, "Copd")
})

test_that("uploading a phenotype with an existing name fails", {
  connection <- createTestPegasusConnection()
  phenotypes <- data.frame(phenotypeName = "Asthma", clinicalDefinition = "def a")
  Pegasus::uploadPhenotypes(phenotypes, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  expect_error(
    Pegasus::uploadPhenotypes(phenotypes, pegasusConnection = connection, pegasusDatabaseSchema = testSchema),
    "Overlapping"
  )
})

test_that("uploaded concept sets are assigned IDs and linked to their phenotype", {
  connection <- createTestPegasusConnection()
  Pegasus::uploadPhenotypes(
    data.frame(phenotypeName = "Asthma", clinicalDefinition = "def a"),
    pegasusConnection = connection,
    pegasusDatabaseSchema = testSchema
  )
  conceptSets <- data.frame(
    phenotypeId = 1,
    pertains = "Disease of interest",
    category = "Disease of interest",
    conceptId = 1,
    conceptName = "Concept 1",
    conceptSetId = NA_real_,
    conceptSetName = "Asthma",
    clinicalDefinition = "def a",
    sourceType = "Concept + descendants",
    circeConceptSetExpression = createCirceConceptSet(list(1, FALSE, TRUE)),
    caprConceptSetExpression = "cs(descendants(1), name = \"Asthma\")",
    hasDescendants = 1
  )
  Pegasus::uploadConceptSets(conceptSets, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)

  stored <- Pegasus::downloadConceptSets(pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  expect_equal(nrow(stored), 1)
  expect_equal(stored$conceptSetName, "Asthma")

  links <- Pegasus::downloadPhenotypeToConceptSets(phenotypeIds = 1,
                                                   pegasusConnection = connection,
                                                   pegasusDatabaseSchema = testSchema)
  expect_equal(links$conceptSetId, stored$conceptSetId)
})

test_that("database metadata round trips and rejects duplicate database IDs", {
  connection <- createTestPegasusConnection()
  metaData <- data.frame(
    databaseId = 1,
    cdmSourceName = "Test CDM",
    cdmSourceAbbreviation = "TEST",
    cdmReleaseDate = as.Date("2024-01-01"),
    cdmReleaseIdentifier = "v1",
    vocabularyVersion = "v5.0",
    cdmDatabaseSchema = "cdm",
    resultDatabaseSchema = "results"
  )
  Pegasus::uploadDatabaseMetaData(metaData, pegasusConnection = connection, pegasusDatabaseSchema = testSchema)
  downloaded <- Pegasus::downloadDatabaseMetaData(pegasusConnection = connection,
                                                  pegasusDatabaseSchema = testSchema)
  expect_equal(downloaded$cdmSourceName, "Test CDM")
  expect_equal(as.Date(downloaded$cdmReleaseDate), as.Date("2024-01-01"))
  expect_error(
    Pegasus::uploadDatabaseMetaData(metaData, pegasusConnection = connection, pegasusDatabaseSchema = testSchema),
    "overlap"
  )
})

test_that("concept set person counts are computed against a CDM", {
  cdmConnectionDetails <- createTestCdmConnectionDetails()
  connection <- DatabaseConnector::connect(cdmConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  countPersons <- function(expression) {
    Pegasus:::getCounts(circeConceptSetExpression = expression,
                        connection = connection,
                        cdmDatabaseSchema = testSchema,
                        tempEmulationSchema = NULL)
  }

  expect_equal(countPersons(createCirceConceptSet(list(1, FALSE, FALSE)))$overallPersons, 1)
  expect_equal(countPersons(createCirceConceptSet(list(1, FALSE, TRUE)))$overallPersons, 3)
  expect_equal(countPersons(createCirceConceptSet(list(1, FALSE, TRUE), list(3, TRUE, FALSE)))$overallPersons, 2)
  expect_equal(countPersons(createCirceConceptSet(list(1, FALSE, TRUE)))$conditionPersons, 3)
})

test_that("createConceptSetCounts counts concept sets in each database and the result can be uploaded", {
  cdmConnectionDetails <- createTestCdmConnectionDetails()
  pegasusConnection <- createTestPegasusConnection()
  databaseReference <- Pegasus::createDatabaseReference(
    databaseName = "Test",
    connectionDetails = cdmConnectionDetails,
    tempEmulationSchema = NULL,
    cdmDatabaseSchema = testSchema,
    sensitiveCohortDatabaseSchema = testSchema,
    sensitiveCohortTable = "sensitive",
    referenceCohortDatabaseSchema = testSchema,
    referenceCohortTable = "reference",
    candidateCohortDatabaseSchema = testSchema,
    candidateCohortTable = "candidate"
  )
  conceptSets <- data.frame(
    conceptSetId = c(1, 2),
    circeConceptSetExpression = c(createCirceConceptSet(list(1, FALSE, TRUE)),
                                  createCirceConceptSet(list(4, FALSE, FALSE)))
  )
  counts <- Pegasus::createConceptSetCounts(
    conceptSets = conceptSets,
    pegasusConnection = pegasusConnection,
    pegasusDatabaseSchema = testSchema,
    databaseReferences = list(databaseReference),
    maxCores = 1
  )
  expect_equal(counts$overallPersons[counts$conceptSetId == 1], 3)
  expect_equal(counts$overallPersons[counts$conceptSetId == 2], 1)
  expect_true(all(counts$databaseId == databaseReference$databaseId))

  Pegasus::uploadConceptSetPersonCounts(counts,
                                        pegasusConnection = pegasusConnection,
                                        pegasusDatabaseSchema = testSchema)
  stored <- DatabaseConnector::querySql(
    pegasusConnection,
    "SELECT concept_set_id, overall_persons FROM main.concept_set_person_count;",
    snakeCaseToCamelCase = TRUE
  )
  expect_equal(nrow(stored), 2)
})
