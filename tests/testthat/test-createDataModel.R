test_that("createDataModel creates the phenotype table in DuckDB", {
  skip_if_not_installed("duckdb")

  connection <- DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  expect_message(
    createDataModel(connection = connection, databaseSchema = "main"),
    "Creating data model in schema main"
  )

  expect_true(
    DatabaseConnector::existsTable(
      connection,
      databaseSchema = "main",
      tableName = "phenotype"
    )
  )
})

test_that("createDataModel creates the expected phenotype columns", {
  skip_if_not_installed("duckdb")

  connection <- DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  createDataModel(connection = connection, databaseSchema = "main")

  result <- DatabaseConnector::querySql(
    connection,
    "SELECT * FROM main.phenotype LIMIT 0"
  )

  expect_named(result, c("phenotype_id", "phenotype_name", "clinical_definition", "creator", "time_created",
                         "draft", "valid", "parent_phenotype_id"))
})

test_that("createDataModel can be run repeatedly", {
  skip_if_not_installed("duckdb")

  connection <- DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  createDataModel(connection = connection, databaseSchema = "main")
  DatabaseConnector::executeSql(
    connection,
    "INSERT INTO main.phenotype (phenotype_id, phenotype_name, clinical_definition, creator, time_created, draft, valid)
     VALUES (1, 'Test phenotype', 'Test definition', 'test', CURRENT_TIMESTAMP, 0, 1)"
  )

  expect_message(
    createDataModel(connection = connection, databaseSchema = "main"),
    "Creating data model in schema main"
  )

  result <- DatabaseConnector::querySql(connection, "SELECT COUNT(*) AS n FROM main.phenotype")
  expect_equal(result$n, 0)
})