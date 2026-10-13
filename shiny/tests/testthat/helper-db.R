createTestPool <- function() {
  pool <- pool::poolCreate(
    factory = function() {
      DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
    },
    minSize = 1,
    maxSize = 1
  )
  withConnection(pool, function(connection) {
    DatabaseConnector::executeSql(
      connection,
      "CREATE TABLE main.phenotype (
        phenotype_id INT PRIMARY KEY,
        phenotype_name VARCHAR NOT NULL,
        clinical_definition VARCHAR NOT NULL,
        creator VARCHAR NOT NULL,
        time_created DATETIME NOT NULL
      );",
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  })
  pool
}
