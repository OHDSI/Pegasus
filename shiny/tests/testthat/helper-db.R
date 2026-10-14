createTestPool <- function() {
  pool <- pool::poolCreate(
    factory = function() {
      DatabaseConnector::connect(dbms = "duckdb", server = ":memory:")
    },
    minSize = 1,
    maxSize = 1
  )
  withConnection(pool, function(connection) {
    # Uses the working-tree copy of the data model so tests track the real schema.
    sql <- SqlRender::render(
      SqlRender::readSql(file.path("..", "..", "..", "inst", "sql", "CreateDataModel.sql")),
      database_schema = "main"
    )
    sql <- SqlRender::translate(sql, targetDialect = "duckdb")
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  })
  pool
}
