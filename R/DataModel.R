# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Pegasus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create the data model on the server
#'
#' @template Connection
#' @param databaseSchema The database schema where the tables will be created.
#'
#' @returns
#' Does not return anything. Creates the tables of the data model on the server.
#'
#' @export
createDataModel <- function(connectionDetails = NULL,
                            connection = NULL,
                            databaseSchema) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(connection, "DatabaseConnectorConnection", null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(databaseSchema, len = 1, add = errorMessages)
  checkmate::reportAssertions(errorMessages)
  if (is.null(connectionDetails) && is.null(connection)) {
    stop("Must provide either connectionDetails or a connection.")
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  message("Creating data model in schema ", databaseSchema)
  sql <- SqlRender::loadRenderTranslateSql(
    "CreateDataModel.sql",
    packageName = "Pegasus",
    dbms = DatabaseConnector::dbms(connection),
    database_schema = databaseSchema
  )
  DatabaseConnector::executeSql(connection, sql)
}
