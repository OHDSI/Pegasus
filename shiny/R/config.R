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

# Written as an immediate warning: it prints at once even while the app is busy, and warnings are
# reliably shown in deployment logs where plain messages were not.
logEvent <- function(...) {
  if (isTRUE(getOption("pegasus.quiet"))) {
    return(invisible(NULL))
  }
  warning("[pegasus] ", ..., call. = FALSE, immediate. = TRUE)
}

getEnv <- function(name, required = TRUE) {
  value <- Sys.getenv(name)
  if (required && !nzchar(value)) {
    stop("Environment variable ", name, " is not set.")
  }
  if (nzchar(value)) value else NULL
}

loadConfig <- function() {
  dbms <- getEnv("PEGASUS_DBMS")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = getEnv("PEGASUS_SERVER", required = FALSE),
    connectionString = getEnv("PEGASUS_CONNECTION_STRING", required = FALSE),
    user = getEnv("PEGASUS_DB_USER", required = FALSE),
    password = getEnv("PEGASUS_DB_PASSWORD", required = FALSE)
  )
  list(
    dbms = dbms,
    connectionDetails = connectionDetails,
    vocabulary = loadVocabularyConfig(),
    databaseSchema = getEnv("PEGASUS_DATABASE_SCHEMA"),
    llm = list(
      provider = getEnv("PEGASUS_LLM_PROVIDER", required = FALSE),
      endpoint = getEnv("PEGASUS_LLM_ENDPOINT"),
      apiKey = getEnv("PEGASUS_LLM_API_KEY"),
      model = getEnv("PEGASUS_LLM_MODEL"),
      apiVersion = getEnv("PEGASUS_LLM_API_VERSION")
    )
  )
}

# The vocabulary database is optional; without it concept search only finds exact concept matches.
loadVocabularyConfig <- function() {
  dbms <- getEnv("PEGASUS_VOCAB_DBMS", required = FALSE)
  if (is.null(dbms)) {
    return(NULL)
  }
  list(
    dbms = dbms,
    connectionDetails = DatabaseConnector::createConnectionDetails(
      dbms = dbms,
      server = getEnv("PEGASUS_VOCAB_SERVER", required = FALSE),
      connectionString = getEnv("PEGASUS_VOCAB_CONNECTION_STRING", required = FALSE),
      user = getEnv("PEGASUS_VOCAB_DB_USER", required = FALSE),
      password = getEnv("PEGASUS_VOCAB_DB_PASSWORD", required = FALSE)
    ),
    schema = getEnv("PEGASUS_VOCAB_SCHEMA")
  )
}

ensureJdbcDriver <- function(dbms) {
  if (dbms %in% c("duckdb", "sqlite")) {
    return(invisible(NULL))
  }
  jarFolder <- getEnv("DATABASECONNECTOR_JAR_FOLDER")
  if (length(list.files(jarFolder, pattern = "\\.jar$", ignore.case = TRUE)) == 0) {
    dir.create(jarFolder, recursive = TRUE, showWarnings = FALSE)
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jarFolder)
  }
  invisible(NULL)
}

isRunningOnConnect <- function() {
  Sys.getenv("RSTUDIO_PRODUCT") == "CONNECT"
}

# Returns NULL when on Connect without an authenticated user, which disables saving.
getUserName <- function(session) {
  if (!is.null(session$user) && nzchar(session$user)) {
    return(session$user)
  }
  if (isRunningOnConnect()) {
    return(NULL)
  }
  defaultUser <- Sys.getenv("PEGASUS_DEFAULT_USER")
  if (nzchar(defaultUser)) defaultUser else "anonymous"
}
