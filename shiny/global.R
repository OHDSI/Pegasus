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

library(shiny)

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file, local = FALSE)
}

config <- loadConfig()
ensureJdbcDriver(config$dbms)

connectionPool <- pool::poolCreate(
  factory = function() {
    DatabaseConnector::connect(config$connectionDetails)
  }
)
onStop(function() {
  pool::poolClose(connectionPool)
})

# Vocabulary requests run in a background R process with a fresh connection each time (see vocabulary.R), so no
# connection is opened here and a slow or stale vocabulary connection cannot freeze the app.
vocabularyRunner <- NULL
if (is.null(config$vocabulary)) {
  logEvent("No vocabulary database configured (PEGASUS_VOCAB_DBMS is not set); concept search is limited to concept IDs.")
} else {
  ensureJdbcDriver(config$vocabulary$dbms)
  vocabularyRunner <- backgroundVocabularyRunner(normalizePath(getwd()))
}

systemPrompt <- loadSystemPrompt("skills/ClinicalDefinitionRefiner.md")
