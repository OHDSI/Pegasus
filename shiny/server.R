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

function(input, output, session) {
  phenotypes <- reactiveVal(data.frame(
    phenotypeId = integer(0),
    phenotypeName = character(0),
    clinicalDefinition = character(0),
    creator = character(0),
    timeCreated = as.POSIXct(character(0))
  ))

  refreshPhenotypes <- function() {
    tryCatch(
      phenotypes(withConnection(connectionPool, function(connection) {
        getPhenotypes(connection, config$databaseSchema)
      })),
      error = function(e) {
        showNotification(paste("Unable to load phenotypes:", conditionMessage(e)), type = "error")
      }
    )
  }
  refreshPhenotypes()

  searchServer("search", phenotypes, refreshPhenotypes)
  createServer(
    "create",
    connectionPool = connectionPool,
    databaseSchema = config$databaseSchema,
    llmConfig = config$llm,
    systemPrompt = systemPrompt,
    userName = getUserName(session),
    phenotypes = phenotypes,
    refreshPhenotypes = refreshPhenotypes
  )
}
