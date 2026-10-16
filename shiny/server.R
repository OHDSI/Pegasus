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
  phenotypes <- reactiveVal(emptyPhenotypeSummary())
  evaluations <- reactiveVal(emptyEvaluations())
  selectedPhenotypeId <- reactiveVal(NULL)
  shortlist <- reactiveVal(integer(0))
  cloneSource <- reactiveVal(NULL)
  returnToEvaluation <- reactiveVal(FALSE)
  evaluationPreselect <- reactiveVal(NULL)

  output$shortlistCount <- renderUI({
    if (length(shortlist()) > 0) tags$span(class = "badge text-bg-primary", length(shortlist()))
  })

  # Reloads everything the Browse and Performance tabs show.
  refreshPhenotypes <- function() {
    tryCatch(
      withConnection(connectionPool, function(connection) {
        phenotypes(getPhenotypeSummary(connection, config$databaseSchema))
        evaluations(getEvaluations(connection, config$databaseSchema))
      }),
      error = function(e) {
        showNotification(paste("Unable to load phenotypes:", conditionMessage(e)), type = "error")
      }
    )
  }
  refreshPhenotypes()

  openPhenotype <- function(phenotypeId) {
    selectedPhenotypeId(phenotypeId)
    bslib::nav_select("mainNav", "Browse", session = session)
  }

  # Opens the New phenotype wizard seeded with an existing phenotype's definition. The nonce makes
  # refining the same phenotype twice restart the wizard.
  refinePhenotype <- function(phenotypeId) {
    data <- phenotypes()
    row <- data[data$phenotypeId == phenotypeId, , drop = FALSE]
    if (nrow(row) != 1) {
      return(invisible(NULL))
    }
    cloneSource(list(
      id = row$phenotypeId,
      name = row$phenotypeName,
      definition = row$clinicalDefinition,
      nonce = as.numeric(Sys.time())
    ))
    bslib::nav_select("mainNav", "New phenotype", session = session)
  }

  # The Evaluation tab can send the user to create a phenotype and brings them back with it selected.
  newPhenotypeForEvaluation <- function() {
    returnToEvaluation(TRUE)
    cloneSource(NULL)
    bslib::nav_select("mainNav", "New phenotype", session = session)
  }

  phenotypeSaved <- function(phenotypeId) {
    if (returnToEvaluation()) {
      returnToEvaluation(FALSE)
      evaluationPreselect(list(id = phenotypeId, nonce = as.numeric(Sys.time())))
      bslib::nav_select("mainNav", "Evaluation", session = session)
    }
  }

  # Leaving for any other tab abandons the round trip, so a later unrelated save does not jump back.
  observeEvent(input$mainNav, {
    if (!input$mainNav %in% c("Evaluation", "New phenotype")) {
      returnToEvaluation(FALSE)
    }
  })

  browseServer(
    "browse",
    connectionPool = connectionPool,
    databaseSchema = config$databaseSchema,
    phenotypes = phenotypes,
    refreshPhenotypes = refreshPhenotypes,
    selectedId = selectedPhenotypeId,
    shortlist = shortlist,
    onRefine = refinePhenotype
  )
  performanceServer("performance", evaluations, shortlist = shortlist, onOpenPhenotype = openPhenotype)
  evaluationServer(
    "evaluation",
    connectionPool = connectionPool,
    databaseSchema = config$databaseSchema,
    userName = getUserName(session),
    phenotypes = phenotypes,
    refreshPhenotypes = refreshPhenotypes,
    preselect = evaluationPreselect,
    onNewPhenotype = newPhenotypeForEvaluation
  )
  conceptsServer(
    "concepts",
    connectionPool = connectionPool,
    databaseSchema = config$databaseSchema,
    vocabularyRunner = vocabularyRunner,
    phenotypes = phenotypes,
    shortlist = shortlist,
    onOpenPhenotype = openPhenotype
  )
  shortlistServer(
    "shortlist",
    connectionPool = connectionPool,
    databaseSchema = config$databaseSchema,
    phenotypes = phenotypes,
    evaluations = evaluations,
    shortlist = shortlist,
    onOpenPhenotype = openPhenotype
  )
  createServer(
    "create",
    connectionPool = connectionPool,
    databaseSchema = config$databaseSchema,
    llmConfig = config$llm,
    systemPrompt = systemPrompt,
    userName = getUserName(session),
    phenotypes = phenotypes,
    refreshPhenotypes = refreshPhenotypes,
    cloneSource = cloneSource,
    onSaved = phenotypeSaved
  )
}
