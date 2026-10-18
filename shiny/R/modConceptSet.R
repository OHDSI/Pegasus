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

# Concept set landing page: what the concept set contains and where it is used (cohort definitions and
# phenotypes).

emptyConceptSetItems <- function() {
  data.frame(
    conceptId = integer(0), conceptName = character(0), domainId = character(0), vocabularyId = character(0),
    conceptCode = character(0), standardConcept = character(0), isExcluded = integer(0),
    includeDescendants = integer(0), includeMapped = integer(0), stringsAsFactors = FALSE
  )
}

# The items of a Circe concept set expression ({"items": [{"concept": {...}, "isExcluded": ...}]}), one row per
# item. Text that is not a valid expression gives no rows.
parseConceptSetItems <- function(expression) {
  parsed <- tryCatch(jsonlite::fromJSON(expression, simplifyVector = FALSE), error = function(e) NULL)
  items <- parsed$items
  if (length(items) == 0) {
    return(emptyConceptSetItems())
  }
  field <- function(concept, name) {
    value <- concept[[name]]
    if (is.null(value)) NA_character_ else as.character(value)
  }
  rows <- lapply(items, function(item) {
    concept <- item$concept
    data.frame(
      conceptId = suppressWarnings(as.integer(field(concept, "CONCEPT_ID"))),
      conceptName = field(concept, "CONCEPT_NAME"),
      domainId = field(concept, "DOMAIN_ID"),
      vocabularyId = field(concept, "VOCABULARY_ID"),
      conceptCode = field(concept, "CONCEPT_CODE"),
      standardConcept = field(concept, "STANDARD_CONCEPT"),
      isExcluded = as.integer(isTRUE(item$isExcluded)),
      includeDescendants = as.integer(isTRUE(item$includeDescendants)),
      includeMapped = as.integer(isTRUE(item$includeMapped)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

conceptSetUi <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("back"), "All phenotypes", icon = icon("arrow-left"), class = "btn-outline-primary mb-3"),
    uiOutput(ns("header")),
    bslib::navset_card_tab(
      id = ns("tabs"),
      height = "80vh",
      bslib::nav_panel("Expression", DT::DTOutput(ns("items"))),
      bslib::nav_panel("Cohort definitions", DT::DTOutput(ns("cohorts"))),
      bslib::nav_panel("Phenotypes", DT::DTOutput(ns("phenotypes"))),
      bslib::nav_panel("Person counts", DT::DTOutput(ns("counts"))),
      bslib::nav_panel("Circe and Capr", uiOutput(ns("raw")))
    )
  )
}

conceptSetServer <- function(id, connectionPool, databaseSchema, conceptSetId, navigate) {
  moduleServer(id, function(input, output, session) {
    # Every newly opened concept set starts on its expression.
    observeEvent(conceptSetId(), bslib::nav_select("tabs", "Expression", session = session))
    observeEvent(input$back, navigate())

    details <- reactive({
      req(conceptSetId())
      tryCatch(
        withConnection(connectionPool, function(connection) {
          list(
            conceptSet = getConceptSet(connection, databaseSchema, conceptSetId()),
            cohorts = getConceptSetCohorts(connection, databaseSchema, conceptSetId()),
            phenotypes = getConceptSetPhenotypes(connection, databaseSchema, conceptSetId()),
            counts = getConceptSetCounts(connection, databaseSchema, conceptSetId()),
            atlasBaseUrl = getAtlasBaseUrl(connection, databaseSchema)
          )
        }),
        error = function(e) {
          showNotification(paste("Unable to load the concept set:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    conceptSet <- reactive({
      detail <- details()
      req(detail, nrow(detail$conceptSet) == 1)
      detail$conceptSet
    })

    output$header <- renderUI({
      req(conceptSetId())
      detail <- details()
      req(detail)
      if (nrow(detail$conceptSet) != 1) {
        return(tags$div(class = "alert alert-warning", sprintf("Concept set %d was not found.", conceptSetId())))
      }
      row <- detail$conceptSet
      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-start",
          tags$h3(row$conceptSetName, HTML(" "), HTML(validityBadge(row$valid))),
          tags$div(
            class = "d-flex gap-2",
            copyLinkButton(list(page = "concept_set", id = row$conceptSetId)),
            downloadButton(session$ns("downloadJson"), "Download JSON", class = "btn-outline-primary btn-sm")
          )
        ),
        tags$p(
          class = "small",
          sprintf(
            "ID %d | Source: %s | Descendants: %s",
            row$conceptSetId,
            if (is.na(row$csSourceType)) "unknown" else row$csSourceType,
            if (row$hasDescendants == 1) "yes" else "no"
          ),
          if (!is.na(row$atlasConceptSetId)) {
            HTML(paste0(" | ", atlasLink(detail$atlasBaseUrl, "conceptSet", row$atlasConceptSetId)))
          }
        ),
        if (!is.na(row$clinicalDefinition) && nzchar(row$clinicalDefinition)) {
          tags$div(
            class = "card card-body bg-body-tertiary mb-3",
            tags$h6("Description"),
            tags$p(class = "mb-0", style = "white-space: pre-wrap;", row$clinicalDefinition)
          )
        }
      )
    })

    output$downloadJson <- downloadHandler(
      filename = function() paste0(safeFileName(conceptSet()$conceptSetName), ".json"),
      content = function(file) writeLines(prettyJson(conceptSet()$circeConceptSetExpression), file, useBytes = TRUE)
    )

    showTable <- function(data, emptyMessage) {
      filterableTable(
        data,
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        options = list(pageLength = 25, language = list(emptyTable = emptyMessage))
      )
    }

    output$items <- DT::renderDT({
      items <- parseConceptSetItems(conceptSet()$circeConceptSetExpression)
      yesNo <- function(x) ifelse(x == 1, "Yes", "No")
      showTable(
        data.frame(
          "Concept ID" = items$conceptId,
          Name = htmltools::htmlEscape(items$conceptName),
          Domain = htmltools::htmlEscape(items$domainId),
          Vocabulary = htmltools::htmlEscape(items$vocabularyId),
          Code = htmltools::htmlEscape(items$conceptCode),
          Standard = htmltools::htmlEscape(items$standardConcept),
          Excluded = yesNo(items$isExcluded),
          Descendants = yesNo(items$includeDescendants),
          Mapped = yesNo(items$includeMapped),
          check.names = FALSE
        ),
        "This concept set has no concepts."
      )
    })

    output$cohorts <- DT::renderDT({
      detail <- details()
      req(detail)
      cohorts <- detail$cohorts
      showTable(
        data.frame(
          Cohort = entityLink("cohort_definition", cohorts$cohortDefinitionId, cohorts$cohortDefinitionName),
          Phenotype = entityLink("phenotype", cohorts$phenotypeId, cohorts$phenotypeName),
          Source = htmltools::htmlEscape(cohorts$paSourceType),
          Status = draftOrValid(cohorts$valid),
          check.names = FALSE
        ),
        "No cohort definition uses this concept set."
      )
    })

    output$phenotypes <- DT::renderDT({
      detail <- details()
      req(detail)
      phenotypes <- detail$phenotypes
      showTable(
        data.frame(
          Phenotype = entityLink("phenotype", phenotypes$phenotypeId, phenotypes$phenotypeName),
          Pertains = htmltools::htmlEscape(phenotypes$pertains),
          Category = htmltools::htmlEscape(phenotypes$category),
          "Referenced through" = phenotypes$via,
          check.names = FALSE
        ),
        "No phenotype references this concept set."
      )
    })

    output$counts <- DT::renderDT({
      detail <- details()
      req(detail)
      counts <- detail$counts
      result <- showTable(
        data.frame(
          Database = htmltools::htmlEscape(counts$cdmSourceAbbreviation),
          Overall = counts$overallPersons,
          Condition = counts$conditionPersons,
          Procedure = counts$procedurePersons,
          Drug = counts$drugPersons,
          Device = counts$devicePersons,
          Measurement = counts$measurementPersons,
          Observation = counts$observationPersons,
          Visit = counts$visitPersons,
          check.names = FALSE
        ),
        "No person counts are stored for this concept set."
      )
      DT::formatRound(result, c("Overall", "Condition", "Procedure", "Drug", "Device", "Measurement", "Observation", "Visit"), digits = 0)
    })

    output$raw <- renderUI({
      row <- conceptSet()
      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$h6(class = "mb-0", "Circe expression"),
          copyTextButton(session$ns("circeText"))
        ),
        tags$pre(
          id = session$ns("circeText"),
          style = "white-space: pre-wrap; max-height: 35vh; overflow: auto;",
          prettyJson(row$circeConceptSetExpression)
        ),
        tags$h6("Capr expression"),
        tags$pre(
          style = "white-space: pre-wrap;",
          if (is.na(row$caprConceptSetExpression)) "Not available" else row$caprConceptSetExpression
        )
      )
    })
  })
}
