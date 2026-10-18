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

# Cohort definition landing page: the definition in readable form, its phenotype, its concept sets and its
# own performance measures.

# CirceR (which needs Java) writes the same readable description that Atlas shows, as Markdown.
cohortLogicMarkdown <- function(json) {
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    stop("The CirceR package is not installed.")
  }
  CirceR::cohortPrintFriendly(CirceR::cohortExpressionFromJson(json))
}

# Raw HTML inside the Markdown is dropped (commonmark's default), so definition text cannot inject markup.
markdownToHtml <- function(markdown) {
  htmltools::HTML(commonmark::markdown_html(markdown, extensions = TRUE))
}

validityBadge <- function(valid) {
  if (isTRUE(valid == 1)) {
    '<span class="badge text-bg-success">Valid</span>'
  } else {
    '<span class="badge text-bg-warning">Draft (awaiting validation)</span>'
  }
}

cohortUi <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("back"), "All phenotypes", icon = icon("arrow-left"), class = "btn-outline-primary mb-3"),
    uiOutput(ns("header")),
    bslib::navset_card_tab(
      id = ns("tabs"),
      height = "80vh",
      bslib::nav_panel("Logic", uiOutput(ns("logic"))),
      bslib::nav_panel("Concept sets", DT::DTOutput(ns("conceptSets"))),
      bslib::nav_panel(
        "Performance",
        plotly::plotlyOutput(ns("scatter"), height = "380px"),
        DT::DTOutput(ns("evaluations"))
      ),
      bslib::nav_panel("Circe JSON", uiOutput(ns("json")))
    )
  )
}

cohortServer <- function(id, connectionPool, databaseSchema, cohortDefinitionId, navigate) {
  moduleServer(id, function(input, output, session) {
    # Every newly opened cohort definition starts on its logic.
    observeEvent(cohortDefinitionId(), bslib::nav_select("tabs", "Logic", session = session))

    details <- reactive({
      req(cohortDefinitionId())
      tryCatch(
        withConnection(connectionPool, function(connection) {
          list(
            cohort = getCohortDefinition(connection, databaseSchema, cohortDefinitionId()),
            conceptSets = getCohortConceptSets(connection, databaseSchema, cohortDefinitionId()),
            evaluations = getEvaluations(connection, databaseSchema, cohortDefinitionId = cohortDefinitionId()),
            atlasBaseUrl = getAtlasBaseUrl(connection, databaseSchema)
          )
        }),
        error = function(e) {
          showNotification(paste("Unable to load the cohort definition:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    cohort <- reactive({
      detail <- details()
      req(detail, nrow(detail$cohort) == 1)
      detail$cohort
    })

    observeEvent(input$back, navigate())

    output$header <- renderUI({
      req(cohortDefinitionId())
      detail <- details()
      req(detail)
      if (nrow(detail$cohort) != 1) {
        return(tags$div(class = "alert alert-warning", sprintf("Cohort definition %d was not found.", cohortDefinitionId())))
      }
      row <- detail$cohort
      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-start",
          tags$h3(row$cohortDefinitionName, HTML(" "), HTML(validityBadge(row$valid))),
          tags$div(
            class = "d-flex gap-2",
            copyLinkButton(list(page = "cohort_definition", id = row$cohortDefinitionId)),
            downloadButton(session$ns("downloadJson"), "Download JSON", class = "btn-outline-primary btn-sm")
          )
        ),
        tags$p(
          class = "small",
          "Phenotype: ", HTML(entityLink("phenotype", row$phenotypeId, row$phenotypeName)),
          sprintf(" | ID %d | Source: %s", row$cohortDefinitionId, row$paSourceType),
          if (!is.na(row$atlasCohortDefinitionId)) {
            HTML(paste0(" | ", atlasLink(detail$atlasBaseUrl, "cohort", row$atlasCohortDefinitionId)))
          }
        ),
        tags$details(
          class = "mb-3",
          tags$summary("Clinical definition of the phenotype"),
          tags$p(class = "mt-2", style = "white-space: pre-wrap;", row$clinicalDefinition)
        )
      )
    })

    output$downloadJson <- downloadHandler(
      filename = function() paste0(safeFileName(cohort()$cohortDefinitionName), ".json"),
      content = function(file) writeLines(prettyJson(cohort()$circeCohortExpression), file, useBytes = TRUE)
    )

    logic <- reactive({
      json <- cohort()$circeCohortExpression
      tryCatch(
        withProgress(message = "Describing the cohort definition", value = 0.5, {
          markdownToHtml(cohortLogicMarkdown(json))
        }),
        error = function(e) {
          logEvent("cohort logic could not be rendered: ", conditionMessage(e))
          tagList(
            tags$div(
              class = "alert alert-warning",
              "The readable description is not available (", conditionMessage(e), "). The Circe JSON is shown instead."
            ),
            tags$pre(style = "white-space: pre-wrap;", prettyJson(json))
          )
        }
      )
    })

    output$logic <- renderUI(logic())

    output$json <- renderUI({
      tags$pre(style = "white-space: pre-wrap;", prettyJson(cohort()$circeCohortExpression))
    })

    output$conceptSets <- DT::renderDT({
      detail <- details()
      req(detail)
      conceptSets <- detail$conceptSets
      filterableTable(
        data.frame(
          Name = entityLink("concept_set", conceptSets$conceptSetId, conceptSets$conceptSetName),
          Source = htmltools::htmlEscape(conceptSets$csSourceType),
          Status = draftOrValid(conceptSets$valid),
          Descendants = ifelse(conceptSets$hasDescendants == 1, "Yes", "No"),
          Atlas = atlasLink(detail$atlasBaseUrl, "conceptSet", conceptSets$atlasConceptSetId),
          check.names = FALSE
        ),
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        options = list(pageLength = 25, language = list(emptyTable = "No concept sets are linked to this cohort definition."))
      )
    })

    output$scatter <- plotly::renderPlotly({
      detail <- details()
      req(detail)
      buildScatter(
        detail$evaluations,
        colorBy = "database",
        showCi = TRUE,
        source = "cohortScatter",
        clickable = FALSE,
        emptyMessage = "This cohort definition has not been evaluated."
      )
    })

    output$evaluations <- DT::renderDT({
      detail <- details()
      req(detail)
      filterableTable(
        evaluationTable(detail$evaluations, linkCohorts = FALSE),
        rownames = FALSE,
        selection = "none",
        escape = FALSE,
        options = list(pageLength = 25, language = list(emptyTable = "This cohort definition has not been evaluated."))
      )
    })
  })
}
