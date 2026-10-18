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

# The concept sets of a cohort definition as table columns. `matches` says which concept set in the cohort's own
# JSON (by name) matched each stored set; NULL when that comparison could not be made. A concept set in the JSON
# can match several stored sets with identical content, and those rows point at each other.
cohortConceptSetTable <- function(conceptSets, matches, atlasBaseUrl) {
  table <- data.frame(
    Name = entityLink("concept_set", conceptSets$conceptSetId, conceptSets$conceptSetName),
    Source = htmltools::htmlEscape(conceptSets$csSourceType),
    Status = draftOrValid(conceptSets$valid),
    Descendants = ifelse(conceptSets$hasDescendants == 1, "Yes", "No"),
    check.names = FALSE
  )
  if (!is.null(matches)) {
    rows <- lapply(conceptSets$conceptSetId, function(id) {
      own <- matches[matches$conceptSetId == id, , drop = FALSE]
      others <- matches[matches$setIndex %in% own$setIndex & matches$conceptSetId != id, , drop = FALSE]
      others <- others[!duplicated(others$conceptSetId), , drop = FALSE]
      list(
        inCohort = if (nrow(own) == 0) "" else paste(htmltools::htmlEscape(unique(ifelse(is.na(own$name), "(unnamed)", own$name))), collapse = ", "),
        note = if (nrow(own) == 0) {
          "Not found in the cohort's JSON"
        } else if (nrow(others) > 0) {
          paste("Same content as", paste(entityLink("concept_set", others$conceptSetId, others$conceptSetName), collapse = ", "))
        } else {
          ""
        }
      )
    })
    table[["In the cohort JSON as"]] <- vapply(rows, function(row) row$inCohort, "")
    table$Note <- vapply(rows, function(row) row$note, "")
  }
  table$Atlas <- atlasLink(atlasBaseUrl, "conceptSet", conceptSets$atlasConceptSetId)
  table
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
      bslib::nav_panel("Concept sets", uiOutput(ns("conceptSetNote")), DT::DTOutput(ns("conceptSets"))),
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
          cohort <- getCohortDefinition(connection, databaseSchema, cohortDefinitionId())
          linked <- getCohortConceptSets(connection, databaseSchema, cohortDefinitionId())
          # Compare the concept sets inside the cohort's own JSON with the stored ones. This is only extra
          # information, so the page still works when the comparison fails.
          matched <- if (nrow(cohort) == 1) {
            tryCatch(
              matchCohortConceptSetsInDatabase(
                connection, databaseSchema, cohort[, c("cohortDefinitionId", "circeCohortExpression")]
              ),
              error = function(e) {
                logEvent("cohort concept sets could not be compared with the JSON: ", conditionMessage(e))
                NULL
              }
            )
          }
          # Without rows in cohort_definition_to_concept_set, the matches from the JSON are the concept sets.
          fromJson <- nrow(linked) == 0 && !is.null(matched) && nrow(matched$links) > 0
          conceptSets <- if (fromJson) getConceptSetSummaries(connection, databaseSchema, matched$links$conceptSetId) else linked
          list(
            cohort = cohort,
            conceptSets = conceptSets,
            conceptSetsFromJson = fromJson,
            conceptSetMatches = if (is.null(matched)) NULL else matched$matches,
            jsonSetsWithoutMatch = if (is.null(matched)) NULL else matched$report$unmatched,
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

    # The button sits above a scrolling block, so it stays in view however long the JSON is.
    output$json <- renderUI({
      tagList(
        tags$div(class = "mb-2", copyTextButton(session$ns("jsonText"))),
        tags$pre(
          id = session$ns("jsonText"),
          style = "white-space: pre-wrap; max-height: 60vh; overflow: auto;",
          prettyJson(cohort()$circeCohortExpression)
        )
      )
    })

    output$conceptSetNote <- renderUI({
      detail <- details()
      req(detail)
      unmatched <- detail$jsonSetsWithoutMatch
      tagList(
        if (isTRUE(detail$conceptSetsFromJson)) {
          tags$p(
            class = "text-muted small",
            "No concept sets are recorded for this cohort definition in cohort_definition_to_concept_set. These were found by ",
            "matching the concept sets inside its Circe JSON with the stored concept sets."
          )
        },
        if (!is.null(unmatched) && nrow(unmatched) > 0) {
          tags$div(
            class = "alert alert-warning small",
            sprintf(
              "%d concept set%s in this cohort's JSON ha%s no stored match: ",
              nrow(unmatched), if (nrow(unmatched) == 1) "" else "s", if (nrow(unmatched) == 1) "s" else "ve"
            ),
            paste(ifelse(is.na(unmatched$name), "(unnamed)", unmatched$name), collapse = ", ")
          )
        },
        if (!is.null(detail$conceptSetMatches) && any(duplicated(detail$conceptSetMatches[, c("cohortDefinitionId", "setIndex")]))) {
          tags$p(
            class = "text-muted small",
            "Some concept sets in this cohort's JSON have identical content in several stored concept sets; ",
            "they are listed together and marked in the Note column."
          )
        }
      )
    })

    output$conceptSets <- DT::renderDT({
      detail <- details()
      req(detail)
      filterableTable(
        cohortConceptSetTable(detail$conceptSets, detail$conceptSetMatches, detail$atlasBaseUrl),
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
