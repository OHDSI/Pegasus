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

# The base URL is the Atlas web UI (e.g. https://host/atlas), as stored in atlas_instance.
atlasLink <- function(baseUrl, type, id) {
  link <- rep("", length(id))
  if (is.null(baseUrl)) {
    return(link)
  }
  hasId <- !is.na(id)
  path <- switch(type, cohort = "cohortdefinition/%d", conceptSet = "conceptset/%d/details")
  link[hasId] <- sprintf(
    '<a href="%s/#/%s" target="_blank" rel="noopener">Atlas</a>',
    htmltools::htmlEscape(sub("/+$", "", baseUrl), attribute = TRUE),
    sprintf(path, as.integer(id[hasId]))
  )
  link
}

formatPercentWithInterval <- function(estimate, lower, upper) {
  sprintf("%.1f (%.1f-%.1f)", 100 * estimate, 100 * lower, 100 * upper)
}

# Adds one column per database with the overall person count of each concept set.
addPersonCountColumns <- function(conceptSets, personCounts) {
  for (abbreviation in unique(personCounts$cdmSourceAbbreviation)) {
    counts <- personCounts[personCounts$cdmSourceAbbreviation == abbreviation, , drop = FALSE]
    conceptSets[[abbreviation]] <- counts$overallPersons[match(conceptSets$conceptSetId, counts$conceptSetId)]
  }
  conceptSets
}

# Indented JSON for display; text that is not valid JSON is shown as it is.
prettyJson <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(text)) {
    return("Not available")
  }
  tryCatch(as.character(jsonlite::prettify(text)), error = function(e) text)
}

expressionModal <- function(conceptSet) {
  modalDialog(
    title = conceptSet$conceptSetName,
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    tags$h6("Capr expression"),
    tags$pre(style = "white-space: pre-wrap;", if (is.na(conceptSet$caprConceptSetExpression)) "Not available" else conceptSet$caprConceptSetExpression),
    tags$h6("Circe expression"),
    tags$pre(style = "white-space: pre-wrap; max-height: 40vh; overflow: auto;", prettyJson(conceptSet$circeConceptSetExpression))
  )
}

linkList <- function(ns, ids, names) {
  if (length(ids) == 0) {
    return("none")
  }
  links <- mapply(function(id, name) {
    as.character(tags$a(
      href = "#",
      onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;", ns("goto"), as.integer(id)),
      name
    ))
  }, ids, names)
  HTML(paste(links, collapse = ", "))
}

phenotypeDetailUi <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("back"), "All phenotypes", icon = icon("arrow-left"), class = "btn-outline-primary mb-3"),
    uiOutput(ns("header")),
    bslib::navset_card_tab(
      id = ns("tabs"),
      bslib::nav_panel("Definition", uiOutput(ns("definition"))),
      bslib::nav_panel("Concept sets", DT::DTOutput(ns("conceptSets"))),
      bslib::nav_panel("Person counts", DT::DTOutput(ns("personCounts"))),
      bslib::nav_panel("Draft targets", DT::DTOutput(ns("draftTargets"))),
      bslib::nav_panel("Cohorts", DT::DTOutput(ns("cohorts"))),
      bslib::nav_panel(
        "Performance",
        plotly::plotlyOutput(ns("scatter"), height = "380px"),
        DT::DTOutput(ns("evaluations"))
      )
    )
  )
}

phenotypeDetailServer <- function(id, connectionPool, databaseSchema, phenotypes, selectedId, shortlist, onBack, onRefine) {
  # Evaluate eagerly so a missing argument fails at startup instead of on the first click.
  force(onRefine)
  moduleServer(id, function(input, output, session) {
    observeEvent(input$back, onBack())
    observeEvent(input$goto, selectedId(as.integer(input$goto)))
    observeEvent(input$toggleShortlist, toggleShortlist(shortlist, selectedId()))
    observeEvent(input$refine, onRefine(selectedId()))

    # Every newly opened phenotype starts on the definition, not on the previous phenotype's tab.
    observeEvent(selectedId(), bslib::nav_select("tabs", "Definition", session = session))

    phenotype <- reactive({
      req(selectedId())
      data <- phenotypes()
      data[data$phenotypeId == selectedId(), , drop = FALSE]
    })

    details <- reactive({
      req(selectedId())
      tryCatch(
        withConnection(connectionPool, function(connection) {
          getPhenotypeDetails(connection, databaseSchema, selectedId())
        }),
        error = function(e) {
          showNotification(paste("Unable to load phenotype details:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    output$header <- renderUI({
      row <- phenotype()
      req(nrow(row) == 1)
      detail <- details()
      onShortlist <- row$phenotypeId %in% shortlist()
      tagList(
        tags$div(
          class = "d-flex justify-content-between align-items-start",
          tags$h3(row$phenotypeName, HTML(" "), HTML(statusBadge(row$status))),
          tags$div(
            class = "d-flex gap-2",
            actionButton(session$ns("refine"), "Refine this", icon = icon("pen"), class = "btn-outline-primary"),
            actionButton(
              session$ns("toggleShortlist"),
              if (onShortlist) "Remove from shortlist" else "Add to shortlist",
              icon = icon(if (onShortlist) "minus" else "plus"),
              class = if (onShortlist) "btn-secondary" else "btn-primary"
            )
          )
        ),
        tags$p(
          class = "text-muted small",
          sprintf("ID %s | Created by %s on %s UTC", row$phenotypeId, row$creator, formatTimestamp(row$timeCreated))
        ),
        if (!is.null(detail)) {
          tags$p(
            class = "small",
            "Refined from: ", linkList(session$ns, detail$parents$phenotypeId, detail$parents$phenotypeName),
            " | Refined into: ", linkList(session$ns, detail$children$phenotypeId, detail$children$phenotypeName)
          )
        }
      )
    })

    output$definition <- renderUI({
      row <- phenotype()
      req(nrow(row) == 1)
      tags$p(style = "white-space: pre-wrap;", row$clinicalDefinition)
    })

    showTable <- function(data, emptyMessage, ...) {
      DT::datatable(
        data,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 25, language = list(emptyTable = emptyMessage)),
        ...
      )
    }

    output$conceptSets <- DT::renderDT({
      detail <- details()
      req(detail)
      conceptSets <- addPersonCountColumns(detail$conceptSets, detail$personCounts)
      databaseColumns <- setdiff(names(conceptSets), names(detail$conceptSets))
      table <- data.frame(
        Name = htmltools::htmlEscape(conceptSets$conceptSetName),
        Pertains = htmltools::htmlEscape(conceptSets$pertains),
        Category = htmltools::htmlEscape(conceptSets$category),
        Source = htmltools::htmlEscape(conceptSets$csSourceType),
        Status = ifelse(conceptSets$valid == 1, "Valid", "Draft (awaiting validation)"),
        Descendants = ifelse(conceptSets$hasDescendants == 1, "Yes", "No"),
        check.names = FALSE
      )
      # Persons with the concept set per database come before the long expression so they are not scrolled away.
      for (column in databaseColumns) {
        table[[paste(column, "persons")]] <- conceptSets[[column]]
      }
      table$Expression <- sprintf(
        "<a href='#' onclick=\"Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;\">View</a>",
        session$ns("showExpression"), as.integer(conceptSets$conceptSetId)
      )
      table$Atlas <- atlasLink(detail$atlasBaseUrl, "conceptSet", conceptSets$atlasConceptSetId)
      result <- showTable(table, "No concept sets for this phenotype.", escape = FALSE)
      if (length(databaseColumns) > 0) {
        result <- DT::formatRound(result, paste(databaseColumns, "persons"), digits = 0)
      }
      result
    })

    observeEvent(input$showExpression, {
      conceptSets <- details()$conceptSets
      conceptSet <- conceptSets[conceptSets$conceptSetId == as.integer(input$showExpression), , drop = FALSE]
      if (nrow(conceptSet) == 1) {
        showModal(expressionModal(conceptSet))
      }
    })

    output$personCounts <- DT::renderDT({
      detail <- details()
      req(detail)
      counts <- detail$personCounts
      names <- detail$conceptSets$conceptSetName[match(counts$conceptSetId, detail$conceptSets$conceptSetId)]
      result <- showTable(
        data.frame(
          "Concept set" = names,
          Database = counts$cdmSourceAbbreviation,
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
        "No person counts have been computed for this phenotype's concept sets."
      )
      DT::formatRound(result, c("Overall", "Condition", "Procedure", "Drug", "Device", "Measurement", "Observation", "Visit"), digits = 0)
    })

    output$draftTargets <- DT::renderDT({
      detail <- details()
      req(detail)
      targets <- detail$draftTargets
      showTable(
        data.frame(
          Pertains = targets$pertains,
          Category = targets$category,
          Term = targets$term,
          "Concept ID" = targets$conceptId,
          "Concept name" = targets$conceptName,
          Domain = targets$domainId,
          Vocabulary = targets$vocabularyId,
          "Match type" = targets$matchType,
          check.names = FALSE
        ),
        "No draft targets for this phenotype."
      )
    })

    output$cohorts <- DT::renderDT({
      detail <- details()
      req(detail)
      cohorts <- detail$cohorts
      showTable(
        data.frame(
          Name = htmltools::htmlEscape(cohorts$cohortDefinitionName),
          Source = htmltools::htmlEscape(cohorts$paSourceType),
          Status = ifelse(cohorts$valid == 1, "Valid", "Draft (awaiting validation)"),
          Atlas = atlasLink(detail$atlasBaseUrl, "cohort", cohorts$atlasCohortDefinitionId)
        ),
        "No cohort definitions for this phenotype.",
        escape = FALSE
      )
    })

    output$scatter <- plotly::renderPlotly({
      detail <- details()
      req(detail)
      buildScatter(
        detail$evaluations,
        colorBy = "source",
        showCi = TRUE,
        source = "phenotypeScatter",
        clickable = FALSE,
        emptyMessage = "This phenotype has not been evaluated."
      )
    })

    output$evaluations <- DT::renderDT({
      detail <- details()
      req(detail)
      evaluations <- detail$evaluations
      showTable(
        data.frame(
          Cohort = evaluations$cohortDefinitionName,
          Source = evaluations$paSourceType,
          Database = evaluations$cdmSourceAbbreviation,
          "Evaluation type" = evaluations$evaluationType,
          Subjects = evaluations$cohortSubjects,
          "PPV % (95% CI)" = formatPercentWithInterval(evaluations$ppv, evaluations$ppvLb, evaluations$ppvUb),
          "Sensitivity % (95% CI)" = formatPercentWithInterval(
            evaluations$sensitivity, evaluations$sensitivityLb, evaluations$sensitivityUb
          ),
          TP = evaluations$truePositives,
          FP = evaluations$falsePositives,
          FN = evaluations$falseNegatives,
          TN = evaluations$trueNegatives,
          check.names = FALSE
        ),
        "This phenotype has not been evaluated."
      )
    })
  })
}
