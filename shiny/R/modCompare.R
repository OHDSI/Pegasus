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

# Side-by-side comparison of two to four phenotypes: what differs in their definitions and concept sets, and how
# their performance differs for one database and evaluation type. The logic is in compareLogic.R.

compareStyle <- "
ins.diff-add { background: rgba(40, 167, 69, 0.25); text-decoration: none; }
del.diff-del { background: rgba(220, 53, 69, 0.25); }
.compare-definition { white-space: pre-wrap; line-height: 1.5; }
.cohort-diff { font-family: var(--bs-font-monospace); font-size: 0.8rem; border: 1px solid var(--bs-border-color); max-height: 70vh; overflow: auto; }
.cohort-diff div { padding: 0 0.4rem; white-space: pre-wrap; }
.cohort-diff .line-add { background: rgba(40, 167, 69, 0.25); }
.cohort-diff .line-del { background: rgba(220, 53, 69, 0.25); }
.cohort-diff .line-skip { color: var(--bs-secondary-color); background: var(--bs-tertiary-bg); font-style: italic; }"

compareUi <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(compareStyle)),
    bslib::card(
      fill = FALSE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Compare phenotypes",
        downloadButton(ns("download"), "Download comparison (HTML)", class = "btn-outline-primary btn-sm")
      ),
      bslib::layout_columns(
        col_widths = c(5, 3, 2, 2),
        fill = FALSE,
        selectizeInput(
          ns("phenotypes"), sprintf("Phenotypes (2 to %d)", maxComparedPhenotypes), choices = NULL, multiple = TRUE,
          width = "100%", options = list(maxItems = maxComparedPhenotypes, placeholder = "Choose phenotypes to compare")
        ),
        selectInput(ns("baseline"), "Baseline (others are compared with it)", choices = NULL, width = "100%"),
        selectInput(ns("database"), "Database", choices = NULL, width = "100%"),
        selectInput(ns("evaluationType"), "Evaluation type", choices = NULL, width = "100%")
      )
    ),
    uiOutput(ns("body"))
  )
}

# preselect is NULL or list(ids, nonce); the nonce makes choosing the same phenotypes again take effect.
compareServer <- function(id, connectionPool, databaseSchema, phenotypes, evaluations, preselect, onOpenPhenotype) {
  moduleServer(id, function(input, output, session) {
    selectedIds <- reactive(as.integer(input$phenotypes))

    observeEvent(phenotypes(), {
      data <- phenotypes()
      choices <- setNames(data$phenotypeId, data$phenotypeName)
      updateSelectizeInput(session, "phenotypes", choices = choices[order(names(choices))], selected = isolate(input$phenotypes), server = FALSE)
    })

    observeEvent(preselect(), {
      ids <- head(unique(as.integer(preselect()$ids)), maxComparedPhenotypes)
      updateSelectizeInput(session, "phenotypes", selected = ids)
    })

    # The baseline, database and evaluation type can only be chosen among what the selected phenotypes have.
    observeEvent(selectedIds(), {
      ids <- selectedIds()
      data <- phenotypes()
      labels <- data$phenotypeName[match(ids, data$phenotypeId)]
      current <- suppressWarnings(as.integer(isolate(input$baseline)))
      updateSelectInput(
        session, "baseline", choices = setNames(ids, labels),
        selected = if (length(current) == 1 && current %in% ids) current else head(ids, 1)
      )

      found <- evaluations()[evaluations()$phenotypeId %in% ids, , drop = FALSE]
      databases <- unique(found$cdmSourceAbbreviation[order(found$databaseId)])
      types <- orderedLevels(found$evaluationType, evaluationTypeOrder)
      default <- defaultMetricChoice(found)
      keep <- function(value, choices, fallback) if (!is.null(value) && value %in% choices) value else fallback
      updateSelectInput(session, "database", choices = databases, selected = keep(isolate(input$database), databases, default$database))
      updateSelectInput(session, "evaluationType", choices = types, selected = keep(isolate(input$evaluationType), types, default$evaluationType))
    }, ignoreNULL = FALSE)

    # Every cohort definition of the chosen phenotypes; one of each is compared.
    cohortExpressions <- reactive({
      ids <- selectedIds()
      req(length(ids) >= 2)
      tryCatch(
        withConnection(connectionPool, function(connection) getCohortExpressions(connection, databaseSchema, ids)),
        error = function(e) {
          showNotification(paste("Unable to load the cohort definitions:", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    cohortInputId <- function(phenotypeId) paste0("cohort_", phenotypeId)

    # The cohort definition chosen for each phenotype, as phenotype ID -> cohort definition ID. A phenotype
    # starts on its first cohort definition.
    cohortChoices <- reactive({
      expressions <- cohortExpressions()
      choices <- list()
      for (phenotypeId in selectedIds()) {
        available <- expressions$cohortDefinitionId[expressions$phenotypeId == phenotypeId]
        if (length(available) == 0) {
          next
        }
        chosen <- suppressWarnings(as.integer(input[[cohortInputId(phenotypeId)]]))
        choices[[as.character(phenotypeId)]] <- if (length(chosen) == 1 && chosen %in% available) chosen else available[1]
      }
      choices
    })

    output$cohortChoosers <- renderUI({
      expressions <- cohortExpressions()
      data <- phenotypes()
      selectors <- lapply(selectedIds(), function(phenotypeId) {
        rows <- expressions[expressions$phenotypeId == phenotypeId, , drop = FALSE]
        label <- data$phenotypeName[match(phenotypeId, data$phenotypeId)]
        if (nrow(rows) == 0) {
          return(tags$div(class = "text-muted small", paste0(label, ": no cohort definition")))
        }
        current <- suppressWarnings(as.integer(isolate(input[[cohortInputId(phenotypeId)]])))
        selectInput(
          session$ns(cohortInputId(phenotypeId)), paste("Cohort definition:", label),
          choices = setNames(rows$cohortDefinitionId, rows$cohortDefinitionName),
          selected = if (length(current) == 1 && current %in% rows$cohortDefinitionId) current else rows$cohortDefinitionId[1],
          width = "100%"
        )
      })
      do.call(bslib::layout_columns, c(list(col_widths = rep(floor(12 / max(1, length(selectors))), length(selectors)), fill = FALSE), selectors))
    })

    comparison <- reactive({
      ids <- selectedIds()
      baselineId <- suppressWarnings(as.integer(input$baseline))
      req(length(ids) >= 2, length(baselineId) == 1, !is.na(baselineId), baselineId %in% ids)
      expressions <- tryCatch(
        withConnection(connectionPool, function(connection) {
          getPhenotypeConceptSetExpressions(connection, databaseSchema, ids)
        }),
        error = function(e) {
          showNotification(paste("Unable to load the concept sets:", conditionMessage(e)), type = "error")
          NULL
        }
      )
      req(expressions)
      assembleComparison(
        phenotypes(), evaluations(), expressions, ids, baselineId,
        database = input$database, evaluationType = input$evaluationType,
        cohortTexts = cohortTextsFor(cohortExpressions(), cohortChoices(), cohortLogicMarkdown)
      )
    })

    # The page itself only depends on whether enough phenotypes are chosen; each part fills itself in. Choosing a
    # different cohort definition therefore redraws only the cohort part, and keeps the choices made.
    output$body <- renderUI({
      if (length(selectedIds()) < 2) {
        return(tags$div(
          class = "alert alert-info",
          "Choose at least two phenotypes above. You can also start from the Shortlist or Performance tab."
        ))
      }
      tagList(
        bslib::card(
          fill = FALSE,
          bslib::card_header("Summary of differences from the baseline"),
          uiOutput(session$ns("notes")),
          tags$p(
            class = "small text-muted mb-0",
            "Worked out from the stored definitions, concept sets and evaluations; nothing here is written by a language model."
          )
        ),
        bslib::card(
          fill = FALSE,
          bslib::card_header("Performance"),
          DT::DTOutput(session$ns("metrics")),
          tags$p(
            class = "small text-muted mb-0",
            "A phenotype with several cohort definitions has a row with their median above the individual rows.",
            "Differences are percentage points against the baseline's median (or its only cohort definition).",
            "Specificity and NPV come from the stored confusion matrix counts."
          )
        ),
        bslib::card(
          fill = FALSE,
          bslib::card_header("Definitions"),
          tags$p(
            class = "small text-muted",
            "Compared with the baseline: ", HTML('<ins class="diff-add">added words</ins>'), " and ",
            HTML('<del class="diff-del">removed words</del>'), "."
          ),
          uiOutput(session$ns("definitions"))
        ),
        bslib::card(
          fill = FALSE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            "Cohort definitions",
            checkboxInput(session$ns("changesOnly"), "Show only changes", value = TRUE, width = "auto")
          ),
          tags$p(
            class = "small text-muted",
            "Choose which cohort definition of each phenotype to compare. Each one is compared line by line with the",
            "baseline's, using the readable description that the cohort page shows."
          ),
          uiOutput(session$ns("cohortChoosers")),
          uiOutput(session$ns("cohortDiffs"))
        ),
        bslib::card(
          fill = FALSE,
          bslib::card_header("Concept sets"),
          DT::DTOutput(session$ns("conceptSets"))
        )
      )
    })

    output$notes <- renderUI(tags$ul(lapply(comparison()$notes, tags$li)))

    output$definitions <- renderUI({
      comparison <- comparison()
      count <- nrow(comparison$phenotypes)
      do.call(bslib::layout_columns, c(
        list(col_widths = rep(floor(12 / count), count), fill = FALSE),
        lapply(comparison$phenotypes$phenotypeId, function(phenotypeId) {
          name <- comparison$phenotypes$phenotypeName[comparison$phenotypes$phenotypeId == phenotypeId]
          bslib::card(
            fill = FALSE,
            bslib::card_header(
              HTML(entityLink("phenotype", phenotypeId, name)),
              if (phenotypeId == comparison$baselineId) tags$span(class = "badge text-bg-secondary ms-2", "Baseline")
            ),
            tags$div(class = "compare-definition", HTML(comparison$definitionHtml[[as.character(phenotypeId)]]))
          )
        })
      ))
    })

    output$cohortDiffs <- renderUI({
      diffs <- comparison()$cohortDiffs
      phenotypes <- comparison()$phenotypes
      contextLines <- if (isTRUE(input$changesOnly)) 2 else NULL
      if (length(diffs) == 0) {
        return(NULL)
      }
      tagList(lapply(names(diffs), function(id) {
        entry <- diffs[[id]]
        name <- phenotypes$phenotypeName[phenotypes$phenotypeId == as.integer(id)]
        if (!isTRUE(entry$available)) {
          return(tags$div(class = "alert alert-warning", paste0(name, ": a cohort definition is missing, so nothing can be compared.")))
        }
        tagList(
          tags$h6(class = "mt-3", sprintf("%s: %s compared with %s", name, entry$otherName, entry$baselineName)),
          if (!isTRUE(entry$readable)) {
            tags$p(class = "small text-warning", "The readable description is not available, so the Circe JSON is compared.")
          },
          tags$div(class = "cohort-diff", HTML(lineDiffHtml(entry$diff, contextLines)))
        )
      }))
    })

    output$metrics <- DT::renderDT({
      metrics <- comparison()$metrics
      filterableTable(
        data.frame(
          Phenotype = entityLink("phenotype", metrics$phenotypeId, metrics$phenotypeName),
          "Cohort definition" = {
            label <- htmltools::htmlEscape(ifelse(is.na(metrics$cohortDefinitionName), "No evaluation", metrics$cohortDefinitionName))
            ifelse(grepl("^Median of", label), paste0("<strong>", label, "</strong>"), label)
          },
          "PPV (%)" = percentText(metrics$ppv),
          "PPV vs baseline" = deltaText(metrics$ppvDelta),
          "Sensitivity (%)" = percentText(metrics$sensitivity),
          "Sensitivity vs baseline" = deltaText(metrics$sensitivityDelta),
          "Specificity (%)" = percentText(metrics$specificity),
          "NPV (%)" = percentText(metrics$npv),
          check.names = FALSE
        ),
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(dom = "t", paging = FALSE)
      )
    })

    output$conceptSets <- DT::renderDT({
      changes <- comparison()$conceptChanges
      filterableTable(
        data.frame(
          Phenotype = htmltools::htmlEscape(changes$phenotypeName),
          Change = changes$change,
          Pertains = htmltools::htmlEscape(changes$pertains),
          Category = htmltools::htmlEscape(changes$category),
          "Baseline concept set" = htmltools::htmlEscape(changes$baselineName),
          "Concept set here" = htmltools::htmlEscape(changes$otherName),
          Details = htmltools::htmlEscape(changes$details),
          check.names = FALSE
        ),
        escape = FALSE,
        rownames = FALSE,
        selection = "none",
        options = list(pageLength = 25, language = list(emptyTable = "No concept set differences."))
      )
    })

    output$download <- downloadHandler(
      filename = function() sprintf("pegasus_comparison_%s.html", format(Sys.Date(), "%Y%m%d")),
      content = function(file) writeLines(comparisonReport(comparison()), file, useBytes = TRUE)
    )
  })
}
